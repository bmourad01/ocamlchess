open Core_kernel
open Monads.Std

module Bb = Bitboard
module Cr = Castling_rights

type entry = {
  move : Move.t;
  weight : int;
}

let entry_size = 16

type t = (int64, entry list) Hashtbl.t

(* Moves are stored compactly within a 16-bit integer:

   ------------------------------------------------------------------------
   | unused | promotion piece | src rank | src file | dst rank | dst file |
   ------------------------------------------------------------------------
   | 16  15 | 14           12 | 11     9 | 8      6 | 5      3 | 2      0 |
   ------------------------------------------------------------------------
*)
let decode_move i =
  let src =
    let rank = (i land 0xE00) lsr 9 in
    let file = (i land 0x1C0) lsr 6 in
    Square.create_exn ~rank ~file in
  let dst =
    let rank = (i land 0x38) lsr 3 in
    let file = i land 0x7 in
    Square.create_exn ~rank ~file in
  let promote =
    match (i land 0x7000) lsr 12 with
    | 0 -> None
    | 1 -> Some Move.Promote.Knight
    | 2 -> Some Move.Promote.Bishop
    | 3 -> Some Move.Promote.Rook
    | 4 -> Some Move.Promote.Queen
    | n -> invalid_argf "Invalid promotion piece %d" n () in
  Move.create src dst ~promote

(* The Polyglot entry format can be seen as the following C struct:

   struct entry {
     uint64_t key;
     uint16_t move;
     uint16_t weight;
     uint32_t learn;
   };

   - `key` is the Zobrist hash of the position that was stored.
   - `move` is the move chosen in response to this position.
   - `weight` is the measurement of how "good" the response was.
   - `learn` is used to record learning information, and we will
      leave it unused.
*)
let create filepath =
  In_channel.with_file filepath ~binary:true ~f:(fun file ->
      let book = Hashtbl.create (module Int64) in
      let contents = Bigstring.of_string @@ In_channel.input_all file in
      let len = Bigstring.length contents in
      if len land 0b1111 <> 0 then
        invalid_arg "Invalid length of book file, must be divisible by 16"
      else
        let rec read i =
          if i < len then begin
            let key = Bigstring.get_int64_t_be contents ~pos:i in
            let move = decode_move @@
              Bigstring.get_uint16_be contents ~pos:(i + 8) in
            let weight = Bigstring.get_uint16_be contents ~pos:(i + 8 + 2) in
            (* let _learn = Bigstring.get_int32_t_be contents ~pos:(i + 8 + 2 + 2) in *)
            Hashtbl.add_multi book ~key ~data:{move; weight};
            read (i + entry_size)
          end else book in
        read 0)

module Error = struct
  type t =
    | Position_not_found of Position.t
    | Illegal_move of Move.t * Position.t
    | Bad_weight of Position.t

  let to_string = function
    | Position_not_found pos ->
      sprintf "Failed to find position %s in the book"
        (Position.Fen.to_string pos)
    | Illegal_move (m, pos) ->
      sprintf "Lookup found illegal move %s for position %s"
        (Move.to_string m) (Position.Fen.to_string pos)
    | Bad_weight pos ->
      sprintf "Failed to find best weighted move for positon %s"
        (Position.Fen.to_string pos)
end

type error = Error.t

let make_move pos move =
  let src, dst, promote = Move.decomp move in
  let move = match Piece.kind @@ Position.piece_at_square_exn pos src with
    | King -> begin
        (* Special case for castling moves. The Polyglot format uses
           the extremal files as the destination squares instead of 
           the actual squares that the king will move to. *)
        assert (Option.is_none promote);
        match Position.active pos with
        | White when Square.(src = e1 && dst = h1) ->
          Move.create src Square.g1
        | White when Square.(src = e1 && dst = a1) ->
          Move.create src Square.c1
        | Black when Square.(src = e8 && dst = h8) ->
          Move.create src Square.g8
        | Black when Square.(src = e8 && dst = a8) ->
          Move.create src Square.c8
        | _ -> move
      end
    | _ -> move in
  Position.make_move pos move

let lookup book pos =
  let open Error in
  match Hashtbl.find book @@ Position.hash pos with
  | None -> Error (Position_not_found pos)
  | Some entries ->
    let sum =
      List.fold entries ~init:0 ~f:(fun acc {weight; _} ->
          acc + weight) in
    let target = Random.int_incl 0 sum in
    let rec find sum = function
      | [] -> Error (Bad_weight pos)
      | {move; weight} :: rest ->
        let sum = sum - weight in
        if sum <= target then match make_move pos move with
          | exception _ -> Error (Illegal_move (move, pos))
          | legal -> Ok legal
        else find sum rest in
    find sum entries
