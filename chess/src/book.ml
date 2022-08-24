open Core_kernel

type entry = {
  move   : Move.t;
  weight : int;
  depth  : int;
  score  : int;
}

let entry_size = 16

type t = {
  filename : string;
  table    : (int64, entry list) Hashtbl.t;
}

let filename book = book.filename

(* Moves are stored compactly within a 16-bit integer:

   +--------+-----------------+----------+----------+----------+----------+
   | unused | promotion piece | src rank | src file | dst rank | dst file |
   +--------+-----------------+----------+----------+----------+----------+
   |   15   | 14           12 | 11     9 | 8      6 | 5      3 | 2      0 |
   +--------+-----------------+----------+----------+----------+----------+
*)
let decode_move i =
  let src =
    let rank = (i land 0b0_000_111_000_000_000) lsr 9 in
    let file = (i land 0b0_000_000_111_000_000) lsr 6 in
    Square.create_exn ~rank ~file in
  let dst =
    let rank = (i land 0b0_000_000_000_111_000) lsr 3 in
    let file = (i land 0b0_000_000_000_000_111) lsr 0 in
    Square.create_exn ~rank ~file in
  let promote =
    match (i land 0b0_111_000_000_000_000) lsr 12 with
    | 0b000 -> None
    | 0b001 -> Some Move.Promote.Knight
    | 0b010 -> Some Move.Promote.Bishop
    | 0b011 -> Some Move.Promote.Rook
    | 0b100 -> Some Move.Promote.Queen
    | n -> invalid_argf "Invalid promotion piece %d" n () in
  Move.create src dst ~promote

(* The Polyglot entry format can be seen as the following C struct:

   struct entry {
     uint64_t key;
     uint16_t move;
     uint16_t weight;
     uint16_t depth;
     uint16_t score;
   };

   - `key` is the Zobrist hash of the position that was stored.
   - `move` is the move chosen in response to this position.
   - `weight` is the measurement of how "good" the response was.
   - `depth` and `score` are unused.
*)
let[@inline] read_entry buf o =
  let key = Bigstring.get_int64_t_be buf ~pos:o in
  let move = decode_move @@ Bigstring.get_int16_be buf ~pos:(o + 8) in
  let weight = Bigstring.get_int16_be buf ~pos:(o + 10) in
  let depth = Bigstring.get_int16_be buf ~pos:(o + 12) in
  let score = Bigstring.get_int16_be buf ~pos:(o + 14) in
  key, {move; weight; depth; score}

let read file =
  let book = Hashtbl.create (module Int64) in
  let buf = Bigstring.of_string @@ In_channel.input_all file in
  let len = Bigstring.length buf in
  let o = ref 0 in
  while !o < len do
    let key, data = read_entry buf !o in
    Hashtbl.add_multi book ~key ~data;
    o := !o + entry_size
  done;
  book

let create filename =
  let table = In_channel.with_file filename ~binary:true ~f:read in
  let compare x y = compare y.weight x.weight in
  Hashtbl.map_inplace table ~f:(List.sort ~compare);
  {filename; table}

module Error = struct
  type t =
    | Position_not_found of Position.t
    | Illegal_move of Move.t * Position.t
    | No_moves of Position.t

  let pp ppf = function
    | Position_not_found pos ->
      Format.fprintf ppf "Failed to find position %a in the book%!"
        Position.pp pos
    | Illegal_move (m, pos) ->
      Format.fprintf ppf "Lookup found illegal move %a for position %a%!"
        Move.pp m Position.pp pos
    | No_moves pos ->
      Format.fprintf ppf "Failed to find best weighted move for positon %a%!"
        Position.pp pos

  let to_string t = Format.asprintf "%a%!" pp t
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

let lookup ?(skip_illegal = false) ?(random = true) book pos =
  match Hashtbl.find book.table @@ Position.hash pos with
  | None | Some [] -> Error (Error.Position_not_found pos)
  | Some entries ->
    let sum =
      List.fold entries ~init:0 ~f:(fun acc {weight; _} ->
          acc + weight) in
    let target = Random.int_incl 0 sum in
    let rec find sum = function
      | [] -> Error (Error.No_moves pos)
      | {move; weight; _} :: rest ->
        let sum = sum - weight in
        if not random || sum <= target then match make_move pos move with
          | None when skip_illegal -> find sum rest
          | None -> Error (Error.Illegal_move (move, pos))
          | Some legal -> Ok legal
        else find sum rest in
    find sum entries
