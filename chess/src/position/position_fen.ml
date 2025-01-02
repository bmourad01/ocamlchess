open Core_kernel [@@warning "-D"]
open Position_common
open Monads.Std

(* This is based on the string:

   pppppppp/pppppppp/pppppppp/pppppppp/pppppppp/pppppppp/pppppppp/pppppppp w KQkq e3 9223372036854775807 9223372036854775807

   where 9223372036854775807 is (2^63)-1, the largest possible
   unsigned value for OCaml integers.
*)
let max_len = 121

module Error = struct
  type t =
    | Invalid_number_of_ranks of int
    | Invalid_file_increment of int * Square.t
    | Rank_full of int
    | Invalid_piece_symbol of char * Square.t
    | Unspecified_squares of int * int
    | Invalid_active_color of string
    | Invalid_castling_rights of string
    | Invalid_en_passant of string
    | Invalid_halfmove of string
    | Invalid_fullmove of string
    | Invalid_position of Position_valid.error
    | Invalid_number_of_sections of int
    | String_too_long of int

  let pp ppf = function
    | Invalid_number_of_ranks n ->
      Format.fprintf ppf "Invalid number of ranks %d%!" n
    | Invalid_file_increment (n, sq) ->
      Format.fprintf ppf "Invalid file increment %d on square %a%!"
        n Square.pp sq
    | Rank_full rank ->
      Format.fprintf ppf "Piece placement on full rank %d%!" (rank + 1)
    | Invalid_piece_symbol (sym, sq) ->
      Format.fprintf ppf "Invalid piece symbol '%c' placed at square %a%!"
        sym Square.pp sq
    | Unspecified_squares (rank, n) ->
      Format.fprintf ppf "Rank %d has %d unspecified square(s)%!"
        (rank + 1) n
    | Invalid_active_color s ->
      Format.fprintf ppf "Invalid active color '%s'%!" s
    | Invalid_castling_rights s ->
      Format.fprintf ppf "Invalid castling rights '%s'%!" s
    | Invalid_en_passant s ->
      Format.fprintf ppf "Invalid en passant square '%s'%!" s
    | Invalid_halfmove s ->
      Format.fprintf ppf "Invalid halfmove clock '%s'%!" s
    | Invalid_fullmove s ->
      Format.fprintf ppf "Invalid fullmove clock '%s'%!" s
    | Invalid_position e ->
      Format.fprintf ppf "Invalid position; %a%!" Position_valid.Error.pp e
    | Invalid_number_of_sections n ->
      Format.fprintf ppf "Invalid number of sections %d%!" n
    | String_too_long n ->
      Format.fprintf ppf "String is too long (%d), maximum is %d" n max_len

  let to_string t = Format.asprintf "%a%!" pp t
end

type error = Error.t

module E = struct
  include Monad.Result.Make(Error)(Monad.Ident)

  module String = struct
    let fold =
      let rec loop s i acc ~f ~len =
        if i = len then return acc
        else
          f acc s.[i] >>= fun acc ->
          loop s (i + 1) acc ~f ~len in
      fun s ~init ~f -> loop s 0 init ~f ~len:(String.length s)
  end

  module List = struct
    include List

    let iteri =
      let rec loop i ~f = function
        | [] -> return ()
        | x :: xs ->
          f i x >>= fun () ->
          loop (i + 1) xs ~f in
      fun l ~f -> loop 0 l ~f
  end
end

open E.Syntax

let start = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

let pp_placement ppf pos =
  let rec aux rank file skip =
    if rank < 0 then ()
    else if file > 7 then begin
      if skip > 0 then Format.fprintf ppf "%d%!" skip;
      if rank > 0 then Format.fprintf ppf "/%!";
      aux (rank - 1) 0 0
    end else
      let p = piece_at_square_uopt pos @@ Square.create_exn ~rank ~file in
      if Uopt.is_none p then aux rank (file + 1) (skip + 1)
      else
        let p = Uopt.unsafe_value p in
        if skip > 0 then Format.fprintf ppf "%d%!" skip;
        Format.fprintf ppf "%c%!" @@ Piece.to_fen p;
        aux rank (file + 1) 0 in
  aux 7 0 0

let pp_active ppf = function
  | Piece.White -> Format.fprintf ppf "w%!"
  | Piece.Black -> Format.fprintf ppf "b%!"

let pp_castle ppf cr = Format.fprintf ppf "%a%!" Cr.pp cr

let pp_en_passant ppf = function
  | None -> Format.fprintf ppf "-%!"
  | Some ep -> Format.fprintf ppf "%a%!" Square.pp ep

let pp ppf pos =
  let sep () = Format.fprintf ppf " %!" in
  pp_placement ppf pos; sep ();
  pp_active ppf @@ active pos; sep ();
  pp_castle ppf @@ castle pos; sep ();
  pp_en_passant ppf @@ Uopt.to_option pos.en_passant; sep ();
  Format.fprintf ppf "%d %d%!" pos.halfmove pos.fullmove

let to_string pos = Format.asprintf "%a%!" pp pos

let parse_placement s =
  (* Split the ranks so we can parse them individually. *)
  begin match String.split s ~on:'/' with
    | [_; _; _; _; _; _; _; _] as ranks -> E.return @@ List.rev ranks
    | ranks -> E.fail @@ Invalid_number_of_ranks (List.length ranks)
  end >>= fun ranks ->
  (* Tables for our bitboards. *)
  let color_tbl = Array.create Bb.empty ~len:Piece.Color.count in
  let kind_tbl = Array.create Bb.empty ~len:Piece.Kind.count in
  (* The main entry to parsing the rank. *)
  let rec parse_rank rank file sym = match Char.get_digit sym with
    | Some inc -> skip_file rank file inc
    | None -> place_piece rank file sym
  (* Advance the file (we hit a numeric symbol). *)
  and skip_file rank file inc =
    let file' = file + inc in
    if file' > Square.File.count then E.fail @@
      Invalid_file_increment (inc, Square.create_exn ~rank ~file)
    else E.return file'
  (* Place a piece on the board (we hit an alphabetical symbol). *)
  and place_piece rank file sym =
    if file > Square.File.h then E.fail @@ Rank_full rank
    else
      let sq = Square.create_unsafe ~rank ~file in
      match Piece.of_fen sym with
      | Some p ->
        let c = Piece.(color p |> Color.to_int) in
        let k = Piece.(kind p |> Kind.to_int) in
        color_tbl.(c) <- Bb.(color_tbl.(c) ++ sq);
        kind_tbl.(k) <- Bb.(kind_tbl.(k) ++ sq);
        E.return (file + 1)
      | None -> E.fail @@ Invalid_piece_symbol (sym, sq) in
  (* Parse each rank individually. *)
  E.List.iteri ranks ~f:(fun rank s ->
      let init = Square.File.a and f = parse_rank rank in
      E.String.fold s ~init ~f >>= fun file ->
      let diff = Square.File.count - file in
      (* All eight squares of the rank must be specified. *)
      if diff <> 0 then E.fail @@ Unspecified_squares (rank, diff)
      else E.return ()) >>| fun () ->
  (* Return the bitboard tables. *)
  color_tbl, kind_tbl

let parse_active = function
  | "w" -> E.return Piece.White
  | "b" -> E.return Piece.Black
  | s -> E.fail @@ Invalid_active_color s

let parse_castle s = try E.return @@ Cr.of_string_exn s with
  | _ -> E.fail @@ Invalid_castling_rights s

let parse_en_passant = function
  | "-" -> E.return Uopt.none
  | s -> try E.return @@ Uopt.some @@ Square.of_string_exn s with
    | _ -> E.fail @@ Invalid_en_passant s

let parse_halfmove s = try
    E.return @@ Int.of_string s
  with _ -> E.fail @@ Invalid_halfmove s

let parse_fullmove s = try
    E.return @@ Int.of_string s
  with _ -> E.fail @@ Invalid_fullmove s

let validate_and_map pos = Error.(
    Position_valid.check pos |>
    Result.map_error ~f:(fun e -> Invalid_position e)) >>= fun () ->
  E.return pos

let of_string ?(validate = true) s =
  let len = String.length s in
  if len <= max_len then match String.split s ~on:' ' with
    | [placement; active; castle; en_passant; halfmove; fullmove] ->
      parse_placement placement >>= fun (color_tbl, kind_tbl) ->
      let white  = color_tbl.(Piece.Color.white) in
      let black  = color_tbl.(Piece.Color.black) in
      let pawn   =  kind_tbl.(Piece.Kind.pawn)   in
      let knight =  kind_tbl.(Piece.Kind.knight) in
      let bishop =  kind_tbl.(Piece.Kind.bishop) in
      let rook   =  kind_tbl.(Piece.Kind.rook)   in
      let queen  =  kind_tbl.(Piece.Kind.queen)  in
      let king   =  kind_tbl.(Piece.Kind.king)   in
      parse_active active >>= fun active ->
      parse_castle castle >>= fun castle ->
      parse_en_passant en_passant >>= fun en_passant ->
      parse_halfmove halfmove >>= fun halfmove ->
      parse_fullmove fullmove >>= fun fullmove ->
      let pos =
        let checkers = lazy Bb.empty in
        let checks = lazy empty_checks in
        let attacks = lazy [||] in
        Fields.create ~hash:0L ~pawn_king_hash:0L ~checkers ~checks
          ~attacks ~white ~black ~pawn ~knight ~bishop ~rook ~queen
          ~king ~active ~castle ~en_passant ~halfmove ~fullmove in
      set_hash pos @@ Position_hash.of_position pos;
      set_pawn_king_hash pos @@ Position_hash.of_pawn_king pos;
      calculate_checkers pos;
      Position_analysis.calculate_checks pos;
      Position_attacks.calculate pos;
      if validate then validate_and_map pos else E.return pos
    | sections -> E.fail @@ Invalid_number_of_sections (List.length sections)
  else E.fail @@ String_too_long len

let of_string_exn ?(validate = true) s =
  of_string s ~validate |> Result.map_error ~f:Error.to_string |> function
  | Error e -> invalid_argf "Failed to parse FEN string '%s': %s" s e ()
  | Ok pos -> pos
