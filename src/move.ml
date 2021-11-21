open Core_kernel

let bits = Piece.kind_bits + (Square.bits * 2)

module T = struct type t = int [@@deriving compare, equal, hash, sexp] end
include T
include Comparable.Make (T)

let create ?(promote = None) src dst =
  let src = Square.to_int src in
  let dst = Square.to_int dst in
  let promote = Option.value_map promote ~default:(-1) ~f:Piece.Kind.to_int in
  (promote lsl (Square.bits * 2)) lor (dst lsl Square.bits) lor src

let square_mask = (1 lsl Square.bits) - 1
let src m = m land square_mask |> Square.of_int_exn
let dst m = (m lsr Square.bits) land square_mask |> Square.of_int_exn
let promote m = m lsr (Square.bits * 2) |> Piece.Kind.of_int

let to_string m =
  sprintf "%s%s%s"
    (src m |> Square.to_string)
    (dst m |> Square.to_string)
    ( promote m
    |> Option.value_map ~default:"" ~f:(fun kind ->
         Piece.create Black kind |> Piece.to_fen |> Char.to_string ) )

let of_string_exn s =
  try
    let src = String.subo s ~len:2 |> Square.of_string_exn in
    let dst = String.subo s ~pos:2 ~len:2 |> Square.of_string_exn in
    let promote =
      if String.length s = 4 then None
      else
        let s = String.subo s ~pos:4 in
        assert (String.length s = 1);
        Some (s.[0] |> Piece.of_fen_exn |> Piece.kind) in
    create src dst ~promote
  with Invalid_argument _ | Assert_failure _ ->
    invalid_arg (sprintf "Invalid move string '%s'" s)

let of_string s = Option.try_with (fun () -> of_string_exn s)
