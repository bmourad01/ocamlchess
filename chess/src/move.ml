open Core_kernel

let bits = Piece.kind_bits + (Square.bits * 2)

module T = struct
  type t = int [@@deriving compare, equal, hash, sexp]
end

include T
include Comparable.Make(T)

let create ?(promote = None) src dst =
  let src = Square.to_int src in
  let dst = Square.to_int dst in
  (* As a hack, we'll use all set bits to indicate that no piece was promoted. *)
  let promote = Option.value_map promote ~default:(-1) ~f:Piece.Kind.to_int in
  (promote lsl (Square.bits * 2)) lor (dst lsl Square.bits) lor src

let create_with_promote src dst promote =
  let src = Square.to_int src in
  let dst = Square.to_int dst in
  let promote = Piece.Kind.to_int promote in
  (promote lsl (Square.bits * 2)) lor (dst lsl Square.bits) lor src

let forget_promote m = ((-1) lsl (Square.bits * 2)) lor m
let square_mask = (1 lsl Square.bits) - 1

let[@inline] src m = Square.of_int_unsafe @@ m land square_mask

let[@inline] dst m =
  Square.of_int_unsafe @@ (m lsr Square.bits) land square_mask

let[@inline] promote m = Piece.Kind.of_int @@ m lsr (Square.bits * 2)
let[@inline] decomp m = src m, dst m, promote m

let pp ppf m =
  Format.fprintf ppf "%a%a" Square.pp (src m) Square.pp (dst m);
  promote m |> Option.iter ~f:(fun k ->
      Format.fprintf ppf "%a" Piece.pp @@ Piece.create Black k)

let to_string m = Format.asprintf "%a" pp m

let of_string_exn s = try
    let src = String.subo s ~len:2 |> Square.of_string_exn in
    let dst = String.subo s ~pos:2 ~len:2 |> Square.of_string_exn in
    let promote = match String.length s with
      | 4 -> None
      | _ ->
        let s = String.subo s ~pos:4 in
        assert (String.length s = 1);
        Some (s.[0] |> Piece.of_fen_exn |> Piece.kind) in
    create src dst ~promote
  with Invalid_argument _ | Assert_failure _ ->
    invalid_argf "Invalid move string '%s'" s ()

let of_string s = Option.try_with @@ fun () -> of_string_exn s
