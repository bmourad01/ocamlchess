open Core_kernel

let bits = Piece.kind_bits + (Square.bits * 2)

module T = struct
  type t = int [@@deriving compare, equal, hash, sexp]
end

include T
include Comparable.Make(T)

module Promote = struct
  module T = struct
    type t = Knight | Bishop | Rook | Queen
    [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparable.Make(T)

  let nmask = lnot 0b11

  let of_piece_kind : Piece.kind -> t = function
    | Piece.Knight -> Knight
    | Piece.Bishop -> Bishop
    | Piece.Rook   -> Rook
    | Piece.Queen  -> Queen
    | k -> invalid_argf "Invalid promotion piece %s"
             (Piece.Kind.to_string_hum k) ()

  let[@inline] of_int i =
    Option.some_if Int.(i land nmask = 0) (Obj.magic i : t)

  let[@inline] to_int p = (Obj.(magic @@ repr p) : int)
  let[@inline] to_piece_kind p = Piece.Kind.of_int_exn (to_int p + 1)

  let to_string_hum p = Piece.Kind.to_string_hum @@ to_piece_kind p
end

type promote = Promote.t

let create ?(promote : promote option = None) src dst =
  let src = Square.to_int src in
  let dst = Square.to_int dst in
  (* As a hack, we'll use all set bits to indicate that no piece was promoted. *)
  let promote = Option.value_map promote ~default:(-1) ~f:Promote.to_int in
  (promote lsl (Square.bits * 2)) lor (dst lsl Square.bits) lor src

let create_with_promote src dst promote =
  let src = Square.to_int src in
  let dst = Square.to_int dst in
  let promote = Promote.to_int promote in
  (promote lsl (Square.bits * 2)) lor (dst lsl Square.bits) lor src

let square_mask = (1 lsl Square.bits) - 1

let[@inline] src m = Square.of_int_unsafe @@ m land square_mask

let[@inline] dst m =
  Square.of_int_unsafe @@ (m lsr Square.bits) land square_mask

let[@inline] promote m = Promote.of_int @@ m lsr (Square.bits * 2)
let[@inline] decomp m = src m, dst m, promote m

let[@inline] with_src m src =
  (m land lnot square_mask) lor Square.to_int src

let[@inline] with_dst m dst =
  let mask = lnot (square_mask lsl Square.bits) in
  (m land mask) lor (Square.to_int dst lsl Square.bits)

let[@inline] with_promote m promote =
  let mask = (square_mask lsl Square.bits) lor square_mask in
  (m land mask) lor Promote.to_int promote

let[@inline] without_promote m = m lor ((-1) lsl (Square.bits * 2))

let pp ppf m =
  Format.fprintf ppf "%a%a" Square.pp (src m) Square.pp (dst m);
  promote m |> Option.iter ~f:(fun k ->
      let k = Promote.to_piece_kind k in
      Format.fprintf ppf "%a" Piece.pp @@ Piece.create Black k)

let to_string m = Format.asprintf "%a" pp m

let of_string_exn s = try
    let src = String.subo s ~len:2 |> Square.of_string_exn in
    let dst = String.subo s ~pos:2 ~len:2 |> Square.of_string_exn in
    let promote = match String.length s with
      | 4 -> None
      | 5 ->
        let k = s.[4] |> Piece.of_fen_exn |> Piece.kind in
        Some (Promote.of_piece_kind k)
      | _ -> assert false in
    create src dst ~promote
  with Invalid_argument _ | Assert_failure _ ->
    invalid_argf "Invalid move string '%s'" s ()

let of_string s = Option.try_with @@ fun () -> of_string_exn s
