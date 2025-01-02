open Core_kernel [@@warning "-D"]
open Position_common

module T = struct
  type t = {
    color : Piece.color;
    pawn  : Bb.t;
    minor : Bb.t;
    rook  : Bb.t;
  } [@@deriving compare, equal, sexp, fields]
end

include T
include Comparable.Make(T)

let get pos c =
  let them = board_of_color pos @@ Piece.Color.opposite c in
  let major = Bb.(pos.rook + pos.queen) in
  let minor = Bb.(pos.knight + pos.bishop) in
  let pawn_att = Position_attacks.pawn pos c in
  let knight_att = Position_attacks.knight pos c in
  let bishop_att = Position_attacks.bishop pos c in
  let rook_att = Position_attacks.rook pos c in
  Fields.create
    ~color:c
    ~pawn:Bb.(pawn_att & them & (minor + major))
    ~minor:Bb.((knight_att + bishop_att) & them & major)
    ~rook:Bb.(rook_att & them & pos.queen)

let count t = Bb.count t.pawn + Bb.count t.minor + Bb.count t.rook
