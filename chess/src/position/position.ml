open Core_kernel [@@warning "-D"]
open Monads.Std

include Position_common

module Histogram = Position_histogram
module Hash = Position_hash
module Attacks = Position_attacks
module Threats = Position_threats

type threats = Threats.t [@@deriving compare, equal, sexp]

module Analysis = Position_analysis
module Valid = Position_valid
module Fen = Position_fen

let pp = Fen.pp
let start = Fen.(of_string_exn start)

module Makemove = Position_makemove
module Child = Position_child

type child = Child.t

module Movegen = Position_movegen
include Position_movegen_helpers

module Unsafe = Position_unsafe

let null_move_exn pos =
  if in_check pos
  then invalid_argf "Illegal null move on position %s" (Fen.to_string pos) ()
  else Unsafe.null_move pos

module San = Position_san
module See = Position_see

include Position_comparable
include Base.Comparable.Make(struct
    include Position_comparable
    type t = position
  end)
