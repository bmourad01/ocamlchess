open Core_kernel [@@warning "-D"]
open Monads.Std
open Bap_future.Std

module Bb = Bitboard
module Pre = Precalculated
module Child = Position.Child
module See = Position.See
module Oa = Option_array

let[@inline][@specialise] (>>?) x f = match x with
  | None -> f ()
  | Some _ -> x

let b2i = Bool.to_int
let b2in = Fn.compose b2i not

let[@inline] max x y =
  let m = x - y in
  x - (m land (m asr (Caml.Sys.int_size - 1)))

let[@inline] min x y =
  let m = x - y in
  y + (m land (m asr (Caml.Sys.int_size - 1)))

(* Constants. *)
let inf = 65535
let mate_score = inf / 2
let max_ply = 64
let max_moves = 256
let update_time = 3000

let ply_to_moves ply = (ply + (ply land 1)) / 2

(* We'll use the UCI datatype, which is either a numerical score in centipawns,
   or our distance from checkmate.

   The centipawn score is always exact, since the aspiration loop will not
   terminate until we get a score that is within our search window.
*)
let convert_score score ~bound ~mate ~mated =
  let open Uci.Send.Info in
  if mate then Mate (ply_to_moves (mate_score - score))
  else if mated then Mate (-(ply_to_moves (mate_score + score)))
  else Cp (score, bound)

(* Mate scores should be relative to the distance from the root position.
   Shorter distances are to be preferred. *)
let is_mate score = score >= mate_score - max_ply && score <= mate_score
let is_mated score = score >= -mate_score && score <= (-mate_score) + max_ply

(* When we return a mate score during search, it's from the perspective
   of the player that is being mated. *)
let mating ply = mate_score - ply
let mated ply = -mate_score + ply

(* Evaluations aren't useful for positions that are in check, so just
   assume that if we've made it this far from the root then it's a draw *)
let end_of_line pos ~check = if check then 0 else Eval.go pos

(* The `check` parameter is for deciding when to generate quiet checks in
   quiescence search. *)
let is_noisy ?(check = true) m =
  Child.is_capture m ||
  Move.is_promote @@ Child.move m ||
  (check && Child.gives_check m)

let is_quiet ?(check = true) =
  Fn.non @@ is_noisy ~check

let same_move a b =
  Child.is_move a @@ Child.move b
