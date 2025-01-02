(* Evaluation of threats. *)

open Core_kernel [@@warning "-D"]
open Eval_common
open Score.Syntax

let weak_pawn = -11 $ -38
let pawn_minor = -55 $ -83
let minor_minor = -25 $ -45
let major_minor = -30 $ -55
let pawn_minor_rook = -48 $ -28
let king_minor = -43 $ -21
let king_rook = -33 $ -18
let any_queen = -50 $ -7
let overload = -7 $ -16
let pawn_push = 15 $ 32

let[@inline] go info c =
  let open Bb in
  let c' = Piece.Color.opposite c in
  let our_info = Eval_info.color info c in
  let their_info = Eval_info.color info c' in
  let na = ~~(our_info.attacks) in
  let na2 = ~~(our_info.attacks2) in
  let na' = ~~(their_info.attacks) in
  let na2' = ~~(their_info.attacks2) in
  let p = Eval_info.attacked_by our_info Pawn in
  let all = Position.all_board info.pos in
  let us = Position.board_of_color info.pos c in
  let them = all - us in
  let pawn = Position.pawn info.pos in
  let knight = Position.knight info.pos in
  let bishop = Position.bishop info.pos in
  let rook = Position.rook info.pos in
  let queen = Position.queen info.pos in
  let our_minors = (knight + bishop) & us in
  (* Enemy attacks. *)
  let pawn_att   = Eval_info.attacked_by their_info Pawn   in
  let knight_att = Eval_info.attacked_by their_info Knight in
  let bishop_att = Eval_info.attacked_by their_info Bishop in
  let rook_att   = Eval_info.attacked_by their_info Rook   in
  let queen_att  = Eval_info.attacked_by their_info Queen  in
  let king_att   = Eval_info.attacked_by their_info King   in
  let minor_att  = knight_att + bishop_att in
  let major_att  = rook_att   + queen_att  in
  (* More attackers, few defenders, and no pawn support. *)
  let poorly_defended =
    (their_info.attacks & na) +
    (their_info.attacks2 & na2 & ~~p) in
  let weak_minors = our_minors & poorly_defended in
  (* Attacked and defended by exactly one. *)
  let overloaded =
    ((knight + bishop + rook + queen) & us) &
    our_info.attacks & na2 & their_info.attacks & na2' in
  (* New threats from a pawn advance. *)
  let np' = ~~pawn_att in
  let push_threat =
    let adv = pawn_advance c in
    (* Discard squares that are already theatened by our pawns. *)
    let m = ~~(them - p) in
    (* Single pawn advance. *)
    let b = adv (pawn & us) - all in
    (* Double pawn advance if we're at the third rank and we're
       not countered by an enemy pawn. *)
    let b = b + (adv (b & np' & third_rank c) - all) in
    (* Remove pawn counter-threats. *)
    let b = b & np' in
    (* Focus on squares that are not attacked or can be defended. *)
    let b = b & (na' + our_info.attacks) in
    (* Get the pieces threatened by the pawn push. *)
    let l, r = pawn_captures b c in
    (l + r) - m in
  (* Combine the features. *)
  (weak_pawn       ** count (pawn & us & np' & poorly_defended))      +$
  (pawn_minor      ** count (our_minors & pawn_att))                  +$
  (minor_minor     ** count (our_minors & minor_att))                 +$
  (major_minor     ** count (weak_minors & major_att))                +$
  (pawn_minor_rook ** count (rook & us & (pawn_att + minor_att)))     +$
  (king_minor      ** count (weak_minors & king_att))                 +$
  (king_rook       ** count (rook & us & poorly_defended & king_att)) +$
  (any_queen       ** count (queen & us & their_info.attacks))        +$
  (overload        ** count overloaded)                               +$
  (pawn_push       ** count push_threat)
