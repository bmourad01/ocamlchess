(* Scale the evaluation based on certain conditions. *)

open Core_kernel [@@warning "-D"]
open Eval_common
open Score.Syntax

let (>>?) x f = match x with
  | None -> f ()
  | Some x -> x

module Opposite_bishops = struct
  let one_knight = 106
  let one_rook = 96
  let only_bishops = 64

  let[@inline] go w b knight bishop rook queen =
    let open Bb.Syntax in
    if Bb.count (w & bishop) = 1
    && Bb.count (b & bishop) = 1
    && Bb.count (bishop & Bb.white) = 1 then
      if Bb.((rook + queen) = empty)
      && Bb.count (w & knight) = 1
      && Bb.count (b & knight) = 1 then
        Some one_knight
      else if Bb.((knight + queen) = empty)
           && Bb.count (w & rook) = 1
           && Bb.count (b & rook) = 1 then
        Some one_rook
      else if Bb.((knight + rook + queen) = empty) then
        Some only_bishops
      else None
    else None
end

module Lone_queen = struct
  let scale = 88

  let[@inline] go knight bishop rook queen weak =
    let open Bb.Syntax in
    let pieces = knight + bishop + rook in
    if Bb.count queen = 1
    && Bb.several pieces
    && Bb.(pieces = (weak & pieces))
    then Some scale else None
end

module Lone_minor = struct
  let scale = 0

  let[@inline] go knight bishop strong =
    if Bb.((strong & (knight + bishop)) <> empty)
    && Bb.count strong = 2 then Some scale else None
end

module Lone_vs_pawns = struct
  let scale = 144

  let[@inline] go w b pawn knight bishop rook queen weak strong =
    let open Bb in
    let pieces = knight + bishop + rook in
    if queen = empty
    && not (several (pieces & w))
    && not (several (pieces & b))
    && Int.(count (strong & pawn) - count (weak & pawn) > 2)
    then Some scale else None
end

let normal = 128

let[@inline] go pos eval =
  let pawn = Position.pawn pos in
  let knight = Position.knight pos in
  let bishop = Position.bishop pos in
  let rook = Position.rook pos in
  let queen = Position.queen pos in
  let w = Position.white pos in
  let b = Position.black pos in
  let weak = if Score.eg eval < 0 then w else b in
  let strong = if Score.eg eval < 0 then b else w in
  Opposite_bishops.go w b knight bishop rook queen >>? fun () ->
  Lone_queen.go knight bishop rook queen weak >>? fun () ->
  Lone_minor.go knight bishop strong >>? fun () ->
  Lone_vs_pawns.go w b pawn knight bishop rook queen weak strong >>? fun () ->
  imin normal (96 + Bb.(count (pawn & strong)) * 8)
