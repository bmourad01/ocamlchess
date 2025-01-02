(* Evaluation of pawn placement. *)

open Core_kernel [@@warning "-D"]
open Eval_common
open Score.Syntax

module Passers = struct
  let t = [|
    (* No backup. *)
    0 $ 0;
    -11 $ -18;
    -16 $ 18;
    -18 $ 29;
    -22 $ 61;
    21 $ 59;
    0 $ 0;
    0 $ 0;
    (* Backup. *)
    0 $ 0;
    -12 $ 21;
    -7 $ 27;
    2 $ 53;
    22 $ 116;
    49 $ 78;
    0 $ 0;
    0 $ 0;
  |]

  let get backup rank =
    let i = rank + (Bool.to_int backup * Square.Rank.count) in
    uget t i
end

(* By file. *)
let isolated = [|
  -13 $ -12;
  -1 $ -16;
  1 $ -16;
  3 $ -18;
  7 $ -19;
  3 $ -15;
  -4 $ -14;
  -4 $ -17;
|]

module Stacked = struct
  let t = [|
    (* Not unstackable. *)
    10 $ -29;
    -2 $ -26;
    0 $ -23;
    0 $ -20;
    3 $ -20;
    5 $ -26;
    4 $ -30;
    8 $ -31;
    (* Unstackable. *)
    3 $ -14;
    0 $ -15;
    -6 $ -9;
    -7 $ -10;
    -4 $ -9;
    -2 $ -10;
    0 $ -13;
    0 $ -17;
  |]

  let get unstack file =
    let i = file + (Bool.to_int unstack * Square.File.count) in
    uget t i
end

module Backwards = struct
  let t = [|
    (* Blocking. *)
    0 $ 0;
    0 $ -7;
    7 $ -7;
    6 $ -18;
    -4 $ -29;
    0 $ 0;
    0 $ 0;
    0 $ 0;
    (* Not blocking. *)
    0 $ 0;
    -9 $ -32;
    -5 $ -30;
    3 $ -31;
    29 $ -41;
    0 $ 0;
    0 $ 0;
    0 $ 0;
  |]

  let get not_blocking rank =
    let i = rank + (Bool.to_int not_blocking * Square.Rank.count) in
    uget t i
end

module Connected = struct
  let t = [|
    (* 1 *)
    0 $ 0;
    0 $ 0;
    0 $ 0;
    0 $ 0;
    (* 2 *)
    -1 $ -11;
    12 $ -4;
    0 $ -2;
    6 $ 8;
    (* 3 *)
    14 $ 0;
    20 $ -6;
    19 $ 3;
    17 $ 8;
    (* 4 *)
    6 $ -1;
    20 $ 1;
    6 $ 3;
    14 $ 10;
    (* 5 *)
    8 $ 14;
    21 $ 17;
    31 $ 23;
    25 $ 18;
    (* 6 *)
    45 $ 40;
    36 $ 64;
    58 $ 74;
    64 $ 88;
    (* 7 *)
    108 $ 35;
    214 $ 45;
    216 $ 70;
    233 $ 61;
    (* 8 *)
    0 $ 0;
    0 $ 0;
    0 $ 0;
    0 $ 0;
  |]

  let get sq c =
    let r = relative_rank c sq in
    let fc = Square.File.count / 2 in
    let f = Square.(File.mirror_exn @@ file sq) in
    uget t (fc * r + f)
end

let[@inline] go info c =
  let open Bb in
  let our_info = Eval_info.color info c in
  let c' = Piece.Color.opposite c in
  let all = Position.all_board info.pos in
  let us = Position.board_of_color info.pos c in
  let them = all - us in
  let pawn = Position.pawn info.pos in
  let our_pawns = pawn & us in
  let their_pawns = pawn & them in
  let fwd = square_fwd c in
  let relr = relative_rank c in
  let eval = Bb.fold our_pawns ~init:(0 $ 0) ~f:(fun acc sq ->
      let fsq = fwd sq and file = Square.file sq in
      (* Features. *)
      let neighbors    = our_pawns   & neighbor_files_exn file in
      let backup       = our_pawns   & Pre.passed_pawns sq c'  in
      let stoppers     = their_pawns & Pre.passed_pawns sq c   in
      let threats      = their_pawns & Pre.pawn_capture sq c   in
      let support      = our_pawns   & Pre.pawn_capture sq c'  in
      let push_threats = their_pawns & Pre.pawn_capture fsq c  in
      let push_support = our_pawns   & Pre.pawn_capture fsq c' in
      let leftovers    = stoppers ^ threats ^ push_threats     in
      (* Passed pawns. *)
      let acc =
        if stoppers <> empty then
          let ns, nt = count push_support, count push_threats in
          if leftovers = empty && Int.(ns >= nt) then
            let ns, nt = count support, count threats in
            acc +$ Passers.get Int.(ns >= nt) (relr sq)
          else acc
        else begin
          our_info.passed_pawns <- our_info.passed_pawns ++ sq;
          acc
        end in
      (* Isolated pawns. *)
      let not_isolated = threats <> empty || neighbors <> empty in
      let acc = if not_isolated then acc else acc +$ uget isolated file in
      (* Stacked (doubled) pawns. *)
      let fb = file_exn file in
      let acc =
        if several (our_pawns & fb) then
          let unstack =
            (stoppers <> empty && not_isolated) ||
            (stoppers - Pre.forward_files sq c) <> empty in
          acc +$ Stacked.get unstack file
        else acc in
      (* Backward or connected pawns. *)
      let acc =
        if neighbors <> empty
        && push_threats <> empty
        && backup = empty then
          let not_blocking = (fb & their_pawns) = empty in
          acc +$ Backwards.get not_blocking (relr sq)
        else if (our_pawns & Pre.connected_pawns sq c) <> empty then
          acc +$ Connected.get sq c
        else acc in
      acc) in
  our_info.pawn_king_eval <- eval
