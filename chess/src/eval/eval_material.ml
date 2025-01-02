(* Static evaluation for how much a piece is "worth". *)

open Eval_score.Syntax

module White = struct
  let pawn = 82 $ 114
  let knight = 427 $ 475
  let bishop = 441 $ 510
  let rook = 627 $ 803
  let queen = 1292 $ 1623
  let king = 0 $ 0
end

module Black = struct
  let pawn = -82 $ -114
  let knight = -427 $ -475
  let bishop = -441 $ -510
  let rook = -627 $ -803
  let queen = -1292 $ -1623
  let king = 0 $ 0
end
