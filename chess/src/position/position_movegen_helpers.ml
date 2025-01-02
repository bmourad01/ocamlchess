open Core_kernel [@@warning "-D"]
open Position_common

let legal_moves pos =
  Position_movegen.(go bb_to_moves) @@ Position_analysis.create pos

let capture_moves pos =
  Position_movegen.(go_captures bb_to_moves) @@ Position_analysis.create pos

let promotion_moves pos =
  Position_movegen.(go_promotions bb_to_moves) @@ Position_analysis.create pos

let quiet_moves pos =
  Position_movegen.(go_quiet bb_to_moves) @@ Position_analysis.create pos

let children pos = Position_movegen.(go bb_to_children) @@ Position_analysis.create pos

let capture_children pos =
  Position_movegen.(go_captures bb_to_children) @@ Position_analysis.create pos

let promotion_children pos =
  Position_movegen.(go_promotions bb_to_children) @@ Position_analysis.create pos

let quiet_children pos =
  Position_movegen.(go_quiet bb_to_children) @@ Position_analysis.create pos

let make_move pos m =
  children pos |> List.find ~f:(Fn.flip Position_child.is_move m)

let make_move_exn pos move = match make_move pos move with
  | None -> invalid_argf "Move %s is not legal" (Move.to_string move) ()
  | Some child -> child
