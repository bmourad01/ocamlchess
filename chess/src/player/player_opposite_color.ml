open Core_kernel

module Bb = Bitboard
module Legal = Position.Legal

let opposite_color active sq =
  let rank, file = Square.decomp sq in
  match active with
  | Piece.White -> (rank land 1) = (file land 1)
  | Piece.Black -> (rank land 1) <> (file land 1)

let choose pos = function
  | [] -> raise Player.No_moves
  | moves ->
    let active = Position.active pos in
    Player.best_moves moves ~eval:(fun m ->
        let pos = Legal.new_position m in
        Option.return @@ Bb.count @@
        Bb.filter ~f:(opposite_color active) @@
        Position.board_of_color pos active) |>
    List.random_element_exn

let create ?(limits = None) () = object
  method choose = choose
  method limits = limits
  method name = "opposite-color"
end
