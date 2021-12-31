open Core_kernel

module Bb = Bitboard
module Legal = Position.Legal

let piece_value (k : Piece.kind) = match k with
  | Pawn -> 1
  | Knight | Bishop -> 3
  | Rook -> 5
  | Queen -> 9
  | King -> 0

let choose _ = function
  | [] -> raise Player.No_moves
  | moves -> Player.best_moves moves ~eval:(fun m ->
      let pos = Legal.new_position m in
      let in_check = Position.in_check pos in
      (* Give the highest possible penalty for checkmating the opponent.
         Give a similarly high penalty for checking the opponent's king.
         In all other cases, maximize the opponent's material advantage. 
         This works to prioritize moves where we avoid capturing any piece. *)
      let score = match Position.legal_moves pos with
        | [] when in_check -> Int.min_value
        | _ when in_check -> Int.min_value / 2
        | _ ->
          Position.collect_active pos |>
          List.fold ~init:0 ~f:(fun material (_, k) ->
              material + piece_value k) in
      Some score) |> List.random_element_exn

let create ?(limits = None) () = object
  method choose = choose
  method limits = limits
  method name = "pacifist"
end
