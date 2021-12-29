open Core_kernel

module Bb = Bitboard
module Legal = Position.Legal

let avoid_checkmate = List.filter ~f:(fun m ->
    let pos = Legal.new_position m in
    match Position.legal_moves pos with
    | [] -> not @@ Position.in_check pos
    | _ -> true)

let avoid_check = List.filter ~f:(fun m ->
    Legal.new_position m |> Position.in_check |> not)

let avoid_capture =
  List.filter ~f:(fun m -> Option.is_none @@ Legal.capture m)

let piece_value (k : Piece.kind) = match k with
  | Pawn -> 1
  | Knight | Bishop -> 3
  | Rook -> 5
  | Queen -> 9
  | King -> 0

let capture_lowest_value =
  Player.best_moves ~eval:(fun m ->
      Legal.capture m |> Option.map ~f:(fun (k, _) -> -(piece_value k)))

let choose _ = function
  | []-> raise Player.No_moves
  | moves ->
    let moves = match avoid_checkmate moves with
      | (_ :: _) as moves -> moves
      | [] -> match avoid_check moves with
        | (_ :: _) as moves -> moves
        | [] -> match avoid_capture moves with
          | (_ :: _) as moves -> moves
          | [] -> capture_lowest_value moves in
    List.random_element_exn moves

let create ?(limits = None) () = object
  method choose = choose
  method limits = limits
  method name = "pacifist"
end
