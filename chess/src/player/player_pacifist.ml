open Core_kernel

module Bb = Bitboard
module Legal = Position.Legal_move
module Legals = Position.Legal_moves

let avoid_checkmate = List.filter ~f:(fun mv ->
    let pos = Legal.position mv in
    match Legals.moves @@ Position.legal_moves pos with
    | [] -> not @@ Position.in_check pos
    | _ -> true)

let avoid_check = List.filter ~f:(fun mv ->
    Legal.position mv |> Position.in_check |> not)

let avoid_capture pos =
  let enemy = Position.enemy pos in
  List.filter ~f:(fun mv ->
      let pos' = Legal.position mv in
      let b = Position.board_of_color pos enemy in
      let b' = Position.board_of_color pos' enemy in
      Option.is_none Bb.(first_set (b ^ b')))

let piece_value (k : Piece.kind) = match k with
  | Pawn -> 1
  | Knight | Bishop -> 3
  | Rook -> 5
  | Queen -> 9
  | King -> 0

let capture_lowest_value pos moves =
  let enemy = Position.enemy pos in
  Player.best_moves moves ~eval:(fun mv ->
      let open Option.Monad_infix in
      let pos' = Legal.position mv in
      let b = Position.board_of_color pos enemy in
      let b' = Position.board_of_color pos' enemy in
      Bb.(first_set (b ^ b')) >>=
      Position.piece_at_square pos >>| fun p ->
      piece_value @@ Piece.kind p)

let choose legals = match Legals.decomp legals with
  | [], _ -> raise Player.No_moves
  | moves, pos ->
    let moves = match avoid_checkmate moves with
      | (_ :: _) as moves -> moves
      | [] -> match avoid_check moves with
        | (_ :: _) as moves -> moves
        | [] -> match avoid_capture pos moves with
          | (_ :: _) as moves -> moves
          | [] -> capture_lowest_value pos moves in
    List.random_element_exn moves

let create ?(limits = None) () = object
  method choose = choose
  method limits = limits
  method name = "pacifist"
end
