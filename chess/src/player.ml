open Core_kernel

include Player_intf

let players = ref @@ Map.empty (module String)

let register (player : t) =
  let key = player#name in
  match Map.add !players ~key ~data:player with
  | `Duplicate -> invalid_argf "Player %s is already registered" key ()
  | `Ok m -> players := m

let lookup = Map.find !players
let enumerate () = Map.data !players

(* Simple, weak players, taken directly from the paper:

   Elo World, a framework for benchmarking weak chess engines
   (http://tom7.org/chess/weak.pdf) by Tom Murphy VII. *)

module Bb = Bitboard
module Legal = Position.Legal

(* Static value assigned to each piece kind. *)
let piece_value = function
  | Piece.Pawn -> 1
  | Piece.(Knight | Bishop) -> 3
  | Piece.Rook -> 5
  | Piece.Queen -> 9
  | Piece.King -> 0

module Cccp = struct
  (* Try to checkmate the enemy king. *)
  let checkmate = List.filter ~f:(fun mv ->
      let pos = Legal.new_position mv in
      match Position.legal_moves pos with
      | [] -> Position.in_check pos
      | _ -> false)

  (* Try to check the enemy king. *)
  let check = List.filter ~f:(fun m ->
      Legal.new_position m |> Position.in_check)

  (* Try to capture an enemy piece of the highest value. *)
  let capture moves = Legal.best moves ~eval:(fun m ->
      Legal.capture m |> Option.map ~f:(fun (k, _) -> piece_value k))

  (* Push a piece that results in the the largest number of controlled
     squares. *)
  let push pos moves =
    let active = Position.active pos in
    Legal.best moves ~eval:(fun m ->
        let pos = Legal.new_position m in
        Some (Bb.count @@ Position.Attacks.all pos active))

  let () = register @@ object
      method choose pos = function
        | [] -> raise No_moves
        | moves ->
          let moves = match checkmate moves with
            | (_ :: _) as moves -> moves
            | [] -> match check moves with
              | (_ :: _) as moves -> moves
              | [] -> match capture moves with
                | (_ :: _) as moves -> moves
                | [] -> push pos moves in
          List.random_element_exn moves

      method limits = None
      method name = "cccp"
      method desc = "The player that plays the 'checkmate, check, capture, push \
                     strategy (in that order)."
    end
end

module Huddle = struct
  let () = register @@ object
      method choose pos = function
        | [] -> raise No_moves
        | moves ->
          let active = Position.active pos in
          Legal.best moves ~eval:(fun m ->
              let pos = Legal.new_position m in
              let active_board = Position.board_of_color pos active in
              let king = Position.king pos in
              let king_sq = Bb.(first_set_exn (king & active_board)) in
              Position.collect_color pos active |>
              List.fold ~init:0 ~f:(fun acc (sq, _) ->
                  acc - Square.chebyshev king_sq sq) |>
              Option.return) |>
          List.random_element_exn

      method limits = None
      method name = "huddle"
      method desc = "The player that tries to minimize the distance between its \
                     pieces and its king."
    end
end

module Max_oppt_moves = struct
  let () = register @@ object
      method choose _ = function
        | [] -> raise No_moves
        | moves ->
          Legal.best moves ~eval:(fun m ->
              let pos = Legal.new_position m in
              Some (List.length @@ Position.legal_moves pos)) |>
          List.random_element_exn

      method limits = None
      method name = "max-oppt-moves"
      method desc = "The player that attempts to maximize the number of \
                     moves the opponent can make."
    end
end

module Min_oppt_moves = struct
  let () = register @@ object
      method choose _ = function
        | [] -> raise No_moves
        | moves ->
          Legal.best moves ~eval:(fun m ->
              let pos = Legal.new_position m in
              Some (-(List.length @@ Position.legal_moves pos))) |>
          List.random_element_exn

      method limits = None
      method name = "min-oppt-moves"
      method desc = "The player that attempts to minimize the number of \
                     moves the opponent can make."
    end
end

module Opposite_color = struct
  let opposite_color active sq =
    let rank, file = Square.decomp sq in
    match active with
    | Piece.White -> (rank land 1) = (file land 1)
    | Piece.Black -> (rank land 1) <> (file land 1)

  let () = register @@ object
      method choose pos = function
        | [] -> raise No_moves
        | moves ->
          let active = Position.active pos in
          Legal.best moves ~eval:(fun m ->
              let pos = Legal.new_position m in
              Option.return @@ Bb.count @@
              Bb.filter ~f:(opposite_color active) @@
              Position.board_of_color pos active) |>
          List.random_element_exn

      method limits = None
      method name = "opposite-color"
      method desc = "The player that tries to maximize the number of pieces \
                     that are on squares that are opposite of its color."
    end
end

module Pacifist = struct
  let () = register @@ object
      method choose _ = function
        | [] -> raise No_moves
        | moves -> Legal.best moves ~eval:(fun m ->
            let pos = Legal.new_position m in
            let in_check = Position.in_check pos in
            (* Give the highest possible penalty for checkmating the opponent.
               Give a similarly high penalty for checking the opponent's king.
               In all other cases, maximize the opponent's material advantage. 
               This works to prioritize moves where we avoid capturing any
               piece. *)
            let score = match Position.legal_moves pos with
              | [] when in_check -> Int.min_value
              | _ when in_check -> Int.min_value / 2
              | _ ->
                Position.collect_active pos |>
                List.fold ~init:0 ~f:(fun material (_, k) ->
                    material + piece_value k) in
            Some score) |> List.random_element_exn

      method limits = None
      method name = "pacifist"
      method desc = "the player that avoids, in the following priority, \
                     checkmating the opponent, checking the opponent, and \
                     capturing pieces. Failing that, it will capture the \
                     lowest-value piece possible."
    end
end

module Random = struct
  let () = register @@ object
      method choose _ moves = match List.random_element moves with
        | None -> raise No_moves
        | Some m -> m

      method limits = None
      method name = "random"
      method desc = "The player that makes moves randomly."
    end 
end

module Same_color = struct
  let same_color active sq =
    let rank, file = Square.decomp sq in
    match active with
    | Piece.White -> (rank land 1) <> (file land 1)
    | Piece.Black -> (rank land 1) = (file land 1)

  let () = register @@ object
      method choose pos = function
        | [] -> raise Player_intf.No_moves
        | moves ->
          let active = Position.active pos in
          Legal.best moves ~eval:(fun m ->
              let pos = Legal.new_position m in
              Option.return @@ Bb.count @@
              Bb.filter ~f:(same_color active) @@
              Position.board_of_color pos active) |>
          List.random_element_exn

      method limits = None
      method name = "same-color"
      method desc = "The player that tries to maximize the number of pieces \
                     that are on squares of its color."
    end
end

module Suicide_king = struct
  let () = register @@ object
      method choose pos = function
        | [] -> raise Player_intf.No_moves
        | moves ->
          let active = Position.active pos in
          Legal.best moves ~eval:(fun m ->
              let pos = Legal.new_position m in
              let king = Position.king pos in
              let active_board = Position.board_of_color pos active in
              let enemy_board = Position.active_board pos in
              let k1 = Bb.(first_set_exn (king & active_board)) in
              let k2 = Bb.(first_set_exn (king & enemy_board)) in
              Some (-(Square.chebyshev k1 k2))) |>
          List.random_element_exn

      method limits = None
      method name = "suicide-king"
      method desc = "The player that attempts to minimize the distance between \
                     both kings."
    end
end

module Swarm = struct
  let () = register @@ object
      method choose pos = function
        | [] -> raise Player_intf.No_moves
        | moves ->
          let active = Position.active pos in
          let enemy = Position.enemy pos in
          let enemy_board = Position.board_of_color pos enemy in
          let king = Position.king pos in
          let king_sq = Bb.(first_set_exn (king & enemy_board)) in
          Legal.best moves ~eval:(fun m ->
              let pos = Legal.new_position m in
              Position.collect_color pos active |>
              List.fold ~init:0 ~f:(fun acc (sq, _) ->
                  acc - Square.chebyshev king_sq sq) |>
              Option.return) |>
          List.random_element_exn

      method limits = None
      method name = "swarm"
      method desc = "The player that tries to minimize the distance between its \
                     pieces and the enemy king."
    end
end
