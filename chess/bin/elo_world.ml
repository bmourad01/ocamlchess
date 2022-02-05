open Core_kernel
open Chess

module Bb = Bitboard
module Legal = Position.Legal

module Cccp = struct
  (* Try to checkmate the inactive king. *)
  let checkmate = List.filter ~f:(fun mv ->
      let pos = Legal.new_position mv in
      match Position.legal_moves pos with
      | [] -> Position.in_check pos
      | _ -> false)

  (* Try to check the inactive king. *)
  let check = List.filter ~f:(fun m ->
      Legal.new_position m |> Position.in_check)

  (* Try to capture an inactive piece of the highest value. *)
  let capture moves = Legal.best moves ~eval:(fun m ->
      Legal.capture m |> Option.map ~f:Piece.Kind.value)

  (* Push a piece that results in the the largest number of controlled
     squares. *)
  let push moves = Legal.best moves ~eval:(fun m ->
      let active = Position.active @@ Legal.parent m in
      let pos = Legal.new_position m in
      Some (Bb.count @@ Position.Attacks.all pos active))

  let choice _ _ moves =
    let moves = match checkmate moves with
      | (_ :: _) as moves -> moves
      | [] -> match check moves with
        | (_ :: _) as moves -> moves
        | [] -> match capture moves with
          | (_ :: _) as moves -> moves
          | [] -> push moves in
    List.random_element_exn moves, ()

  let player = Player.create () ~choice ~state:() ~name:"cccp"
      ~desc:"The player that plays the \"checkmate, check, capture, \
             push strategy\" (in that order)."
end

module Huddle = struct
  let choice _ _ moves =
    Legal.best moves ~eval:(fun m ->
        let active = Position.active @@ Legal.parent m in
        let pos = Legal.new_position m in
        let active_board = Position.board_of_color pos active in
        let king = Position.king pos in
        let king_sq = Bb.(first_set_exn (king & active_board)) in
        Position.collect_color pos active |>
        List.fold ~init:0 ~f:(fun acc (sq, _) ->
            acc - Square.chebyshev king_sq sq) |>
        Option.return) |> List.random_element_exn, ()

  let player = Player.create () ~choice ~state:() ~name:"huddle"
      ~desc:"The player that tries to minimize the distance between its \
             pieces and its king."
end

module Max_oppt_moves = struct
  let choice _ _ moves =
    Legal.best moves ~eval:(fun m ->
        let pos = Legal.new_position m in
        Some (List.length @@ Position.legal_moves pos)) |>
    List.random_element_exn, ()

  let player = Player.create () ~choice ~state:() ~name:"max-oppt-moves"
      ~desc:"The player that attempts to maximize the number of \
             moves the opponent can make."
end

module Min_oppt_moves = struct
  let choice _ _ moves =
    Legal.best moves ~eval:(fun m ->
        let pos = Legal.new_position m in
        Some (-(List.length @@ Position.legal_moves pos))) |>
    List.random_element_exn, ()

  let player = Player.create () ~choice ~state:() ~name:"min-oppt-moves"
      ~desc:"The player that attempts to minimize the number of \
             moves the opponent can make."
end

module Opposite_color = struct
  let opposite_color active sq =
    not @@ Piece.Color.equal active @@ Square.color sq

  let choice _ _ moves =
    Legal.best moves ~eval:(fun m ->
        let active = Position.active @@ Legal.parent m in
        let pos = Legal.new_position m in
        Option.return @@ Bb.count @@
        Bb.filter ~f:(opposite_color active) @@
        Position.board_of_color pos active) |> List.random_element_exn, ()

  let player = Player.create () ~choice ~state:() ~name:"opposite-color"
      ~desc:"The player that tries to maximize the number of pieces \
             that are on squares that are opposite of its color."
end

module Pacifist = struct
  let choice _ _ moves =
    Legal.best moves ~eval:(fun m ->
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
                material + Piece.Kind.value k) in
        Some score) |> List.random_element_exn, ()


  let player = Player.create () ~choice ~state:() ~name:"pacifist"
      ~desc:"The player that avoids, in the following priority, \
             checkmating the opponent, checking the opponent, and \
             capturing pieces. Failing that, it will capture the \
             lowest-value piece possible."
end

module Random = struct
  let choice _ _ moves = List.random_element_exn moves, ()
  let player = Player.create () ~choice ~state:() ~name:"random"
      ~desc:"The player that makes moves randomly."
end

module Same_color = struct
  let same_color active sq = Piece.Color.equal active @@ Square.color sq
  let choice _ _ moves =
    Legal.best moves ~eval:(fun m ->
        let active = Position.active @@ Legal.new_position m in
        let pos = Legal.new_position m in
        Option.return @@ Bb.count @@
        Bb.filter ~f:(same_color active) @@
        Position.board_of_color pos active) |> List.random_element_exn, ()

  let player = Player.create () ~choice ~state:() ~name:"same-color"
      ~desc:"The player that tries to maximize the number of pieces \
             that are on squares of its color."
end

module Suicide_king = struct
  let choice _ _ moves =
    Legal.best moves ~eval:(fun m ->
        let active = Position.active @@ Legal.parent m in
        let pos = Legal.new_position m in
        let king = Position.king pos in
        let active_board = Position.board_of_color pos active in
        let inactive_board = Position.active_board pos in
        let k1 = Bb.(first_set_exn (king & active_board)) in
        let k2 = Bb.(first_set_exn (king & inactive_board)) in
        Some (-(Square.chebyshev k1 k2))) |> List.random_element_exn, ()

  let player = Player.create () ~choice ~state:() ~name:"suicide-king"
      ~desc:"The player that attempts to minimize the distance between \
             both kings."
end

module Swarm = struct
  let choice _ _ moves =
    let pos = Legal.parent @@ List.hd_exn moves in
    let active = Position.active pos in
    let inactive = Position.inactive pos in
    let inactive_board = Position.board_of_color pos inactive in
    let king = Position.king pos in
    let king_sq = Bb.(first_set_exn (king & inactive_board)) in
    Legal.best moves ~eval:(fun m ->
        let pos = Legal.new_position m in
        Position.collect_color pos active |>
        List.fold ~init:0 ~f:(fun acc (sq, _) ->
            acc - Square.chebyshev king_sq sq) |>
        Option.return) |> List.random_element_exn, ()

  let player = Player.create () ~choice ~state:() ~name:"swarm"
      ~desc:"The player that tries to minimize the distance between its \
             pieces and the inactive king."
end

let register : 'a. 'a Player.t -> unit = Player.register

let init =
  let once = ref false in
  fun () -> if !once then () else begin
      register Cccp.player;
      register Huddle.player;
      register Max_oppt_moves.player;
      register Min_oppt_moves.player;
      register Opposite_color.player;
      register Pacifist.player;
      register Random.player;
      register Same_color.player;
      register Suicide_king.player;
      register Swarm.player;
      once := true;
    end
