open Core_kernel
open Chess

module Bb = Bitboard
module Cr = Castling_rights
module Legal = Position.Legal

let thunk player = fun () -> Player.T player

module Alphabetical = struct
  let choice _ moves =
    List.sort moves ~compare:(fun a b ->
        let sa = Position.San.of_legal a in
        let sb = Position.San.of_legal b in
        String.compare sa sb) |> List.hd_exn, ()

  let name = "alphabetical"
  let create = thunk @@ Player.create ~choice ~state:() ~name
      ~desc:"The player that plays the alphabetically first move. The \
             moves are ordered by their Standard Algebraic Notation (SAN) \
             string (e.g. a3 < O-O < Qxg7)."
end

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

  let choice _ moves =
    let moves = match checkmate moves with
      | (_ :: _) as moves -> moves
      | [] -> match check moves with
        | (_ :: _) as moves -> moves
        | [] -> match capture moves with
          | (_ :: _) as moves -> moves
          | [] -> push moves in
    List.random_element_exn moves, ()

  let name = "cccp"
  let create = thunk @@ Player.create ~choice ~state:() ~name
      ~desc:"The player that plays the \"checkmate, check, capture, \
             push strategy\" (in that order)."
end

module Equalizer = struct
  type state = {
    moved : int Map.M(Square).t;
    visited_white : int Map.M(Square).t;
    visited_black : int Map.M(Square).t;
  }

  let least_moved moved moves = Legal.best moves ~eval:(fun m ->
      match Map.find moved @@ Move.src @@ Legal.move m with
      | Some n -> Some (-n)
      | None -> Some 0)

  let least_visited visited moves = Legal.best moves ~eval:(fun m ->
      match Map.find visited @@ Move.dst @@ Legal.move m with
      | Some n -> Some (-n)
      | None -> Some 0)

  let update_moved moved src dst =
    let n = match Map.find moved src with
      | Some n -> n + 1
      | None -> 1 in
    let moved = Map.remove moved src in
    Map.set moved ~key:dst ~data:n

  let update_castle m moved visited active =
    Option.value_map ~default:(moved, visited) ~f:(fun side ->
        let rank = match active with
          | Piece.White -> Square.Rank.one
          | Piece.Black -> Square.Rank.eight in
        let file, src = match side with
          | Cr.Kingside ->
            Square.File.f, Square.create_unsafe ~rank ~file:Square.File.h
          | Cr.Queenside ->
            Square.File.d, Square.create_unsafe ~rank ~file:Square.File.a in
        let dst = Square.create_unsafe ~rank ~file in
        let moved = update_moved moved src dst in
        let visited =
          Square.create_unsafe ~rank ~file |>
          Map.update visited ~f:(function
              | Some n -> n + 1
              | None -> 1) in
        moved, visited) @@ Legal.castle_side m

  let update_en_passant m moved dst active = 
    if Legal.is_en_passant m then
      let rank = Square.rank dst in
      let pw = match active with
        | Piece.White -> Square.with_rank_unsafe dst @@ rank - 1
        | Piece.Black -> Square.with_rank_unsafe dst @@ rank + 1 in
      Map.remove moved pw
    else moved

  let choice state moves =
    let parent = Legal.parent @@ List.hd_exn moves in
    let active = Position.active parent in
    let visited = match active with
      | White -> state.visited_white
      | Black -> state.visited_black in
    let least_moved = least_moved state.moved moves in
    let moves =
      let len = List.length least_moved in
      if len = List.length moves
      then least_visited visited moves
      else least_moved in
    let m = List.random_element_exn moves in
    let src = Move.dst @@ Legal.move m in
    let dst = Move.dst @@ Legal.move m in
    let moved = update_moved state.moved src dst in
    let visited = Map.update visited dst ~f:(function
        | Some n -> n + 1
        | None -> 1) in
    (* Update the square of the rook if we castled. *)
    let moved, visited = update_castle m moved visited active in
    (* Handle en passant capture. *)
    let moved = update_en_passant m moved dst active in
    (* Update the state. *)
    let state = match active with
      | White -> {state with moved; visited_white = visited}
      | Black -> {state with moved; visited_black = visited} in
    m, state

  let state =
    let moved = Map.empty (module Square) in
    let visited_white = Map.of_alist_exn (module Square) [
        Square.a1, 1; Square.b1, 1; Square.c1, 1; Square.d1, 1;
        Square.e1, 1; Square.f1, 1; Square.g1, 1; Square.h1, 1;
        Square.a2, 1; Square.b2, 1; Square.c2, 1; Square.d2, 1;
        Square.e2, 1; Square.f2, 1; Square.g2, 1; Square.h2, 1;
      ] in
    let visited_black = Map.of_alist_exn (module Square) [
        Square.a7, 1; Square.b7, 1; Square.c7, 1; Square.d7, 1;
        Square.e7, 1; Square.f7, 1; Square.g7, 1; Square.h7, 1;
        Square.a8, 1; Square.b8, 1; Square.c8, 1; Square.d8, 1;
        Square.e8, 1; Square.f8, 1; Square.g8, 1; Square.h8, 1;
      ] in
    {moved; visited_white; visited_black}

  let name = "equalizer"
  let create = thunk @@ Player.create ~choice ~state ~name
      ~desc:"The player that prefers to move pieces that have been moved the \
             fewest number of times, then prefers to move to squares that \
             have been visited the least number of times. The behavior of \
             this player is undefined when not playing from the default \
             starting position."
end

module Generous = struct
  let choice _ moves = Legal.best moves ~eval:(fun m ->
      Legal.new_position m |>
      Position.legal_moves |>
      List.fold ~init:0 ~f:(fun acc m -> match Legal.capture m with
          | Some k -> acc + Piece.Kind.value k
          | None -> acc) |>
      Option.some) |> List.random_element_exn, ()

  let name = "generous"
  let create = thunk @@ Player.create ~choice ~state:() ~name
      ~desc:"The player that tries to maximize the number of opponent \
             responses that will capture its pieces. Higher value pieces \
             are favored."
end

module Huddle = struct
  let choice _ moves = Legal.best moves ~eval:(fun m ->
      let active = Position.active @@ Legal.parent m in
      let pos = Legal.new_position m in
      let active_board = Position.board_of_color pos active in
      let king = Position.king pos in
      let king_sq = Bb.(first_set_exn (king & active_board)) in
      Position.collect_color pos active |>
      List.fold ~init:0 ~f:(fun acc (sq, _) ->
          acc - Square.chebyshev king_sq sq) |>
      Option.return) |> List.random_element_exn, ()

  let name = "huddle"
  let create = thunk @@ Player.create ~choice ~state:() ~name
      ~desc:"The player that tries to minimize the distance between its \
             pieces and its king."
end

module Max_oppt_moves = struct
  let choice _ moves = Legal.best moves ~eval:(fun m ->
      let pos = Legal.new_position m in
      Some (List.length @@ Position.legal_moves pos)) |>
                       List.random_element_exn, ()

  let name = "max-oppt-moves"
  let create = thunk @@ Player.create ~choice ~state:() ~name
      ~desc:"The player that attempts to maximize the number of \
             moves the opponent can make."
end

module Min_oppt_moves = struct
  let choice _ moves = Legal.best moves ~eval:(fun m ->
      let pos = Legal.new_position m in
      Some (-(List.length @@ Position.legal_moves pos))) |>
                       List.random_element_exn, ()

  let name = "min-oppt-moves"
  let create = thunk @@ Player.create ~choice ~state:() ~name
      ~desc:"The player that attempts to minimize the number of \
             moves the opponent can make."
end

module Opposite_color = struct
  let opposite_color active sq =
    not @@ Piece.Color.equal active @@ Square.color sq

  let choice _ moves = Legal.best moves ~eval:(fun m ->
      let active = Position.active @@ Legal.parent m in
      let pos = Legal.new_position m in
      Option.return @@ Bb.count @@
      Bb.filter ~f:(opposite_color active) @@
      Position.board_of_color pos active) |> List.random_element_exn, ()

  let name = "opposite-color"
  let create = thunk @@ Player.create ~choice ~state:() ~name
      ~desc:"The player that tries to maximize the number of pieces \
             that are on squares that are opposite of its color."
end

module Pacifist = struct
  let inf = Int.max_value

  let choice _ moves = Legal.best moves ~eval:(fun m ->
      let pos = Legal.new_position m in
      let in_check = Position.in_check pos in
      (* Give the highest possible penalty for checkmating the opponent.
         Give a similarly high penalty for checking the opponent's king.
         In all other cases, maximize the opponent's material advantage. 
         This works to prioritize moves where we avoid capturing any
         piece. *)
      let score = match Position.legal_moves pos with
        | [] when in_check -> -inf
        | _ when in_check -> inf / -2
        | _ ->
          Position.collect_active pos |>
          List.fold ~init:0 ~f:(fun material (_, k) ->
              material + Piece.Kind.value k) in
      Some score) |> List.random_element_exn, ()

  let name = "pacifist"
  let create = thunk @@ Player.create ~choice ~state:() ~name
      ~desc:"The player that avoids, in the following priority, \
             checkmating the opponent, checking the opponent, and \
             capturing pieces. Failing that, it will capture the \
             lowest-value piece possible."
end

module Random = struct
  let choice _ moves = List.random_element_exn moves, ()
  let name = "random"
  let create = thunk @@ Player.create ~choice ~state:() ~name
      ~desc:"The player that makes moves randomly."
end

module Same_color = struct
  let same_color active sq = Piece.Color.equal active @@ Square.color sq

  let choice _ moves = Legal.best moves ~eval:(fun m ->
      let active = Position.active @@ Legal.new_position m in
      let pos = Legal.new_position m in
      Option.return @@ Bb.count @@
      Bb.filter ~f:(same_color active) @@
      Position.board_of_color pos active) |> List.random_element_exn, ()

  let name = "same-color"
  let create = thunk @@ Player.create ~choice ~state:() ~name
      ~desc:"The player that tries to maximize the number of pieces \
             that are on squares of its color."
end

module Suicide_king = struct
  let choice _ moves = Legal.best moves ~eval:(fun m ->
      let active = Position.active @@ Legal.parent m in
      let pos = Legal.new_position m in
      let king = Position.king pos in
      let active_board = Position.board_of_color pos active in
      let inactive_board = Position.active_board pos in
      let k1 = Bb.(first_set_exn (king & active_board)) in
      let k2 = Bb.(first_set_exn (king & inactive_board)) in
      Some (-(Square.chebyshev k1 k2))) |> List.random_element_exn, ()

  let name = "suicide-king"
  let create = thunk @@ Player.create ~choice ~state:() ~name
      ~desc:"The player that attempts to minimize the distance between \
             both kings."
end

module Swarm = struct
  let choice _ moves =
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

  let name = "swarm"
  let create = thunk @@ Player.create ~choice ~state:() ~name
      ~desc:"The player that tries to minimize the distance between its \
             pieces and the inactive king."
end

let init =
  let once = ref false in
  fun () -> if !once then () else begin
      Players.register Alphabetical.name Alphabetical.create;
      Players.register Cccp.name Cccp.create;
      Players.register Equalizer.name Equalizer.create;
      Players.register Generous.name Generous.create;
      Players.register Huddle.name Huddle.create;
      Players.register Max_oppt_moves.name Max_oppt_moves.create;
      Players.register Min_oppt_moves.name Min_oppt_moves.create;
      Players.register Opposite_color.name Opposite_color.create;
      Players.register Pacifist.name Pacifist.create;
      Players.register Random.name Random.create;
      Players.register Same_color.name Same_color.create;
      Players.register Suicide_king.name Suicide_king.create;
      Players.register Swarm.name Swarm.create;
      once := true;
    end
