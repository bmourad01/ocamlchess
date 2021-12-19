open Core_kernel

module Bb = Bitboard

(* The "checkmate, check, capture, push" strategy. *)
class cls ?(limits = None) () = object(self)
  inherit Player.cls ~limits ()

  method private enemy pos =
    Piece.Color.opposite @@ Position.active pos
  
  method private in_check pos =
    let active_board = Position.active_board pos in
    let king = Position.king pos in
    let attacks =
      Position.Attacks.all pos (self#enemy pos) ~ignore_same:true in
    Bb.((active_board & king & attacks) <> empty)

  (* Try to checkmate the enemy king. *)
  method private checkmate moves =
    List.filter moves ~f:(fun (_, pos) ->
        match Position.legal_moves pos with
        | _ :: _ -> false
        | [] -> self#in_check pos)

  (* Try to check the enemy king. *)
  method private check moves =
    List.filter moves ~f:(fun (_, pos) -> self#in_check pos)

  method private enemy_count enemy pos =
    Bb.count @@ Position.board_of_color pos enemy

  method private captured enemy pos pos' =
    let open Option.Monad_infix in
    let b = Position.board_of_color pos enemy in
    let b' = Position.board_of_color pos' enemy in
    Bb.(find b ~f:(fun sq -> not (sq @ b'))) >>=
    Position.piece_at_square pos

  method private piece_value p = match Piece.kind p with
    | Pawn -> 1
    | Knight | Bishop -> 3
    | Rook -> 5
    | Queen -> 9
    | King -> 0

  (* Try to capture an enemy piece of the highest value. *)
  method private capture pos moves =
    let open Option.Monad_infix in
    let enemy = self#enemy pos in
    List.filter_map moves ~f:(fun ((_, pos') as move) ->
        self#captured enemy pos pos' >>| fun p ->
        (move, self#piece_value p)) |>
    List.sort ~compare:(fun (_, v) (_, v') -> Int.compare v' v) |>
    List.fold_until ~init:([], 0) ~finish:fst ~f:(fun (acc, v') (m, v) ->
        match acc with
        | [] -> Continue (m :: acc, v)
        | _ -> if v' > v then Stop acc else Continue (m :: acc, v'))

  (* Push a piece that gains the the largest number of controlled squares. *)
  method private push pos moves =
    let active = Position.active pos in
    List.map moves ~f:(fun ((_, pos') as m) ->
        let attacks =
          Position.Attacks.all pos' active
            ~ignore_same:false ~king_danger:true in
        (m, Bb.count attacks)) |>
    List.sort ~compare:(fun (_, a) (_, a') -> Int.compare a' a) |>
    List.fold_until ~init:([], 0) ~finish:fst ~f:(fun (acc, a') (m, a) ->
        match acc with
        | [] -> Continue (m :: acc, a)
        | _ -> if a' > a then Stop acc else Continue (m :: acc, a'))
  
  method move pos = match Position.legal_moves pos with
    | [] -> raise Player.No_moves
    | moves -> match self#checkmate moves with
      | (_ :: _) as moves -> List.random_element_exn moves
      | [] -> match self#check moves with
        | (_ :: _) as moves -> List.random_element_exn moves
        | [] -> match self#capture pos moves with
          | (_ :: _) as moves -> List.random_element_exn moves
          | [] -> self#push pos moves |> List.random_element_exn

  method name = "cccp"
end


