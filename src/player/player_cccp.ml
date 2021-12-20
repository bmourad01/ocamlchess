open Core_kernel

module Bb = Bitboard
module Lm = Position.Legal_move

(** The player with the simplistic "checkmate, check, capture, push"
    strategy. It attempts each of these four goals in the exact order
    that you are reading them. *)
class cls ?(limits = None) () = object(self)
  inherit Player.cls ~limits ()

  method private enemy pos =
    Piece.Color.opposite @@ Position.active pos

  (* Try to checkmate the enemy king. *)
  method private checkmate = List.filter ~f:(fun mv ->
      let pos = Lm.position mv in
      match Position.legal_moves pos with
      | [] -> Position.in_check pos
      | _ :: _ -> false)

  (* Try to check the enemy king. *)
  method private check = List.filter ~f:(fun mv ->
      Lm.position mv |> Position.in_check)

  method private captured enemy pos pos' =
    let open Option.Monad_infix in
    let b = Position.board_of_color pos enemy in
    let b' = Position.board_of_color pos' enemy in
    (* Only one enemy piece could've been captured, so use an XOR. *)
    Bb.(first_set (b ^ b')) >>= Position.piece_at_square pos >>| Piece.kind

  method private piece_value (k : Piece.kind) =
    match k with
    | Pawn -> 1
    | Knight | Bishop -> 3
    | Rook -> 5
    | Queen -> 9
    | King -> 0

  (* Try to capture an enemy piece of the highest value. *)
  method private capture pos moves =
    let open Option.Monad_infix in
    let enemy = self#enemy pos in
    self#equal_eval moves ~eval:(fun mv ->
        Lm.position mv |> self#captured enemy pos >>| self#piece_value)

  (* Push a piece that gains the the largest number of controlled squares,
     with a bonus for promoting to a high-value piece. *)
  method private push pos moves =
    let active = Position.active pos in
    self#equal_eval moves ~eval:(fun mv ->
        let m, pos' = Lm.decomp mv in
        let promote_bonus =
          Move.promote m |> Option.value_map ~default:0 ~f:self#piece_value in
        let num_controlled = Bb.count @@ Position.Attacks.all pos' active
            ~ignore_same:false ~king_danger:true in
        Option.return (num_controlled + promote_bonus))

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
