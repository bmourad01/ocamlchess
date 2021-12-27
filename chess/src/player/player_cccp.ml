open Core_kernel

module Bb = Bitboard
module Legal = Position.Legal_move
module Legals = Position.Legal_moves

(* Try to checkmate the enemy king. *)
let checkmate = List.filter ~f:(fun mv ->
    let pos = Legal.position mv in
    match Legals.moves @@ Position.legal_moves pos with
    | [] -> Position.in_check pos
    | _ -> false)
    
(* Try to check the enemy king. *)
let check = List.filter ~f:(fun mv -> Legal.position mv |> Position.in_check)

(* Get the piece that was captured as a result of the move (if any). *)
let captured enemy pos pos' =
  let open Option.Monad_infix in
  let b = Position.board_of_color pos enemy in
  let b' = Position.board_of_color pos' enemy in
  (* Only one enemy piece could've been captured, so use an XOR. *)
  Bb.(first_set (b ^ b')) >>= Position.piece_at_square pos >>| Piece.kind

(* Static value assigned to each piece kind. *)
let piece_value (k : Piece.kind) = match k with
  | Pawn -> 1
  | Knight | Bishop -> 3
  | Rook -> 5
  | Queen -> 9
  | King -> 0
    
(* Try to capture an enemy piece of the highest value. *)
let capture pos moves =
  let open Option.Monad_infix in
  let enemy = Position.enemy pos in
  Player.best_moves moves ~eval:(fun mv ->
      let pos' = Legal.position mv in
      captured enemy pos pos' >>| piece_value)

(* Push a piece that results in the the largest number of controlled
   squares. *)
let push pos moves =
  let active = Position.active pos in
  Player.best_moves moves ~eval:(fun mv ->
      let pos = Legal.position mv in
      Some (Bb.count @@ Position.Attacks.all pos active))
    
let choose legals = match Legals.decomp legals with
  | [], _ -> raise Player.No_moves
  | moves, pos ->
    let moves = match checkmate moves with
      | (_ :: _) as moves -> moves
      | [] -> match check moves with
        | (_ :: _) as moves -> moves
        | [] -> match capture pos moves with
          | (_ :: _) as moves -> moves
          | [] -> push pos moves in
    List.random_element_exn moves
                                         
let create ?(limits = None) () = object
  method choose = choose
  method limits = limits
  method name = "cccp"
end
