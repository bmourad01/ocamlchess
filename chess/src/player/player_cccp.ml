open Core_kernel

module Bb = Bitboard
module Legal = Position.Legal

(* Try to checkmate the enemy king. *)
let checkmate = List.filter ~f:(fun mv ->
    let pos = Legal.new_position mv in
    match Position.legal_moves pos with
    | [] -> Position.in_check pos
    | _ -> false)
    
(* Try to check the enemy king. *)
let check = List.filter ~f:(fun m -> Legal.new_position m |> Position.in_check)

(* Static value assigned to each piece kind. *)
let piece_value (k : Piece.kind) = match k with
  | Pawn -> 1
  | Knight | Bishop -> 3
  | Rook -> 5
  | Queen -> 9
  | King -> 0
    
(* Try to capture an enemy piece of the highest value. *)
let capture moves = Player.best_moves moves ~eval:(fun m ->
    Legal.capture m |> Option.map ~f:(fun (k, _) -> piece_value k))

(* Push a piece that results in the the largest number of controlled
   squares. *)
let push pos moves =
  let active = Position.active pos in
  Player.best_moves moves ~eval:(fun m ->
      let pos = Legal.new_position m in
      Some (Bb.count @@ Position.Attacks.all pos active))
    
let choose pos = function
  | [] -> raise Player.No_moves
  | moves ->
    let moves = match checkmate moves with
      | (_ :: _) as moves -> moves
      | [] -> match check moves with
        | (_ :: _) as moves -> moves
        | [] -> match capture moves with
          | (_ :: _) as moves -> moves
          | [] -> push pos moves in
    List.random_element_exn moves
                                         
let create ?(limits = None) () = object
  method choose = choose
  method limits = limits
  method name = "cccp"
end
