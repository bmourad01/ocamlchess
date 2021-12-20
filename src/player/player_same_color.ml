open Core_kernel

module Bb = Bitboard

(** The player that prefers to put its pieces on squares of its color,
    such that the number of pieces on these squares is maximized. Ties are
    broken randomly. *)
class cls ?(limits = None) () = object(self)
  inherit Player.cls ~limits ()

  method private same_color active sq =
    let rank, file = Square.decomp sq in
    match (active : Piece.color) with
    | White -> (rank land 1) <> (file land 1)
    | Black -> (rank land 1) = (file land 1)

  method private board active = match (active : Piece.color) with
    | White -> Position.white
    | Black -> Position.black
  
  method move pos = match Position.legal_moves pos with
    | [] -> raise Player.No_moves
    | moves ->
      let active = Position.active pos in
      let board = self#board active in
      self#equal_eval moves ~eval:(fun (_, pos) ->
          Option.return @@ Bb.count @@
          Bb.filter (board pos) ~f:(self#same_color active)) |>
      List.random_element_exn
  
  method name = "same-color"
end
