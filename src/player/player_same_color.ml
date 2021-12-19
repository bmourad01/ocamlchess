open Core_kernel

module Bb = Bitboard

(** The player that prefers to put its pieces on squares of its color,
    such that the number of pieces on these squares is maximized. Ties are
    broken randomly. *)
class cls ?(limits = None) () = object
  inherit Player.cls ~limits ()

  method move pos = match Position.legal_moves pos with
    | [] -> raise Player.No_moves
    | moves ->
      let active = Position.active pos in
      let board pos = match active with
        | White -> Position.white pos
        | Black -> Position.black pos in
      let same_color sq =
        let rank, file = Square.decomp sq in
        match active with
        | White -> (rank land 1) <> (file land 1)
        | Black -> (rank land 1) = (file land 1) in
      List.map moves ~f:(fun (m, pos) ->
          let n = Bb.(count @@ filter (board pos) ~f:same_color) in
          (m, pos, n)) |>
      List.sort ~compare:(fun (_, _, n) (_, _, n') -> Int.compare n' n) |>
      List.fold_until ~init:([], 0) ~finish:fst
        ~f:(fun (acc, n') (m, pos, n) ->
            match acc with
            | [] -> Continue ((m, pos) :: acc, n)
            | _ -> if n' > n then Stop acc else Continue ((m, pos) :: acc, n')) |>
      List.random_element_exn
end
