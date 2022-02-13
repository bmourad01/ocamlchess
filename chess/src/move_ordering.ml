open Core_kernel

module Legal = Position.Legal

type t = < best : Position.legal; next : Position.legal option >

let capture_bonus = 40
let promote_bonus = 30

(* Most Valuable Victim/Least Valuable Attacker *)
let victims = Piece.[Pawn; Knight; Bishop; Rook; Queen]
let attackers = Piece.King :: List.rev victims
let num_attackers = List.length attackers
let num_victims = List.length victims
let mvv_lva =
  let tbl = Array.create ~len:(num_victims * num_attackers) 0 in
  List.fold victims ~init:0 ~f:(fun acc victim ->
      let i = Piece.Kind.to_int victim in
      List.fold attackers ~init:acc ~f:(fun acc attacker ->
          let j = Piece.Kind.to_int attacker in
          tbl.(i + j * num_victims) <- acc;
          acc + 1)) |> ignore;
  tbl

let create = function
  | [] -> invalid_arg "No legal moves to order"
  | moves ->
    let moves =
      List.map moves ~f:(fun m ->
          match Legal.capture m with
          | Some k ->
            let parent = Legal.parent m in
            let src = Move.src @@ Legal.move m in
            let i = Piece.Kind.to_int k in
            let p = Position.piece_at_square_exn parent src in
            let j = Piece.(Kind.to_int @@ kind p) in
            m, capture_bonus + Array.unsafe_get mvv_lva (i + j * num_victims)
          | None -> match Move.promote @@ Legal.move m with
            | Some k -> m, promote_bonus + Piece.Kind.value k
            | None -> m, 0) |>
      List.sort ~compare:(fun (_, a) (_, b) -> Int.compare b a) in
    object
      val mutable remaining = moves
      method best = fst @@ List.hd_exn moves
      method next = match remaining with
        | [] -> None
        | (m, _) :: rest ->
          remaining <- rest;
          Some m
    end
