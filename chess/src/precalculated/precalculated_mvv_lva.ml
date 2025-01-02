open Core_kernel [@@warning "-D"]

let mvv_lva_tbl =
  let victims = Piece.[Pawn; Knight; Bishop; Rook; Queen; King] in
  let attackers = List.rev victims in
  let n = Piece.Kind.count in
  let len = n * n in
  let tbl = Array.create ~len 0 in
  List.fold victims ~init:0 ~f:(fun init victim ->
      let i = Piece.Kind.to_int victim in
      List.fold attackers ~init ~f:(fun value attacker ->
          let j = Piece.Kind.to_int attacker in
          tbl.(i + j * n) <- value;
          value + 1)) |> ignore;
  tbl

let[@inline] mvv_lva victim attacker =
  let i = Piece.Kind.to_int victim in
  let j = Piece.Kind.to_int attacker in
  Array.unsafe_get mvv_lva_tbl (i + j * Piece.Kind.count)
