open Core_kernel
open Chess

let rec perft ?(first = false) pos depth =
  if depth <= 0 then 1L
  else
    Position.legal_moves pos |>
    Position.Legal_moves.moves |>
    List.fold ~init:0L ~f:(fun acc mv ->
        let m, pos = Position.Legal_move.decomp mv in
        let nodes = perft pos (depth - 1) in
        if first then begin
          printf "%s: %Ld\n%!" (Move.to_string m) nodes
        end;
        Int64.(acc + nodes))

let go depth pos =
  let n = perft pos depth ~first:true in
  printf "\nNodes searched: %Ld\n%!" n
