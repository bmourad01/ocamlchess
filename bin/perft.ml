open Core_kernel
open Chess

let rec perft ?(first = false) pos depth =
  if depth <= 0 then 1L
  else
    Position.legal_moves pos |>
    List.fold ~init:0L ~f:(fun acc (m, pos') ->
        let nodes = perft pos' (depth - 1) in
        if first then begin
          printf "%s: %Ld\n%!" (Move.to_string m) nodes
        end;
        Int64.(acc + nodes))

let () =
  let depth = Int.of_string @@ Sys.argv.(1) in
  let pos =
    if Array.length Sys.argv <= 2
    then Position.start
    else Position.Fen.of_string_exn Sys.argv.(2) in
  let n = perft pos depth ~first:true in
  printf "\nNodes searched: %Ld\n%!" n
  
