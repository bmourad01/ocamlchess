open Core_kernel
open Chess

module Legals = Position.Legal_moves

let rec perft ?(first = false) pos depth =
  if depth <= 0 then 1L
  else
    Position.legal_moves pos |> Legals.moves |>
    List.fold ~init:0L ~f:(fun acc mv ->
        let m, pos = Position.Legal_move.decomp mv in
        let nodes = perft pos (depth - 1) in
        if first then begin
          printf "%s: %Ld\n%!" (Move.to_string m) nodes
        end;
        Int64.(acc + nodes))

let go depth pos =
  let t = Time.now () in
  let n = perft pos depth ~first:true in
  let t' = Time.now () in
  let sec = Time.(Span.to_sec @@ diff t' t) in
  let nps = Float.to_int64 (Float.of_int64 n /. sec) in
  let istr = Int64.to_string_hum ~delimiter:',' in
  printf "\n%!";
  printf "Time taken: %fs\n%!" sec;
  printf "Nodes searched: %s\n%!" @@ istr n;
  printf "Nodes per second: %s\n%!" @@ istr nps
