open Core_kernel
open Chess

module Child = Position.Child

let rec perft pos depth =
  if depth <= 0 then 1L
  else
    let depth = depth - 1 in
    Position.legal_moves pos |> List.fold ~init:0L ~f:(fun acc m ->
        let pos = Child.self @@ Position.Unsafe.make_move pos m in
        Int64.(acc + perft pos depth))

let run depth pos =
  let t = Time.now () in
  let roots = Position.children pos in
  let n =
    let depth = depth - 1 in
    List.fold roots ~init:0L ~f:(fun acc m ->
        let pos = Child.self m in
        let m = Child.move m in
        let n = perft pos depth in
        Format.printf "%a: %Lu\n%!" Move.pp m n;
        Int64.(acc + n)) in
  let t' = Time.now () in
  let sec = Time.(Span.to_sec @@ diff t' t) in
  let nps = Float.(to_int64 (of_int64 n / (sec + epsilon_float))) in
  Format.printf "\n%!";
  Format.printf "Time taken: %fs\n%!" sec;
  Format.printf "Nodes searched: %Lu\n%!" n;
  Format.printf "Nodes per second: %Lu\n%!" nps
