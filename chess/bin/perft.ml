open Core_kernel
open Chess

module Legal = Position.Legal

let perft pos depth =
  let worklist = Stack.singleton (pos, depth) in
  let rec loop n = match Stack.pop worklist with
    | None -> n
    | Some (_, depth) when depth <= 0 -> loop Int64.(n + 1L)
    | Some (pos, depth) ->
      let depth = depth - 1 in
      Position.legal_moves pos |> List.iter ~f:(fun m ->
          Stack.push worklist (Legal.new_position m, depth));
      loop n in
  loop 0L

let go depth pos =
  let t = Time.now () in
  let roots = Position.legal_moves pos in
  let n =
    let depth = depth - 1 in
    List.fold roots ~init:0L ~f:(fun acc m ->
        let m = Legal.move m and pos = Legal.new_position m in
        let n = perft pos depth in
        printf "%s: %Lu\n%!" (Move.to_string m) n;
        Int64.(acc + n)) in
  let t' = Time.now () in
  let sec = Time.(Span.to_sec @@ diff t' t) in
  let nps = Float.(to_int64 (of_int64 n / (sec + epsilon_float))) in
  printf "\n%!";
  printf "Time taken: %fs\n%!" sec;
  printf "Nodes searched: %Lu\n%!" n;
  printf "Nodes per second: %Lu\n%!" nps
