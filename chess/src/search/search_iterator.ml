(* We use an iterator object that incrementally applies selection sort to
   the array. In the context of alpha-beta pruning, we may not actually
   visit all the moves for a given position, so it makes no sense to waste
   time sorting the entire thing. *)

open Core_kernel [@@warning "-D"]
open Search_common

type t = {
  mutable i : int;
  length    : int;
  moves     : (Child.t * int) array;
}

let empty = {
  i = 0;
  length = 0;
  moves = [||];
}

let create moves = {
  i = 0;
  length = Array.length moves;
  moves;
}

let next it =
  let c = it.i in
  let n = it.length in
  if c < n then
    let best_score = ref (-inf) in
    let best_index = ref c in
    let moves = it.moves in
    for i = c to n - 1 do
      let _, score = Array.unsafe_get moves i in
      if score > !best_score then begin
        best_score := score;
        best_index := i;
      end
    done;
    let i = !best_index in
    let result = Array.unsafe_get moves i in
    if i > c then
      Array.unsafe_get moves c |>
      Array.unsafe_set moves i;
    it.i <- c + 1;
    Uopt.some result
  else Uopt.none

let[@specialise] rec fold_until it ~init ~finish ~f =
  let open Continue_or_stop in
  let x = next it in
  if Uopt.is_some x then match f init @@ Uopt.unsafe_value x with
    | Continue init -> fold_until it ~init ~finish ~f
    | Stop z -> z
  else finish init
