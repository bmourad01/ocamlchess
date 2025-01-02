(* Generation of sliding attack patterns. *)

open Core_kernel [@@warning "-D"]
open Precalculated_common

let diagonal, orthogonal =
  (* Computes the bitboards for diagonal and orthogonal attacks, given a
     starting square and the set of occupied squares. *)
  let gen dirs i occupied =
    let open Bb in
    let occupied = of_int64 occupied in
    List.fold dirs ~init:empty ~f:(fun acc (tbl, f) ->
        let ray = tbl.(i) in
        let acc = acc + ray in
        match to_int64 (ray & occupied) with
        | 0L -> acc
        | ray' -> acc - tbl.(f ray')) in
  Mask.Tbl.(gen [(neast, lsb); (nwest, lsb); (seast, msb); (swest, msb)],
            gen [(east,  lsb); (west,  msb); (north, lsb); (south, msb)])

(* Generate the occupied squares for a particular mask and index. *)
let blockers idx mask =
  let n = Bb.count mask in
  let blockers = ref 0L in
  let mask = ref @@ Bb.to_int64 mask in
  for i = 0 to n - 1 do
    let m = !mask in
    if idx land (1 lsl i) <> 0 then
      blockers := Int64.(!blockers lor (one lsl ctz m));
    mask := Int64.(m land pred m);
  done;
  !blockers

(* Compute the index into the hash table. *)
let[@inline] hash occupied magic shift =
  Int64.((occupied * magic) lsr shift |> to_int_trunc)

(* Populate the hash tables for bishop and rook moves. *)
let bishop, rook =
  let go len shift mask magic gen =
    let tbl = Array.create Bb.empty ~len:(Square.count * len) in
    for i = 0 to Square.count - 1 do
      let shift = shift.(i) in
      let mask  = mask.(i)  in
      let magic = magic.(i) in
      for j = 0 to 1 lsl shift do
        let occupied = blockers j mask in
        let hash = hash occupied magic (64 - shift) in
        tbl.(i + hash * Square.count) <- gen i occupied
      done
    done;
    tbl in
  go 1024 Shift.diagonal Mask.Tbl.diagonal Magic.bishop diagonal,
  go 4096 Shift.orthogonal Mask.Tbl.orthogonal Magic.rook orthogonal
