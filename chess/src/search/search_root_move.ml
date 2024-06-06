open Core_kernel [@@warning "-D"]
open Search_common

type t = {
  mutable score      : int;
  mutable prev_score : int;
  mutable avg_score  : int;
  mutable seldepth   : int;
  mutable bound      : Uci.Send.Info.bound;
  pv                 : Child.t Oa.t;
}

let pv_size = max_ply + 2

let create m =
  let t = {
    score = -inf;
    prev_score = -inf;
    avg_score = -inf;
    seldepth = 1;
    bound = Exact;
    pv = Oa.create ~len:pv_size;
  } in
  Oa.unsafe_set_some t.pv 0 m;
  t

let same_move t m = same_move m @@ Oa.unsafe_get_some_exn t.pv 0

let order x y = match compare y.score x.score with
  | n when n <> 0 -> n
  | _ -> match compare y.prev_score x.prev_score with
    | n when n <> 0 -> n
    | _ -> compare y.avg_score x.avg_score

let real_score t =
  let updated = t.score <> (-inf) in
  let score =
    if updated then t.score
    else if t.prev_score <> (-inf) then t.prev_score
    else 0 in
  score, updated

let update_avg t score =
  let a = t.avg_score in
  t.avg_score <- if a = (-inf) then score else (2 * score + a) / 3
