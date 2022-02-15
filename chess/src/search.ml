open Core_kernel
open Monads.Std

module Legal = Position.Legal

module Limits = struct
  type t = {
    depth : int;
    nodes : int option;
  } [@@deriving fields]

  let create ?(nodes = None) ~depth () =
    if depth < 0 then invalid_argf "Invalid depth limit %d" depth ()
    else match nodes with
      | Some n when n < 1 -> invalid_argf "Invalid node limit %d" n ()
      | _ -> Fields.create ~depth ~nodes

  let is_max_nodes n = function
    | {nodes = Some nodes; _} -> n >= nodes
    | _ -> false
end

type limits = Limits.t

type t = {
  limits : limits;
  root : Position.t;
  transpositions : int Int64.Map.t;
} [@@deriving fields]

module Result = struct
  type t = {
    best_move : Position.legal;
    score : int;
    nodes_searched : int;
  } [@@deriving fields]
end

type result = Result.t
type search = t

let create = Fields.create

module State = struct
  module T = struct
    type t = {
      nodes : int;
      search : search;
    } [@@deriving fields]

    let inc_nodes st = {st with nodes = st.nodes + 1}
  end

  include T
  include Monad.State.Make(T)(Monad.Ident)

  module List = struct
    include List

    let fold_until =
      let open Continue_or_stop in
      let rec aux acc ~f ~finish = function
        | [] -> finish acc
        | x :: xs -> f acc x >>= function
          | Continue y -> aux y xs ~f ~finish
          | Stop z -> return z in
      fun l ~init -> aux init l
  end
end

open State.Syntax

let check_threefold_repetition search pos =
  Position.hash pos |> Map.find search.transpositions |>
  Option.value_map ~default:false ~f:(fun n -> n >= 3)

(* Move ordering is critical for optimizing the performance of alpha-beta
   pruning. We use some heuristics to determine which moves are likely
   to be the best, and then search those first, hoping that the worse
   moves get pruned more effectively. *)
module Ordering = struct
  let capture_bonus = 40
  let promote_bonus = 30

  let victims = Piece.[Pawn; Knight; Bishop; Rook; Queen]
  let attackers = Piece.King :: List.rev victims
  let num_attackers = List.length attackers
  let num_victims = List.length victims

  (* Most Valuable Victim/Least Valuable Attacker *)
  let mvv_lva =
    let tbl = Array.create ~len:(num_victims * num_attackers) 0 in
    List.fold victims ~init:0 ~f:(fun acc victim ->
        let i = Piece.Kind.to_int victim in
        List.fold attackers ~init:acc ~f:(fun acc attacker ->
            let j = Piece.Kind.to_int attacker in
            tbl.(i + j * num_victims) <- acc;
            acc + 1)) |> ignore;
    tbl

  let sort =
    Legal.sort ~eval:(fun m ->
        let capture =
          Legal.capture m |> Option.value_map ~default:0 ~f:(fun k ->
              let parent = Legal.parent m in
              let src = Move.src @@ Legal.move m in
              let i = Piece.Kind.to_int k in
              let p = Position.piece_at_square_exn parent src in
              let j = Piece.(Kind.to_int @@ kind p) in
              let v = Array.unsafe_get mvv_lva (i + j * num_victims) in
              capture_bonus + v) in
        let promote =
          Legal.move m |> Move.promote |>
          Option.value_map ~default:0 ~f:(fun k ->
              promote_bonus + Piece.Kind.value k) in
        capture + promote)
end

let neg = Fn.compose State.return Int.neg

(* Quescience search is used when we reach our maximum depth for the main
   search. The goal is then to keep searching only "noisy" positions, until
   we reach one that is "quiet", and then return our evaluation. *)
module Quescience = struct
  let rec new_node pos ~alpha ~beta =
    State.(gets nodes) >>= fun nodes ->
    State.(gets search) >>= fun search ->
    if Limits.is_max_nodes nodes search.limits
    then State.return 0
    else
      let moves = Position.legal_moves pos in
      let in_check = Position.in_check pos in
      if List.is_empty moves
      then State.return @@ if in_check then Int.min_value else 0
      else go pos moves ~alpha ~beta

  and go pos moves ~alpha ~beta =
    State.(update inc_nodes) >>= fun () ->
    let score = Eval.go pos in
    let moves = List.filter moves ~f:(fun m ->
        Option.is_some @@ Legal.capture m ||
        Option.is_some @@ Move.promote @@ Legal.move m) in
    if List.is_empty moves then State.return score
    else
      let alpha = max score alpha in
      if alpha >= beta then State.return score
      else let open Continue_or_stop in
        let init = score, alpha in
        let finish = Fn.compose State.return fst in
        Ordering.sort moves |>
        State.List.fold_until ~init ~finish ~f:(fun acc m ->
            let score, alpha = acc in
            let pos' = Legal.new_position m in
            new_node pos' ~alpha:(-beta) ~beta:(-alpha) >>= neg >>| fun s ->
            let score = max score s in
            let alpha = max alpha score in
            if alpha >= beta then Stop score
            else Continue (score, alpha))
end

type accum = {
  best_move : Position.legal;
  score : int;
  alpha : int;
  full_window : bool;
}

(* Principal variation search can lead to more pruning if the move ordering
   was (more or less) correct.

   See: https://en.wikipedia.org/wiki/Principal_variation_search
*)
let rec pvs acc pos ~depth ~beta =
  let alpha = if acc.full_window then -beta else (-acc.alpha) - 1 in
  let f = negamax pos ~depth ~beta:(-acc.alpha) in
  let finish s = State.return @@ max acc.score s in
  f ~alpha >>= neg >>= function
  | s when acc.full_window -> finish s
  | s when s > acc.alpha && s < beta -> f ~alpha:(-beta) >>= finish
  | s -> finish s

and negamax pos ~depth ~alpha ~beta =
  State.(gets nodes) >>= fun nodes ->
  State.(gets search) >>= fun search ->
  (* Check if we reached the search limits, as well as for positions
     where a player may claim a draw. *)
  if Limits.is_max_nodes nodes search.limits
  || check_threefold_repetition search pos
  || Position.halfmove pos >= 100 then State.return 0
  else (* Continue the search. *)
    let moves = Position.legal_moves pos in
    let in_check = Position.in_check pos in
    if List.is_empty moves
    then State.return @@ if in_check then Int.min_value else 0
    else (* Evaluate once we reach a depth of zero. We extend the search
            if a player is in check. *)
      let check_ext = Bool.to_int in_check in
      if depth + check_ext = 0 then Quescience.go pos moves ~alpha ~beta
      else let open Continue_or_stop in
        (* Search deeper into the game tree. *)
        let moves = Ordering.sort moves in
        let init = {
          best_move = List.hd_exn moves;
          score = Int.min_value;
          alpha;
          full_window = true;
        } in
        let finish acc = State.return acc.score in
        State.List.fold_until moves ~init ~finish ~f:(fun acc m ->
            let pos' = Legal.new_position m in
            let depth = depth - 1 + check_ext in
            pvs acc pos' ~depth ~beta >>| fun score ->
            let alpha = max alpha score in
            if alpha >= beta then Stop score
            else if score <= acc.alpha then Continue {acc with score; alpha}
            else Continue {
                best_move = m;
                score;
                alpha;
                full_window = false;
              })

(* The search we start from the root position. *)
let rootmax moves depth =
  let open Continue_or_stop in
  let beta = Int.max_value in
  let init = {
    best_move = List.hd_exn moves;
    score = Int.min_value;
    alpha = Int.min_value;
    full_window = true;
  } in
  let finish acc = State.return (acc.best_move, acc.score) in
  State.List.fold_until moves ~init ~finish ~f:(fun acc m ->
      let pos' = Legal.new_position m in
      pvs acc pos' ~depth ~beta >>| fun score ->
      if score <= acc.alpha then Continue {acc with score}
      else if score = Int.max_value then Stop (m, score)
      else Continue {
          best_move = m;
          score;
          alpha = score;
          full_window = false;
        }) 

let go search = match Position.legal_moves search.root with
  | [] -> invalid_arg "No legal moves"
  | moves -> 
    let moves = Ordering.sort moves in
    let depth = search.limits.depth - 1 in
    let (best_move, score), st =
      Monad.State.run (rootmax moves depth) @@
      State.Fields.create ~nodes:0 ~search in
    Result.Fields.create ~best_move ~score ~nodes_searched:st.nodes
