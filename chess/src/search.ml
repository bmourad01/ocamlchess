open Core_kernel
open Monads.Std

module Legal = Position.Legal

module Limits = struct
  type t = {
    depth : int;
    nodes : int option;
  }

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

let rec negamax pos ~depth ~alpha ~beta =
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
    if List.is_empty moves then
      (* Checkmate or stalemate. *)
      State.return @@ if in_check then Int.min_value else 0
    else (* Evaluate once we reach a depth of zero. We extend the search
            if a player is in check. *)
      let check_ext = Bool.to_int in_check in
      if depth + check_ext = 0
      then State.(update inc_nodes) >>| fun () -> Eval.go pos
      else let open Continue_or_stop in
        (* Search deeper into the game tree. *)
        let init = alpha and finish = State.return in
        State.List.fold_until moves ~init ~finish ~f:(fun alpha m ->
            let new_pos = Legal.new_position m in
            let depth = depth - 1 + check_ext in
            negamax new_pos ~depth ~alpha:(-beta) ~beta:(-alpha) >>| fun score ->
            let score = -score in
            (* Opponent got a better move, so prune this branch from the tree. *)
            if score >= beta then Stop beta
            (* We got a better move, so update alpha. *)
            else if score > alpha then Continue score
            else Continue alpha)

let go search = match Position.legal_moves search.root with
  | [] -> failwith "No legal moves"
  | moves -> 
    let st = State.Fields.create ~nodes:0 ~search in
    let alpha = Int.min_value and beta = Int.max_value in
    let f = let open Continue_or_stop in
      let depth = search.limits.depth - 1 in
      (* In case the search is inconclusive, pick a random move. *)
      let best = List.random_element_exn moves in
      let init = best, alpha and finish = State.return in
      State.List.fold_until moves ~init ~finish ~f:(fun (best, alpha) m ->
          let new_pos = Legal.new_position m in
          negamax new_pos ~depth ~alpha:(-beta) ~beta:(-alpha) >>| fun score ->
          let score = -score in
          if score > alpha then
            (* Stop if we've reached a maximally good move. *)
            if score = Int.max_value
            then Stop (m, score) else Continue (m, score)
          else Continue (best, alpha)) in
    Monad.State.eval f st
