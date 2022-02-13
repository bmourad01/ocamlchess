open Core_kernel
open Monads.Std

module Bb = Bitboard
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

let create = Fields.create

module State = struct
  module T = struct
    type t = {
      nodes : int;
    } [@@deriving fields]

    let inc_nodes st = {nodes = st.nodes + 1}
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

let check_limits search =
  State.(gets nodes) >>| Fn.flip Limits.is_max_nodes search.limits

let check_threefold_repetition search pos =
  Position.hash pos |> Map.find search.transpositions |>
  Option.value_map ~default:false ~f:(fun n -> n >= 3)

let check_fifty_move pos = Position.halfmove pos >= 100

let rec negamax search pos depth alpha beta =
  (* Check if we reached the search limits. *)
  check_limits search >>= function
  | true -> State.return 0 | false ->
    (* Check for positions where a player may claim a draw. *)
    if check_threefold_repetition search pos
    || check_fifty_move pos then State.return 0
    else (* Continue the search. *)
      let moves = Position.legal_moves pos in
      let in_check = Position.in_check pos in
      if List.is_empty moves then
        (* Checkmate or stalemate. *)
        State.return @@ if in_check then Int.min_value else 0
      else (* Evaluate once we reach a depth of zero. We extend the search
              if a player is in check. *)
        let check_ext = Bool.to_int in_check in
        if depth + check_ext = 0 then eval search pos
        else let open Continue_or_stop in
          (* Search deeper into the game tree. *)
          let init = alpha and finish = State.return in
          State.List.fold_until moves ~init ~finish ~f:(fun alpha m ->
              let new_pos = Legal.new_position m in
              let depth = depth - 1 + check_ext in
              (* Opponent's response, so we negate it. *)
              negamax search new_pos depth (-beta) (-alpha) >>| fun score ->
              let score = -score in
              if score >= beta then Stop beta
              else if score > alpha then Continue score
              else Continue alpha)

and eval _search pos =
  State.(update inc_nodes) >>| fun () ->
  (* Stupid evaluation function for now. We just calculate the material
     imbalance from the active player's perspective. *)
  let active = Position.active_board pos in
  let inactive = Position.inactive_board pos in
  let pawn = Position.pawn pos in
  let knight = Position.knight pos in
  let bishop = Position.bishop pos in
  let rook = Position.rook pos in
  let queen = Position.queen pos in
  let material =
    let pawn_val = Piece.Kind.value Pawn in
    let knight_val = Piece.Kind.value Knight in
    let bishop_val = Piece.Kind.value Bishop in
    let rook_val = Piece.Kind.value Rook in
    let queen_val = Piece.Kind.value Queen in
    (Bb.(count (pawn & active)) - Bb.(count (pawn & inactive))) * pawn_val +
    (Bb.(count (knight & active)) - Bb.(count (knight & inactive))) * knight_val +
    (Bb.(count (bishop & active)) - Bb.(count (bishop & inactive))) * bishop_val +
    (Bb.(count (rook & active)) - Bb.(count (rook & inactive))) * rook_val +
    (Bb.(count (queen & active)) - Bb.(count (queen & inactive))) * queen_val in
  material

let go search = match Position.legal_moves search.root with
  | [] -> failwith "No legal moves"
  | moves -> 
    let st = State.Fields.create ~nodes:0 in
    let alpha = Int.min_value and beta = Int.max_value in
    let f = let open Continue_or_stop in
      let depth = search.limits.depth - 1 in
      let init = List.hd_exn moves, alpha and finish = State.return in
      State.List.fold_until moves ~init ~finish ~f:(fun (best, alpha) m ->
          let new_pos = Legal.new_position m in
          negamax search new_pos depth (-beta) (-alpha) >>| fun score ->
          let score = -score in
          if score >= beta then Stop (best, beta)
          else if score > alpha then
            (* Stop if we've reached a maximally good move. *)
            if score = Int.max_value
            then Stop (m, score) else Continue (m, score)
          else Continue (best, alpha)) in
    Monad.State.eval f st
