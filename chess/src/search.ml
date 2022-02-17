open Core_kernel
open Monads.Std

module Bb = Bitboard
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

(* Transposition table for caching search results. *)
module Tt = struct
  type bound = Lower | Upper | Exact

  type entry = {
    depth : int;
    score : int;
    best : Position.legal;
    bound : bound;
  }

  type t = (int64, entry) Hashtbl.t

  let create () = Hashtbl.create (module Int64)

  let set tt pos ~depth ~score ~best ~bound =
    let entry = {depth; score; best; bound} in
    Hashtbl.set tt ~key:(Position.hash pos) ~data:entry

  (* Check for a previous evaluation of the position at a comparable depth.
     We either have an exact evaluation, or we can reduce the search
     window. *)
  let lookup tt ~pos ~depth ~alpha ~beta =
    match Hashtbl.find tt @@ Position.hash pos with
    | Some {depth = depth'; score; bound; _} when depth' >= depth -> begin
        match bound with
        | Lower ->
          let beta = min beta score in
          if alpha >= beta then First score
          else Second (alpha, beta)
        | Upper ->
          let alpha = max alpha score in
          if alpha >= beta then First score
          else Second (alpha, beta)
        | Exact -> First score
      end
    | _ -> Second (alpha, beta)
end

(* Our state for the entirety of the search. *)
module State = struct
  module T = struct
    type t = {
      nodes : int;
      search : search;
      tt : Tt.t;
    } [@@deriving fields]

    let create ?(tt = Tt.create ()) search =
      {nodes = 0; search; tt}

    (* Start a new search while reusing the transposition table. *)
    let new_iter st = {st with nodes = 0}

    (* Increment the number of nodes we've evaluated. *)
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

let inf = Int.max_value

(* Avoid playing any repetition. *)
let check_repetition search pos =
  Position.hash pos |> Map.find search.transpositions |>
  Option.value_map ~default:false ~f:(fun n -> n > 0)

(* Move ordering is critical for optimizing the performance of alpha-beta
   pruning. We use some heuristics to determine which moves are likely
   to be the best, and then search those first, hoping that the worse
   moves get pruned more effectively. *)
module Ordering = struct
  let capture_bonus = 40
  let promote_bonus = 30
  let pawn_control_penalty = -35

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

  (* Check if a particular move was part of the principal variation. *)
  let is_best pos tt =
    let best =
      Position.hash pos |> Hashtbl.find tt |>
      Option.bind ~f:(fun (entry : Tt.entry) ->
          (* Only allow exact scores. *)
          match entry.bound with
          | Exact -> Some entry.best
          | _ -> None) in
    fun m -> Option.exists best ~f:(fun best ->
        Position.same_hash (Legal.parent m) pos &&
        Position.same_hash (Legal.new_position m) (Legal.new_position best))

  let sort ?(quescience = false) ~pos ~tt =
    let best = if quescience then fun _ -> false else is_best pos tt in
    let inactive = Position.inactive pos in
    let pawn_att = Position.Attacks.pawn pos inactive in
    Legal.sort ~eval:(fun m ->
        if best m then inf
        else
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
          let pawn =
            let src = Move.src @@ Legal.move m in
            let parent = Legal.parent m in
            match Position.piece_at_square_exn parent src with
            | p when Piece.is_pawn p -> 0
            | _ ->
              let dst = Move.dst @@ Legal.move m in
              if Bb.(dst @ pawn_att) then pawn_control_penalty else 0 in
          capture + promote + pawn)
end

let negm = Fn.compose State.return Int.neg

(* Quescience search is used when we reach our maximum depth for the main
   search. The goal is then to keep searching only "noisy" positions, until
   we reach one that is "quiet", and then return our evaluation. *)
let rec quescience pos ~alpha ~beta =
  State.(gets nodes) >>= fun nodes ->
  State.(gets search) >>= fun search ->
  if Limits.is_max_nodes nodes search.limits
  then State.return 0
  else
    let moves = Position.legal_moves pos in
    let in_check = Position.in_check pos in
    if List.is_empty moves
    then State.return @@ if in_check then -inf else 0
    else quescience' pos moves ~alpha ~beta

and quescience' pos moves ~alpha ~beta =
  State.(update inc_nodes) >>= fun () ->
  let score = Eval.go pos in
  let moves = List.filter moves ~f:(fun m ->
      Option.is_some @@ Legal.capture m ||
      Option.is_some @@ Move.promote @@ Legal.move m) in
  if score >= beta then State.return beta
  else let open Continue_or_stop in
    let alpha = max score alpha in
    let finish = State.return in
    State.(gets tt) >>= fun tt ->
    Ordering.sort moves ~pos ~tt ~quescience:true |>
    State.List.fold_until ~init:alpha ~finish ~f:(fun alpha m ->
        let pos' = Legal.new_position m in
        quescience pos' ~alpha:(-beta) ~beta:(-alpha) >>=
        negm >>| fun score -> if score >= beta then Stop beta
        else Continue (max score alpha))

(* Accumulates the results of searching one ply. *)
type ply = {
  mutable best : Position.legal;
  mutable alpha : int;
  mutable full_window : bool;
}

let new_ply ?(alpha = -inf) moves = {
  best = List.hd_exn moves;
  alpha;
  full_window = true;
}

let better ply m score =
  ply.best <- m;
  ply.alpha <- score;
  ply.full_window <- false

(* Principal variation search can lead to more pruning if the move ordering
   was (more or less) correct.

   See: https://en.wikipedia.org/wiki/Principal_variation_search
*)
let rec pvs pos ply ~depth ~beta =
  let f alpha = negamax pos ~depth ~alpha ~beta:(-ply.alpha) >>= negm in
  let alpha = if ply.full_window then -beta else (-ply.alpha) - 1 in
  f alpha >>= function
  | score when ply.full_window -> State.return score
  | score when score > ply.alpha && score < beta -> f (-beta)
  | score -> State.return score

(* Search from a new position. *)
and negamax pos ~depth ~alpha ~beta =
  State.(gets nodes) >>= fun nodes ->
  State.(gets search) >>= fun search ->
  (* Check if we reached the search limits, as well as for positions
     where a player may claim a draw. *)
  if Limits.is_max_nodes nodes search.limits
  || check_repetition search pos
  || Position.halfmove pos >= 100 then State.return 0
  else 
    let alpha' = alpha in
    (* Find if we evaluated this position previously. *)
    State.(gets tt) >>= fun tt ->
    match Tt.lookup tt ~pos ~depth ~alpha ~beta with
    | First score -> State.return score
    | Second (alpha, beta) ->
      let moves = Position.legal_moves pos in
      let in_check = Position.in_check pos in
      if List.is_empty moves
      then State.return @@ if in_check then -inf else 0
      else
        (* Extend the depth limit if we're in check. *)
        let check_ext = Bool.to_int in_check in
        if depth + check_ext = 0 then quescience' pos moves ~alpha ~beta
        else let open Continue_or_stop in
          let moves = Ordering.sort moves ~pos ~tt in
          let depth' = depth - 1 + check_ext in
          let ply = new_ply moves ~alpha in
          let finish () = State.return (ply.alpha, false) in
          State.List.fold_until moves ~init:() ~finish ~f:(fun () m ->
              let pos' = Legal.new_position m in
              pvs pos' ply ~depth:depth' ~beta >>| fun score ->
              if score >= beta then begin
                Tt.(set tt pos ~depth ~score ~best:m ~bound:Lower);
                Stop (beta, true)
              end else begin
                if score > ply.alpha then better ply m score;
                Continue ()
              end) >>| fun (score, cutoff) ->
          (* Update the transposition table and return the score. *)
          if not cutoff then begin
            let bound = if ply.alpha <= alpha' then Tt.Upper else Tt.Exact in
            Tt.(set tt pos ~depth ~score ~best:ply.best ~bound);
          end;
          score

(* The search we start from the root position. *)
let rootmax moves depth =
  let open Continue_or_stop in
  State.(gets search) >>= fun search ->
  State.(gets tt) >>= fun tt ->
  let pos = search.root in
  let moves = Ordering.sort moves ~pos ~tt in
  let ply = new_ply moves in
  let beta = inf in
  let finish () = State.return ply.alpha in
  let depth' = depth - 1 in
  State.List.fold_until moves ~init:() ~finish ~f:(fun () m ->
      let pos' = Legal.new_position m in
      pvs pos' ply ~depth:depth' ~beta >>= fun score ->
      if score >= beta then begin
        (* If we hit beta cutoff at the root, then we've found a maximally
           good move for us. *)
        ply.best <- m;
        State.return @@ Stop beta
      end else State.(gets nodes) >>| fun nodes ->
        if Limits.is_max_nodes nodes search.limits then Stop ply.alpha
        else begin
          if score > ply.alpha then better ply m score;
          Continue ()
        end) >>| fun score ->
  (* Update the transposition table and return the results. *)
  let best = ply.best in
  Tt.(set tt pos ~depth ~score ~best ~bound:Exact);
  best, score

(* Use iterative deepening to optimize the search. This relies on previous
   evaluations stored in the transposition table to prune more nodes with
   each successive iteration. *)
let rec iterdeep ?(i = 1) st ~moves =
  let (best_move, score), st = Monad.State.run (rootmax moves i) st in
  let i = i + 1 in
  if i > st.search.limits.depth
  then Result.Fields.create ~best_move ~score ~nodes_searched:st.nodes
  else iterdeep ~i ~moves @@ State.new_iter st

let go search = match Position.legal_moves search.root with
  | [] -> invalid_arg "No legal moves"
  | moves -> iterdeep ~moves @@ State.create search
