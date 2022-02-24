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
    if depth < 1 then invalid_argf "Invalid depth limit %d" depth ()
    else match nodes with
      | Some n when n < 1 -> invalid_argf "Invalid node limit %d" n ()
      | _ -> Fields.create ~depth ~nodes

  let is_max_nodes n = function
    | {nodes = Some nodes; _} -> n >= nodes
    | _ -> false
end

type limits = Limits.t

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

  let create () = Hashtbl.create ~size:0x40000 (module Int64)
  let clear tt = Hashtbl.clear tt

  (* Once the search has decided on a move, the depth values for the entries
     need to be "aged" (decremented by two ply). The older entries will
     eventually get evicted by future calls to `store`. *)
  let age tt =
    Hashtbl.map_inplace tt ~f:(fun entry ->
        {entry with depth = entry.depth - 2})

  (* Store the evaluation results for the position. There is consideration to
     be made for the replacement strategy:

     https://www.chessprogramming.org/Transposition_Table#Replacement_Strategies

     For now, we will favor entries with a higher depth, as a deeper search 
     should, intuitively, have the more accurate results.
  *)
  let store tt pos ~depth ~score ~best ~bound =
    let entry = {depth; score; best; bound} in
    Position.hash pos |> Hashtbl.update tt ~f:(function
        | Some old when depth > old.depth -> entry
        | Some old -> old
        | None -> entry)

  (* Check for a previous evaluation of the position at a comparable depth.

     - Lower: the score is a lower bound, so only return it if it causes a
              beta cutoff, which would prune the rest of the branch being
              searched.

     - Upper: the score is an upper bound, so if it doesn't improve alpha
              we can use that score to prune the rest of the branch being
              searched.

     - Exact: the score is an exact evaluation for this position.
  *)
  let lookup tt ~pos ~depth ~alpha ~beta =
    match Hashtbl.find tt @@ Position.hash pos with
    | Some {depth = depth'; score; bound; _} when depth' >= depth -> begin
        match bound with
        | Lower -> Option.some_if (score >= beta)  score
        | Upper -> Option.some_if (score <= alpha) score
        | Exact -> Some score
      end
    | _ -> None
end

type t = {
  limits : limits;
  root : Position.t;
  history : int Int64.Map.t;
  tt : Tt.t;
} [@@deriving fields]

module Result = struct
  type t = {
    best : Position.legal;
    score : int;
    evals : int;
    depth : int;
  } [@@deriving fields]
end

type result = Result.t
type search = t

let create ?(tt = Tt.create ()) ~limits ~root ~history () =
  Fields.create ~limits ~root ~history ~tt

(* Important not to use `Int.min_value`, since negating it seems to give
   us back a negative number.

   See: https://ocaml.janestreet.com/ocaml-core/latest/doc/base/Base/Int/index.html#val-abs
*)
let inf = Int.max_value

let is_quiet m =
  Option.is_none @@ Legal.capture m &&
  Option.is_none @@ Move.promote @@ Legal.move m

let is_noisy = Fn.compose Option.is_some Legal.capture

(* Our state for the entirety of the search. *)
module State = struct
  module T = struct
    type t = {
      nodes : int;
      search : search;
      tt : Tt.t;
      killer1 : Position.legal Int.Map.t;
      killer2 : Position.legal Int.Map.t;
      history : int array;
    } [@@deriving fields]

    let create search = {
      nodes = 0;
      search;
      tt = search.tt;
      killer1 = Int.Map.empty;
      killer2 = Int.Map.empty;
      history = Array.create ~len:Square.(count * count) 0;
    }

    (* Start a new search while reusing the transposition table. *)
    let new_iter st = {st with nodes = 0}

    (* Increment the number of nodes we've evaluated. *)
    let inc_nodes st = {st with nodes = st.nodes + 1}

    (* Get the first killer move. *)
    let killer1 ply st =
      if ply > 0 then Map.find st.killer1 ply else None

    (* Get the second killer move. *)
    let killer2 ply st =
      if ply > 0 then Map.find st.killer2 ply else None

    (* Update the killer move for a particular ply. *)
    let killer ply m st =
      if ply > 0 then
        let killer2 = match Map.find st.killer1 ply with
          | Some data -> Map.set st.killer2 ~key:ply ~data
          | None -> st.killer2 in
        let killer1 = Map.set st.killer1 ~key:ply ~data:m in
        {st with killer1; killer2}
      else st

    (* Update the history score. *)
    let quiet m depth st =
      if is_quiet m then
        let m = Legal.move m in
        let src = Square.to_int @@ Move.src m in
        let dst = Square.to_int @@ Move.dst m in
        let i = src + dst * Square.count in
        let d = Array.unsafe_get st.history i + (depth * depth) in
        Array.unsafe_set st.history i d
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

(* Will playing this position likely lead to a repetition draw?
   We could reject any repeated position, but in practice we can
   assume that the opponent will generally try to play for a win
   instead of a draw. *)
let check_repetition search pos =
  Position.hash pos |> Map.find search.history |>
  Option.value_map ~default:false ~f:(fun n -> n >= 2)

(* Move ordering is critical for optimizing the performance of alpha-beta
   pruning. We use some heuristics to determine which moves are likely
   to be the best, and then search those first, hoping that the worse
   moves get pruned more effectively. *)
module Ordering = struct
  let capture_bonus = 4000
  let promote_bonus = 3000
  let killer1_bonus = 2000
  let killer2_bonus = 1000
  let control_penalty = -350

  (* Most Valuable Victim/Least Valuable Attacker *)
  let mvv_lva =
    let victims = Piece.[Pawn; Knight; Bishop; Rook; Queen] in
    let attackers = Piece.King :: List.rev victims in
    let num_attackers = List.length attackers in
    let num_victims = List.length victims in
    let tbl = Array.create ~len:(num_victims * num_attackers) 0 in
    List.fold victims ~init:0 ~f:(fun acc victim ->
        let i = Piece.Kind.to_int victim in
        List.fold attackers ~init:acc ~f:(fun acc attacker ->
            let j = Piece.Kind.to_int attacker in
            tbl.(i + j * num_victims) <- acc;
            acc + 1)) |> ignore;
    fun victim attacker ->
      let i = Piece.Kind.to_int victim in
      let j = Piece.Kind.to_int attacker in
      Array.unsafe_get tbl (i + j * num_victims)

  (* Check if a particular move was part of the principal variation. *)
  let is_best pos tt =
    let best =
      Position.hash pos |> Hashtbl.find tt |>
      Option.bind ~f:(fun (entry : Tt.entry) ->
          (* Only allow exact scores. *)
          match entry.bound with
          | Exact -> Some entry.best
          | _ -> None) in
    fun m -> Option.exists best ~f:(Legal.same m)

  (* Prioritize captures according to the MVV/LVA table. *)
  let capture m =
    Legal.capture m |> Option.value_map ~default:0 ~f:(fun victim ->
        let src = Move.src @@ Legal.move m in
        let p = Position.piece_at_square_exn (Legal.parent m) src in
        let attacker = Piece.kind p in
        capture_bonus + mvv_lva victim attacker)

  (* Prioritize promotions by the value of the piece *)
  let promote m =
    Legal.move m |> Move.promote |>
    Option.value_map ~default:0 ~f:(fun k ->
        promote_bonus + Piece.Kind.value k * Eval.material_weight)

  (* Penalize moving to squares that are attacked by the opponent (unless the
     move is made by a pawn). *)
  let attacked att m =
    let src = Move.src @@ Legal.move m in
    let p = Position.piece_at_square_exn (Legal.parent m) src  in
    if Piece.is_pawn p then 0
    else
      let dst = Move.dst @@ Legal.move m in
      if Bb.(dst @ att) then control_penalty else 0

  (* Overall score. *)
  let score att m = capture m + promote m + attacked att m

  (* Use the transposition table to favor moves that are in the principal
     variation. Also, apply the killer move and history heuristics. *)
  let sort moves ~ply ~pos ~tt =
    let best = is_best pos tt in
    let att = Position.(Attacks.all pos @@ inactive pos) in
    State.(gets @@ killer1 ply) >>= fun killer1 ->
    State.(gets @@ killer2 ply) >>= fun killer2 ->
    State.(gets history) >>| fun history ->
    let killer m b =
      Option.value_map ~default:0 ~f:(fun k ->
          if Legal.same m k then b else 0) in
    Legal.sort moves ~eval:(fun m ->
        if best m then inf
        else
          let s = score att m in
          let k1 = killer m killer1_bonus killer1 in
          let k2 = killer m killer2_bonus killer2 in
          let q =
            let m = Legal.move m in
            let src = Square.to_int @@ Move.src m in
            let dst = Square.to_int @@ Move.dst m in
            let i = src + dst * Square.count in
            Array.unsafe_get history i in
          s + k1 + k2 + q)

  (* Sort for quiescence search, ignoring PV nodes. *)
  let qsort ~pos =
    let att = Position.(Attacks.all pos @@ inactive pos) in
    Legal.sort ~eval:(score att)
end

let negm = Fn.compose State.return Int.neg

(* Quoescence search is used when we reach our maximum depth for the main
   search. The goal is then to keep searching only "noisy" positions, until
   we reach one that is "quiet", and then return our evaluation. *)
module Quiescence = struct
  let rec go pos ~alpha ~beta =
    State.(gets nodes) >>= fun nodes ->
    State.(gets search) >>= fun search ->
    if Limits.is_max_nodes nodes search.limits
    then State.return 0
    else
      let moves = Position.legal_moves pos in
      let in_check = Position.in_check pos in
      if List.is_empty moves
      then State.return @@ if in_check then -inf else 0
      else with_moves pos moves ~alpha ~beta

  and with_moves pos moves ~alpha ~beta =
    State.(update inc_nodes) >>= fun () ->
    let score, _ = Eval.go pos in
    if score >= beta then State.return beta
    else let open Continue_or_stop in
      let init = max score alpha in
      let finish = State.return in
      List.filter moves ~f:is_noisy |> Ordering.qsort ~pos |>
      State.List.fold_until ~init ~finish ~f:(fun alpha m ->
          let pos' = Legal.new_position m in
          go pos' ~alpha:(-beta) ~beta:(-alpha) >>= negm >>| fun score ->
          if score >= beta then Stop beta else Continue (max score alpha))
end

(* The search results for all the moves in one ply. *)
module Plysearch = struct
  type t = {
    mutable best : Position.legal;
    mutable alpha : int;
    mutable full_window : bool;
    mutable bound : Tt.bound;
  }

  let create ?(alpha = -inf) moves = {
    best = List.hd_exn moves;
    alpha;
    full_window = true;
    bound = Tt.Upper;
  }

  (* Beta cutoff. *)
  let cutoff ps ply depth m =
    State.(update @@ killer ply m) >>= fun () ->
    State.(gets @@ quiet m depth) >>| fun () ->
    ps.best <- m;
    ps.bound <- Tt.Lower

  (* Alpha may have improved. *)
  let better ps m score =
    if score > ps.alpha then begin
      ps.best <- m;
      ps.alpha <- score;
      ps.full_window <- false;
      ps.bound <- Tt.Exact;
    end
end

(* Principal variation search can lead to more pruning if the move ordering
   was (more or less) correct.

   See: https://en.wikipedia.org/wiki/Principal_variation_search
*)
let rec pvs ?(null = false) pos ps ~beta ~ply ~depth = let open Plysearch in
  let f alpha =
    negamax pos ~null ~alpha ~beta:(-ps.alpha) ~ply ~depth >>= negm in
  let alpha = if ps.full_window then -beta else (-ps.alpha) - 1 in
  f alpha >>= function
  | score when ps.full_window -> State.return score
  | score when score > ps.alpha && score < beta -> f (-beta)
  | score -> State.return score

(* Search from a new position. *)
and negamax ?(null = false) pos ~alpha ~beta ~ply ~depth =
  State.(gets nodes) >>= fun nodes ->
  State.(gets search) >>= fun search ->
  (* Check if we reached the search limits, as well as for positions
     where a player may claim a draw. *)
  if Limits.is_max_nodes nodes search.limits
  || check_repetition search pos
  || Position.halfmove pos >= 100 then State.return 0
  else 
    (* Find if we evaluated this position previously. *)
    State.(gets tt) >>= fun tt ->
    match Tt.lookup tt ~pos ~depth ~alpha ~beta with
    | Some score -> State.return score
    | None ->
      let moves = Position.legal_moves pos in
      let in_check = Position.in_check pos in
      if List.is_empty moves
      then State.return @@ if in_check then -inf else 0
      else
        let score, endgame = Eval.go pos in
        pre pos moves
          ~alpha
          ~beta
          ~ply
          ~depth
          ~in_check
          ~null
          ~score
          ~endgame >>= function
        | Some score -> State.return score
        | None ->
          (* Extend the depth limit if we're in check. Since the number of
             responses to a check is typically very low, the search should
             finish quickly. *)
          let check = Bool.to_int in_check in
          if depth + check <= 0
          then Quiescence.with_moves pos moves ~alpha ~beta
          else let open Continue_or_stop in
            Ordering.sort moves ~ply ~pos ~tt >>= fun moves ->
            let ps = Plysearch.create moves ~alpha in
            let finish () = State.return ps.alpha in
            (* PV node. *)
            let pv = beta - alpha > 1 in
            (* Futility pruning. *)
            let moves =
              if not pv &&
                 let margin = Piece.Kind.value Rook * Eval.material_weight in
                 depth = 1 && not in_check && score + margin <= alpha
              then List.filter moves ~f:is_quiet else moves in
            State.List.fold_until moves ~init:() ~finish ~f:(fun () m ->
                let pos' = Legal.new_position m in
                pvs pos' ps ~beta ~ply:(ply + 1) ~depth:(depth - 1 + check)
                >>= fun score -> if score >= beta
                then Plysearch.cutoff ps ply depth m >>| fun () -> Stop beta
                else State.return @@ Continue (Plysearch.better ps m score))
            >>| fun score ->
            Tt.store tt pos ~depth ~score ~best:ps.best ~bound:ps.bound;
            score

(* Try to prune this branch before doing the search. *)
and pre pos moves ~alpha ~beta ~ply ~depth ~in_check ~null ~score ~endgame =
  begin if in_check then State.return None
    else razor pos moves ~depth ~alpha ~beta ~score ~endgame end >>= function
  | Some _ as cut -> State.return cut
  | None -> match rfp pos ~depth ~beta ~score with
    | Some _ as cut -> State.return cut
    | None -> if null || in_check
      then State.return None
      else nmr pos ~ply ~depth ~alpha ~beta ~score ~endgame

(* Razoring, see: https://www.chessprogramming.org/Razoring

   If we're on the horizon and we are unlikely to improve alpha,
   then drop into a quiescence search.
*)
and razor pos moves ~depth ~alpha ~beta ~score ~endgame =
  let margin = Piece.Kind.value Rook * Eval.material_weight in
  if Float.(endgame < 0.7) && depth = 1 && score + margin <= alpha
  then Quiescence.with_moves pos moves ~alpha ~beta >>| Option.some
  else State.return None

(* Reverse futility pruning, see:
   https://www.chessprogramming.org/Reverse_Futility_Pruning 
*)
and rfp pos ~depth ~beta ~score =
  if depth >= 1 && depth <= Array.length rfp_margin then
    let margin =
      Array.unsafe_get rfp_margin (depth - 1) * Eval.material_weight in
    let s = score - margin in
    Option.some_if (s >= beta) s
  else None

and rfp_margin = Piece.Kind.[|
    value Pawn;
    value Knight;
    value Bishop;
    value Rook
  |]

(* Null move reduction, see:
   https://www.chessprogramming.org/Null_Move_Pruning

   The idea is that if we pretend that we didn't move on this ply, and
   all of the opponent's responses still leave us with an advantage,
   then we can conclude that this position should never arise from best
   play on both sides.
*)
and nmr pos ~ply ~depth ~alpha ~beta ~score ~endgame =
  let r = 3 in
  if Float.(endgame < 0.7) && depth >= r && score >= beta then
    Position.null_move pos |> negamax
      ~null:true
      ~ply:(ply + 1)
      ~depth:(depth - r - 1)
      ~alpha:(-beta)
      ~beta:((-beta) + 1) >>| fun score ->
    Option.some_if (score >= beta) beta
  else State.return None 

(* The search we start from the root position. *)
let rootmax moves depth =
  let open Continue_or_stop in
  State.(gets search) >>= fun search ->
  State.(gets tt) >>= fun tt ->
  let pos = search.root in
  Ordering.sort moves ~ply:0 ~pos ~tt >>= fun moves ->
  let ps = Plysearch.create moves in
  let finish () = State.return ps.alpha in
  let depth' = depth - 1 in
  State.List.fold_until moves ~init:() ~finish ~f:(fun () m ->
      let pos' = Legal.new_position m in
      pvs pos' ps ~null:true ~ply:1 ~depth:depth' ~beta:inf >>= fun score ->
      State.(gets nodes) >>| fun nodes ->
      if Limits.is_max_nodes nodes search.limits then Stop ps.alpha
      else begin
        (* Stop if we've found a mating sequence. *)
        Plysearch.better ps m score;
        if score = inf then Stop score else Continue ()
      end) >>| fun score ->
  (* Update the transposition table and return the results. *)
  let best = ps.best in
  Tt.store tt pos ~depth ~score ~best ~bound:Exact;
  best, score

(* Use iterative deepening to optimize the search. This relies on previous
   evaluations stored in the transposition table to prune more nodes with
   each successive iteration. *)
let rec iterdeep ?(i = 1) st ~moves =
  let (best, score), st = Monad.State.run (rootmax moves i) st in
  (* If we found a mating sequence, then there's no reason to iterate
     again since it will most likely return the same result. *)
  if score = inf || i >= st.search.limits.depth
  then Result.Fields.create ~best ~score ~evals:st.nodes ~depth:i
  else iterdeep ~i:(i + 1) ~moves @@ State.new_iter st

let go search = match Position.legal_moves search.root with
  | [] -> invalid_arg "No legal moves"
  | moves ->
    let res = iterdeep ~moves @@ State.create search in
    Tt.age search.tt;
    res
