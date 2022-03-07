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
  let clear = Hashtbl.clear

  (* Once the search has decided on a move, the depth values for the entries
     need to be "aged" (decremented by two ply). Entries that reach the root
     should be evicted. *)
  let age tt =
    Hashtbl.filter_map_inplace tt ~f:(fun entry ->
        let depth = entry.depth - 2 in
        if depth <= 0 then None else Some {entry with depth})

  (* Store the evaluation results for the position. There is consideration to
     be made for the replacement strategy:

     https://www.chessprogramming.org/Transposition_Table#Replacement_Strategies
  *)
  let store tt pos ~depth ~score ~best ~bound =
    let key = Position.hash pos in
    let data = {depth; score; best; bound} in
    Hashtbl.set tt ~key ~data

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
        | Lower when score >= beta -> First beta
        | Lower -> Second (max alpha score, beta)
        | Upper when score <= alpha -> First alpha
        | Upper -> Second (alpha, min beta score)
        | Exact -> First score
      end
    | _ -> Second (alpha, beta)

  (* Extract the principal variation from the table. *)
  let pv tt m n =
    let rec aux i acc pos =
      match Hashtbl.find tt @@ Position.hash pos with
      | Some entry when n > i ->
        aux (i + 1) (entry.best :: acc) @@ Legal.new_position entry.best
      | _ -> List.rev acc in
    aux 0 [m] @@ Legal.new_position m
end

type t = {
  limits : limits;
  root : Position.t;
  history : int Int64.Map.t;
  tt : Tt.t;
  pst : Eval.Pst.t;
} [@@deriving fields]

module Result = struct
  type t = {
    best : Position.legal;
    pv : Position.legal list;
    score : int;
    evals : int;
    depth : int;
  } [@@deriving fields]
end

type result = Result.t
type search = t

let create = Fields.create

(* Important not to use `Int.min_value`, since negating it seems to give
   us back a negative number.

   See: https://ocaml.janestreet.com/ocaml-core/latest/doc/base/Base/Int/index.html#val-abs
*)
let inf = Int.max_value

let is_quiet m =
  Option.is_none @@ Legal.capture m &&
  Option.is_none @@ Move.promote @@ Legal.move m

let is_noisy = Fn.non is_quiet

(* Our state for the entirety of the search. *)
module State = struct
  module T = struct
    type t = {
      nodes : int;
      search : search;
      tt : Tt.t;
      pst : Eval.Pst.t;
      killer1 : Position.legal Int.Map.t;
      killer2 : Position.legal Int.Map.t;
      history : int array;
    } [@@deriving fields]

    let create search = {
      nodes = 0;
      search;
      tt = search.tt;
      pst = search.pst;
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

    (* Is `m` a killer move? *)
    let is_killer m ply st =
      Map.find st.killer1 ply |>
      Option.exists ~f:(Legal.same m) ||
      Map.find st.killer2 ply |>
      Option.exists ~f:(Legal.same m)

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
      Option.bind ~f:(fun entry ->
          match entry.Tt.bound with
          | Exact -> Some entry.Tt.best
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

(* Quiescence search is used when we reach our maximum depth for the main
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
      let check = Position.in_check pos in
      if List.is_empty moves
      then State.return @@ if check then -inf else 0
      else with_moves pos moves ~alpha ~beta

  and with_moves pos moves ~alpha ~beta =
    State.(update inc_nodes) >>= fun () ->
    State.(gets pst) >>= fun pst ->
    let score = Eval.go pos pst in
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

(* The main search of the position. The core of it is the negamax algorithm
   with alpha-beta pruning (and other enhancements). *)
module Main = struct
  (* Search from a new position. *)
  let rec go ?(null = true) pos ~alpha ~beta ~ply ~depth =
    State.(gets nodes) >>= fun nodes ->
    State.(gets search) >>= fun search ->
    (* Check if we reached the search limits, as well as for positions
       where a player may claim a draw. *)
    if Limits.is_max_nodes nodes search.limits
    || check_repetition search pos
    || Position.halfmove pos >= 100 then State.return 0
    else (* Find if we evaluated this position previously. *)
      State.(gets tt) >>= fun tt ->
      match Tt.lookup tt ~pos ~depth ~alpha ~beta with
      | First score -> State.return score
      | Second (alpha, beta) ->
        let moves = Position.legal_moves pos in
        let check = Position.in_check pos in
        if List.is_empty moves
        then State.return @@ if check then -inf else 0
        else with_moves pos moves ~alpha ~beta ~ply ~depth ~check ~null

  (* Principal variation search. *)
  and pvs ps pos ~beta ~ply ~depth =
    let open Plysearch in
    let f alpha =
      go pos
        ~alpha
        ~beta:(-ps.alpha)
        ~ply:(ply + 1)
        ~depth:(depth - 1) >>= negm in
    let alpha = if ps.full_window then -beta else (-ps.alpha) - 1 in
    f alpha >>= function
    | score when ps.full_window -> State.return score
    | score when score > ps.alpha && score < beta -> f (-beta)
    | score -> State.return score

  (* Amount to reduce the depth by. *)
  and reduction_factor = 2

  (* Late move reduction. *)
  and lmr ps pos m ~beta ~ply ~depth ~check =
    let open Plysearch in
    let pos' = Legal.new_position m in
    if beta - ps.alpha <= 1
    && not ps.full_window
    && depth > reduction_factor
    && not check
    && is_quiet m
    && not @@ Position.in_check pos'
    then State.(gets @@ is_killer m ply) >>= function
      | true -> State.return None
      | false ->
        Legal.new_position m |> go
          ~alpha:((-ps.alpha) - 1)
          ~beta:(-ps.alpha)
          ~ply:(ply + 1)
          ~depth:(depth - reduction_factor) >>= negm >>| Option.some
    else State.return None

  (* Combine LMR and PVS. *)
  and lmr_pvs ps pos m ~beta ~ply ~depth ~check =
    let pvs () = Legal.new_position m |> pvs ps ~beta ~ply ~depth in
    lmr ps pos m ~beta ~ply ~depth ~check >>= function
    | Some score when score > ps.alpha -> pvs ()
    | Some score -> State.return score
    | None -> pvs ()

  (* The actual search, given all the legal moves. *)
  and with_moves ?(null = true) pos moves ~alpha ~beta ~ply ~depth ~check =
    (* Extend the depth limit if we're in check. Since the number of
       responses to a check is typically very low, the search should
       finish quickly. *)
    let depth = depth + Bool.to_int check in
    if depth <= 0 then Quiescence.with_moves pos moves ~alpha ~beta
    else reduce pos moves
        ~alpha ~beta ~ply ~depth ~check ~depth ~null >>= function
      | Some score -> State.return score
      | None -> let open Continue_or_stop in
        State.(gets tt) >>= fun tt ->
        State.(gets pst) >>= fun pst ->
        let ps = Plysearch.create moves ~alpha in
        Ordering.sort moves ~ply ~pos ~tt >>= fun moves ->
        let finish () = State.return ps.alpha in
        State.List.fold_until moves ~init:() ~finish ~f:(fun () m ->
            lmr_pvs ps pos m ~beta ~ply ~depth ~check
            >>= fun score -> if score >= beta
            then Plysearch.cutoff ps ply depth m >>| fun () -> Stop beta
            else State.return @@ Continue (Plysearch.better ps m score))
        >>| fun score ->
        Tt.store tt pos ~depth ~score ~best:ps.best ~bound:ps.bound;
        score

  (* Try to reduce the depth. *)
  and reduce pos moves ~alpha ~beta ~ply ~depth ~check ~depth ~null =
    if beta - alpha > 1 || check || not null
    then State.return None
    else State.(gets pst) >>= fun pst ->
      let score = Eval.go pos pst in
      nmr pos ~score ~beta ~ply ~depth >>= function
      | Some _ as beta -> State.return beta
      | None -> razor pos moves ~score ~alpha ~beta ~depth

  (* Null move reduction. *)
  and nmr pos ~score ~beta ~ply ~depth =
    if score >= beta && depth > reduction_factor then
      (* Forfeit our right to play a move. *)
      Position.null_move pos |> go
        ~alpha:(-beta)
        ~beta:((-beta) + 1)
        ~ply:(ply + 1)
        ~depth:(depth - 1 - reduction_factor)
        ~null:false >>= negm >>| fun score ->
      (* Opponent's best response still produces a beta cutoff, so we know
         this position is unlikely. *)
      Option.some_if (score >= beta) beta
    else State.return None

  (* Razoring. 

     Drop down to quescience search if we're approaching the horizon and we 
     have little chance to improve alpha.
  *)
  and razor pos moves ~score ~alpha ~beta ~depth =
    let score = score + (razor_margin * depth) in
    if score < alpha && depth < razor_limit then
      Quiescence.with_moves pos moves ~alpha ~beta >>| fun score ->
      Option.some_if (score < alpha) score
    else State.return None

  and razor_margin =
    let m = Piece.Kind.value Pawn * Eval.material_weight in
    m + (m / 2)

  and razor_limit = 4

  (* The search we start from the root position. *)
  and root moves depth =
    let open Continue_or_stop in
    State.(gets search) >>= fun search ->
    State.(gets tt) >>= fun tt ->
    let pos = search.root in
    Ordering.sort moves ~ply:0 ~pos ~tt >>= fun moves ->
    let ps = Plysearch.create moves in
    let finish () = State.return ps.alpha in
    State.List.fold_until moves ~init:() ~finish ~f:(fun () m ->
        Legal.new_position m |>
        pvs ps ~ply:1 ~depth:(depth - 1) ~beta:inf >>= fun score ->
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
end

(* Use iterative deepening to optimize the search. This relies on previous
   evaluations stored in the transposition table to prune more nodes with
   each successive iteration. *)
let rec iterdeep ?(i = 1) st ~moves =
  let (best, score), st = Monad.State.run (Main.root moves i) st in
  (* If we found a mating sequence, then there's no reason to iterate
     again since it will most likely return the same result. *)
  let n = st.search.limits.depth in
  if score = inf || i >= n then
    let pv = Tt.pv st.tt best n in
    Result.Fields.create ~best ~pv ~score ~evals:st.nodes ~depth:i
  else iterdeep ~i:(i + 1) ~moves @@ State.new_iter st

let go search = match Position.legal_moves search.root with
  | [] -> invalid_arg "No legal moves"
  | moves ->
    let res = iterdeep ~moves @@ State.create search in
    Tt.age search.tt;
    res
