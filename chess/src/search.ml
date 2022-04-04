open Core_kernel
open Monads.Std

module Bb = Bitboard
module Legal = Position.Legal

module Limits = struct
  type kind =
    | Infinite
    | Depth of int
    | Time of int

  type t = {
    nodes : int option;
    kind  : kind;
  }

  let nodes {nodes; _} = nodes

  let depth = function
    | {kind = Depth n; _} -> Some n
    | _ -> None

  let time = function
    | {kind = Time n; _} -> Some n
    | _ -> None

  let infinite = function
    | {kind = Infinite; _} -> true
    | _ -> false

  let check_nodes = function
    | None -> ()
    | Some n when n >= 1 -> ()
    | Some n ->
      invalid_argf "Invalid node limit %d, must be greater than 0" n ()

  let of_infinite ?(nodes = None) () =
    check_nodes nodes;
    {nodes; kind = Infinite}

  let of_depth ?(nodes = None) = function
    | n when n < 1 ->
      invalid_argf "Invalid depth limit %d, must be greater than 0" n ()
    | n ->
      check_nodes nodes;
      {nodes; kind = Depth n}

  let of_search_time ?(nodes = None) = function
    | n when n < 1 ->
      invalid_argf "Invalid search time %d, must be greater than 0" n ()
    | n ->
      check_nodes nodes;
      {nodes; kind = Time n}

  let of_game_time
      ?(nodes = None)
      ?(moves_to_go = None)
      ~wtime
      ~winc
      ~btime
      ~binc
      ~active
      () =
    (* Validate inputs. *)
    check_nodes nodes;
    if wtime < 1 then
      invalid_argf "Invalid white time %d, must be greater than 0" wtime ();
    if winc < 0 then
      invalid_argf "Invalid white increment %d, must be positive" winc ();
    if btime < 1 then
      invalid_argf "Invalid black time %d, must be greater than 0" btime ();
    if binc < 0 then
      invalid_argf "Invalid black increment %d, must be positive" binc ();
    Option.iter moves_to_go ~f:(function
        | n when n < 0 ->
          invalid_argf "Invalid number of moves to go %d, must be positive" n ()
        | _ -> ());
    (* Calculate the amount of time to search. *)
    let our_time, our_inc, their_time = match active with
      | Piece.White -> wtime, winc, btime
      | Piece.Black -> btime, binc, wtime in
    let time = match moves_to_go with
      | Some n -> our_time / (n + 3)
      | None ->
        let ratio =
          Float.(min (max (of_int our_time / of_int their_time) 1.0) 2.0) in
        our_time / int_of_float (20.0 *. ratio) in
    {nodes; kind = Time (time + our_inc)}
end

type limits = Limits.t

(* Transposition table for caching search results. *)
module Tt = struct
  type bound = Lower | Upper | Exact

  module Entry = struct
    type t = {
      depth : int;
      score : int;
      best  : Position.legal;
      bound : bound;
    } [@@deriving fields]

    let position {best; _} = Position.Legal.parent best
  end

  type entry = Entry.t
  type t = (int64, entry) Hashtbl.t

  let find tt pos = Hashtbl.find tt @@ Position.hash pos
  let create () = Hashtbl.create (module Int64)
  let clear = Hashtbl.clear

  (* Store the evaluation results for the position. There is consideration to
     be made for the replacement strategy:

     https://www.chessprogramming.org/Transposition_Table#Replacement_Strategies

     For the same position, prefer the more recent search.
  *)
  let store tt pos ~depth ~score ~best ~bound =
    let key = Position.hash pos in
    let data = Entry.Fields.create ~depth ~score ~best ~bound in
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
  let lookup tt ~pos ~depth ~alpha ~beta = match find tt pos with
    | None -> Second (alpha, beta)
    | Some entry when Entry.depth entry < depth -> Second (alpha, beta)
    | Some Entry.{score; bound; _} -> match bound with
      | Lower when score >= beta -> First beta
      | Lower -> Second (max alpha score, beta)
      | Upper when score <= alpha -> First alpha
      | Upper -> Second (alpha, min beta score)
      | Exact -> First score

  (* Extract the principal variation from the table. *)
  let pv ?(mate = false) tt n m =
    let rec aux i acc pos = match find tt pos with
      | None -> List.rev acc
      | Some Entry.{best; _} when mate || n > i ->
        let i = i + 1 in
        let acc = best :: acc in
        aux i acc @@ Legal.new_position best
      | _ -> List.rev acc in
    aux 1 [m] @@ Legal.new_position m
end

module Result = struct
  type t = {
    pv    : Position.legal list;
    score : Uci.Send.Info.score;
    nodes : int;
    depth : int;
    time  : int;
  } [@@deriving fields]

  let best {pv; _} = List.hd_exn pv
end

type result = Result.t

(* The input parameters to the search. *)
type t = {
  limits  : limits;
  root    : Position.t;
  history : int Int64.Map.t;
  tt      : Tt.t;
  stop    : bool ref;
} [@@deriving fields]

let create ~limits ~root ~history ~tt ~stop =
  let history =
    (* Make sure that the root position is in our history. *)
    Position.hash root |> Map.update history ~f:(function
        | Some n -> n | None -> 1) in
  Fields.create ~limits ~root ~history ~tt ~stop

(* Constants. *)
let inf = 65535
let mate_score = inf / 2
let max_ply = 64

(* Mate scores should be relative to the distance from the root position.
   Shorter distances are to be preferred. *)
let is_mate score = score >= mate_score - max_ply
let is_mated score = score <= (-mate_score) + max_ply

let is_quiet m =
  Option.is_none @@ Legal.capture m &&
  Option.is_none @@ Move.promote @@ Legal.move m

let is_noisy = Fn.non is_quiet

(* Our state for the entirety of the search. *)
module State = struct
  type search = t

  module T = struct
    type t = {
      start_time   : Time.t;
      nodes        : int;
      search       : search;
      killer1      : Position.legal Int.Map.t;
      killer2      : Position.legal Int.Map.t;
      move_history : int array;
    } [@@deriving fields]

    let create search = {
      start_time = Time.now ();
      nodes = 0;
      search;
      killer1 = Int.Map.empty;
      killer2 = Int.Map.empty;
      move_history = Array.create ~len:Square.(count * count) 0;
    }

    (* Start a new search while reusing the transposition table. *)
    let new_iter st = {st with nodes = 0}

    (* Increment the number of nodes we've evaluated. *)
    let inc_nodes st = {st with nodes = st.nodes + 1}

    (* Get the first killer move. *)
    let killer1 ply st = Map.find st.killer1 ply

    (* Get the second killer move. *)
    let killer2 ply st = Map.find st.killer2 ply

    (* Is `m` a killer move? *)
    let is_killer m ply st =
      Map.find st.killer1 ply |>
      Option.exists ~f:(Legal.same m) ||
      Map.find st.killer2 ply |>
      Option.exists ~f:(Legal.same m)

    (* Update the killer move for a particular ply. *)
    let killer ply m st =
      if is_quiet m then
        let killer2 = match Map.find st.killer1 ply with
          | Some data -> Map.set st.killer2 ~key:ply ~data
          | None -> st.killer2 in
        let killer1 = Map.set st.killer1 ~key:ply ~data:m in
        {st with killer1; killer2}
      else st

    (* Update the move history heuristic. *)
    let history m depth st =
      if is_quiet m then
        let m = Legal.move m in
        let src = Square.to_int @@ Move.src m in
        let dst = Square.to_int @@ Move.dst m in
        let i = src + dst * Square.count in
        let d = Array.unsafe_get st.move_history i + (depth * depth) in
        Array.unsafe_set st.move_history i d
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

let return = State.return

let check_limits =
  State.get () >>| fun {start_time; search; nodes; _} ->
  !(search.stop) ||
  let limits = search.limits in
  match limits.nodes with
  | Some n when nodes >= n -> true
  | _ -> match limits.kind with
    | Infinite | Depth _ -> false
    | Time t ->
      let elapsed =
        int_of_float @@
        Time.(Span.to_ms @@ diff (now ()) start_time) in
      elapsed >= t

(* Will playing this position likely lead to a repetition draw? *)
let check_repetition search pos =
  Position.hash pos |> Map.find search.history |>
  Option.value_map ~default:false ~f:(fun n -> n > 0)

(* Move ordering is critical for optimizing the performance of alpha-beta
   pruning. We use some heuristics to determine which moves are likely
   to be the best, and then search those first, hoping that the worse
   moves get pruned more effectively. *)
module Order = struct
  let capture_bonus = 4000
  let promote_bonus = 3000
  let killer1_bonus = 2000
  let killer2_bonus = 1000

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

  (* Check if a particular move has been evaluated already. *)
  let is_best pos tt =
    let best =
      Position.hash pos |> Hashtbl.find tt |>
      Option.bind ~f:(fun entry ->
          match Tt.Entry.bound entry with
          | Exact -> Some (Tt.Entry.best entry)
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
        let k = Move.Promote.to_piece_kind k in
        promote_bonus + Piece.Kind.value k * Eval.material_weight)

  (* Sort by the result of `eval` for each move. *)
  let sort_aux moves ~eval =
    List.map moves ~f:(fun m -> m, eval m) |>
    List.sort ~compare:(fun (_, a) (_, b) -> Int.compare b a) |>
    List.map ~f:fst

  (* Sort for normal search. *)
  let sort moves ~ply ~pos ~tt =
    let best = is_best pos tt in
    State.(gets @@ killer1 ply) >>= fun killer1 ->
    State.(gets @@ killer2 ply) >>= fun killer2 ->
    State.(gets move_history) >>| fun move_history ->
    let killer m = match killer1, killer2 with
      | Some k, _ when Legal.same m k -> killer1_bonus
      | _, Some k when Legal.same m k -> killer2_bonus
      | _ -> 0 in
    let history m =
      let m = Legal.move m in
      let src = Square.to_int @@ Move.src m in
      let dst = Square.to_int @@ Move.dst m in
      let i = src + dst * Square.count in
      Array.unsafe_get move_history i in
    sort_aux moves ~eval:(fun m ->
        if best m then inf
        else let capture = capture m in
          if capture <> 0 then capture
          else let promote = promote m in
            if promote <> 0 then promote
            else let killer = killer m in
              if killer <> 0 then killer
              else history m)

  (* Sort for quiescence search. *)
  let qsort moves =
    sort_aux moves ~eval:(fun m ->
        let capture = capture m in
        if capture <> 0 then capture
        else promote m)
end

let negm = Fn.compose return Int.neg

(* Quiescence search is used when we reach our maximum depth for the main
   search. The goal is then to keep searching only "noisy" positions, until
   we reach one that is "quiet", and then return our evaluation. *)
module Quiescence = struct
  let margin = Piece.Kind.value Queen * Eval.material_weight

  let rec go pos ~alpha ~beta ~ply =
    let moves = Position.legal_moves pos in
    let check = Position.in_check pos in
    if List.is_empty moves
    then return @@ if check then -mate_score + ply else 0
    else with_moves pos moves ~alpha ~beta ~ply

  and with_moves pos moves ~alpha ~beta ~ply = check_limits >>= function
    | true -> return 0
    | false -> State.(update inc_nodes) >>= fun () ->
      let score = Eval.go pos in
      if score >= beta then return beta
      else if score + margin < alpha && not @@ Eval.is_endgame pos
      then return alpha
      else let open Continue_or_stop in
        State.(gets search) >>= fun {tt; _} ->
        let init = max score alpha in
        List.filter moves ~f:is_noisy |> Order.qsort |>
        State.List.fold_until ~init ~finish:return ~f:(fun alpha m ->
            Legal.new_position m |>
            go ~alpha:(-beta) ~beta:(-alpha) ~ply:(ply + 1) >>=
            negm >>| fun score -> if score >= beta
            then Stop beta else Continue (max score alpha))
end

(* The main search of the game tree. The core of it is the negamax algorithm
   with alpha-beta pruning (and other enhancements). *)
module Main = struct
  (* The results for a single ply. *)
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
  let cutoff t m ~ply =
    t.best <- m;
    t.bound <- Tt.Lower;
    State.(update @@ killer ply m)

  (* Alpha may have improved. *)
  let better t m ~score ~depth =
    if score > t.alpha then begin
      t.best <- m;
      t.alpha <- score;
      t.full_window <- false;
      t.bound <- Tt.Exact;
      State.(gets @@ history m depth)
    end else return ()

  let drawn search pos =
    check_repetition search pos ||
    Position.halfmove pos >= 100 ||
    Position.is_insufficient_material pos

  (* Search from a new position. *)
  let rec go ?(null = false) pos ~alpha ~beta ~ply ~depth =
    check_limits >>= function
    | true -> return 0
    | false -> State.(gets search) >>= fun search ->
      (* Will the position lead to a draw? *)
      if drawn search pos then return 0
      (* Find a cached evaluation of the position. *)
      else match Tt.lookup search.tt ~pos ~depth ~alpha ~beta with
        | First score -> return score
        | Second (alpha, beta) ->
          let moves = Position.legal_moves pos in
          let check = Position.in_check pos in
          (* Checkmate or stalemate. *)
          if List.is_empty moves
          then return @@ if check then -mate_score + ply else 0
          (* Check extension. *)
          else let depth = depth + Bool.to_int check in
            if depth <= 0 then
              Quiescence.with_moves pos moves ~alpha ~beta ~ply
            else match mdp ~alpha ~beta ~ply with
              | First alpha -> return alpha
              | Second (alpha, beta) ->
                (* Use the null move hypothesis to reduce the depth of the
                   search. *)
                let score = Eval.go pos in
                reduce pos moves
                  ~score ~alpha ~beta ~ply ~depth
                  ~check ~depth ~null >>= function
                | Some score -> return score
                | None -> let open Continue_or_stop in
                  Order.sort moves ~ply ~pos ~tt:search.tt >>= fun moves ->
                  let t = create moves ~alpha in
                  let finish () = return t.alpha in
                  let f = branch t ~pos ~score ~beta ~depth ~ply ~check in
                  State.List.fold_until moves ~init:() ~finish ~f >>| fun score ->
                  (* To be safe, don't cache the results of a null move. *)
                  if not null then
                    Tt.store search.tt pos ~depth ~score
                      ~best:t.best ~bound:t.bound;
                  score

  (* Search a branch of the current node. *)
  and branch t ~pos ~score ~beta ~depth ~ply ~check = fun () m ->
    let open Continue_or_stop in
    (* Skip this move if it has no chance of improving alpha. *)
    if futile m ~score ~alpha:t.alpha ~beta ~depth ~check
    then return @@ Continue ()
    else (* Get the score. *)
      lmr_pvs t m ~beta ~ply ~depth ~check >>= fun score ->
      (* Update alpha if needed. *)
      better t m ~score ~depth >>= fun () ->
      if score >= beta then
        (* Move was too good. *)
        cutoff t m ~ply >>| fun () -> Stop beta
      else return @@ Continue ()

  (* Mate distance pruning. *)
  and mdp ~alpha ~beta ~ply =
    let alpha = max alpha (-mate_score + ply) in
    let beta = min beta (mate_score - ply) in
    if alpha >= beta then First alpha
    else Second (alpha, beta)

  (* Try to reduce the depth. *)
  and reduce pos moves ~score ~alpha ~beta ~ply ~depth ~check ~depth ~null =
    if beta - alpha > 1 || check || null
    then return None
    else match rfp ~depth ~score ~beta with
      | Some _ as score -> return score
      | None -> nmr pos ~score ~beta ~ply ~depth >>= function
        | Some _ as beta -> return beta
        | None -> razor pos moves ~score ~alpha ~beta ~ply ~depth

  (* Reverse futility pruning.

     If our score is within a margin above beta, then it is likely too
     good, and should cause a cutoff.
  *)
  and rfp ~depth ~score ~beta =
    let score = score - 70 * depth in
    Option.some_if (depth <= futility_limit && score >= beta) score

  (* Null move reduction.

     If we forfeit our right to play a move and our opponent's best
     response still produces a beta cutoff, then we know this position
     is unlikely.
  *)
  and nmr pos ~score ~beta ~ply ~depth =
    let pawn = Position.pawn pos in
    let king = Position.king pos in
    let active = Position.active_board pos in
    if score >= beta
    && depth > reduction_factor
    && Bb.(((pawn + king) & active) <> active) then
      let r = 4 + (depth / 6) in
      let r = r + min 3 ((score - beta) / 3000) in
      Position.null_move_unsafe pos |> go
        ~alpha:(-beta)
        ~beta:((-beta) + 1)
        ~ply:(ply + 1)
        ~depth:(depth - r)
        ~null:true >>= negm >>| fun score ->
      Option.some_if (score >= beta) beta
    else return None

  (* Razoring.

     Drop down to quescience search if we're approaching the horizon and we 
     have little chance to improve alpha.
  *)
  and razor pos moves ~score ~alpha ~beta ~ply ~depth =
    if depth = 1 && score + razor_margin <= alpha then
      Quiescence.with_moves pos moves ~alpha ~beta ~ply >>| fun qscore ->
      Option.some_if (qscore <= alpha) qscore
    else return None

  and razor_margin = Piece.Kind.value Rook * Eval.material_weight

  (* Futility pruning.

     If our score is within a margin below alpha, then skip searching
     quiet moves (since they are likely to be "futile" in improving alpha).
  *)
  and futile m ~score ~alpha ~beta ~depth ~check =
    not check &&
    beta - alpha <= 1 &&
    is_quiet m &&
    not (Position.in_check @@ Legal.new_position m) &&
    depth <= futility_limit &&
    score + 115 + 90 * depth <= alpha

  and futility_limit = 6

  (* Principal variation search. *)
  and pvs t pos ~beta ~ply ~depth =
    let f alpha =
      go pos
        ~alpha
        ~beta:(-t.alpha)
        ~ply:(ply + 1)
        ~depth:(depth - 1) >>= negm in
    let alpha = if t.full_window then -beta else (-t.alpha) - 1 in
    f alpha >>= function
    | score when t.full_window -> return score
    | score when score > t.alpha && score < beta -> f (-beta)
    | score -> return score

  (* Amount to reduce the depth by. *)
  and reduction_factor = 2

  (* Late move reduction. *)
  and lmr t m ~beta ~ply ~depth ~check =
    (* Least expensive checks first. *)
    if not check
    && not t.full_window
    && depth > reduction_factor
    && beta - t.alpha <= 1
    && is_quiet m
    && not @@ Position.in_check @@ Legal.new_position m
    then State.(gets @@ is_killer m ply) >>= function
      | true -> return None
      | false ->
        Legal.new_position m |> go
          ~alpha:((-t.alpha) - 1)
          ~beta:(-t.alpha)
          ~ply:(ply + 1)
          ~depth:(depth - reduction_factor) >>= negm >>| Option.some
    else return None

  (* Combine LMR and PVS. *)
  and lmr_pvs t m ~beta ~ply ~depth ~check =
    let pvs () = Legal.new_position m |> pvs t ~beta ~ply ~depth in
    lmr t m ~beta ~ply ~depth ~check >>= function
    | Some score when score > t.alpha -> pvs ()
    | Some score -> return score
    | None -> pvs ()

  (* Search from the root position. This follows slightly different rules than
     the generic search:

     1. There is no need to consider beta cutoff. Instead, we cut off the
        search when a mate is found.
     2. We drop straight down into PVS.
     3. In addition to the score, we return the best move.
  *)
  let root moves ~depth =
    let open Continue_or_stop in
    State.(gets search) >>= fun search ->
    let pos = search.root in
    let ply = 0 in
    let beta = inf in
    Order.sort moves ~ply ~pos ~tt:search.tt >>= fun moves ->
    let t = create moves in
    let finish () = return t.alpha in
    State.List.fold_until moves ~init:() ~finish ~f:(fun () m ->
        Legal.new_position m |>
        pvs t ~ply ~depth ~beta >>= fun score ->
        better t m ~score ~depth >>= fun () ->
        check_limits >>| function
        | true -> Stop t.alpha
        | false when is_mate score -> Stop score
        | false -> Continue ()) >>| fun score ->
    (* Update the transposition table and return the score. *)
    let best = t.best in
    Tt.store search.tt pos ~depth ~score ~best ~bound:Exact;
    best, score
end

type iter = result -> unit

let default_iter : iter = fun _ -> ()

let convert_score score tt root ~pv ~mate ~mated =
  let open Uci.Send.Info in
  let len = List.length pv in
  if mate then Mate len
  else if mated then Mate (-len)
  else match Tt.find tt root with
    | None | Some Tt.Entry.{bound = Exact; _} -> Cp (score, None)
    | Some Tt.Entry.{bound = Lower; _} -> Cp (score, Some `lower)
    | Some Tt.Entry.{bound = Upper; _} -> Cp (score, Some `upper)

(* For debugging, make sure that the PV is a legal sequence of moves. *)
let assert_pv pv moves =
  ignore @@ List.fold pv ~init:moves ~f:(fun moves m ->
      assert (List.exists moves ~f:(Legal.same m));
      Position.legal_moves @@ Legal.new_position m)

(* Use iterative deepening to optimize the search. This works by using TT
   entries from shallower searches in the move ordering for deeper searches,
   which makes pruning more effective. *)
let rec iterdeep ?(prev = None) ?(depth = 1) st ~iter ~moves ~limit =
  let next = iterdeep ~iter ~depth:(depth + 1) ~moves ~limit in
  let (best, score), st = Monad.State.run (Main.root moves ~depth) st in
  let nodes = st.nodes in
  let {tt; root; _} = st.search in
  (* Record the time. *)
  let time =
    int_of_float @@
    Time.(Span.to_ms @@ diff (now ()) st.start_time) in
  let mate = is_mate score in
  let mated = is_mated score in
  (* If the search was stopped, use the previous completed result
     (if available). *)
  if !(st.search.stop) then
    match prev with
    | Some result -> result
    | None ->
      let pv = [best] in
      let score = convert_score score tt root ~pv ~mate ~mated in
      Result.Fields.create ~pv ~score ~nodes ~depth ~time
  else begin
    (* Last iteration may have eaten up at least half the allocated time,
       so the next (deeper) iteration is likely to take longer. Thus, we
       should abort the search. *)
    let too_long =
      Limits.time st.search.limits |>
      Option.value_map ~default:false ~f:(fun n -> time * 2 >= n) in
    (* Extract the current PV. *)
    let pv = Tt.pv tt depth best ~mate:(mate || mated) in
    assert_pv pv moves;
    (* The result for this iteration. *)
    let score = convert_score score tt root ~pv ~mate ~mated in
    let result = Result.Fields.create ~pv ~score ~nodes ~depth ~time in
    iter result;
    if mate || depth >= limit || too_long then result
    else next ~prev:(Some result) @@ State.new_iter st
  end

let go
    ?(tt = Tt.create ())
    ?(iter = default_iter)
    ?(stop = ref false)
    ~root
    ~limits
    ~history
    () =
  match Position.legal_moves root with
  | [] -> invalid_arg "No legal moves"
  | moves ->
    let limit = match limits.Limits.kind with
      | Depth n -> n
      | _ -> Int.max_value in
    iterdeep ~iter ~moves ~limit @@
    State.create @@
    create ~root ~limits ~history ~tt ~stop
