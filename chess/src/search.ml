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
  let pv tt pos n =
    let rec aux i acc pos = match find tt pos with
      | Some Entry.{best; _} when n > i ->
        aux (i + 1) (best :: acc) @@ Legal.new_position best
      | _ -> List.rev acc in
    aux 0 [] pos
end

type t = {
  limits  : limits;
  root    : Position.t;
  history : int Int64.Map.t;
  tt      : Tt.t;
} [@@deriving fields]

let create ~limits ~root ~history ~tt =
  let history =
    (* Make sure that the root position is in our history. *)
    Position.hash root |> Map.update history ~f:(function
        | Some n -> n | None -> 1) in
  Fields.create ~limits ~root ~history ~tt

module Result = struct
  type t = {
    pv    : Position.legal list;
    score : int;
    nodes : int;
    depth : int;
    time  : int;
  } [@@deriving fields]

  let best {pv; _} = List.hd_exn pv
end

type result = Result.t
type search = t

let create = Fields.create

let inf = 65535
let max_score = inf / 2

let is_mate score = score = max_score
let is_mated score = score = (-max_score)

let is_quiet m =
  Option.is_none @@ Legal.capture m &&
  Option.is_none @@ Move.promote @@ Legal.move m

let is_noisy = Fn.non is_quiet

(* Our state for the entirety of the search. *)
module State = struct
  let max_limit = 4096

  module T = struct
    type t = {
      start_time : Time.t;
      limit      : int;
      nodes      : int;
      search     : search;
      killer1    : Position.legal Int.Map.t;
      killer2    : Position.legal Int.Map.t;
      history    : int array;
    } [@@deriving fields]

    let create search = {
      start_time = Time.now ();
      limit = 0;
      nodes = 0;
      search;
      killer1 = Int.Map.empty;
      killer2 = Int.Map.empty;
      history = Array.create ~len:Square.(count * count) 0;
    }

    (* Reset the limit check counter. *)
    let new_limit st = {st with limit = 0}

    (* Increment the limit check counter. *)
    let inc_limit st = {st with limit = st.limit + 1}

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

let return = State.return

let check_limits =
  (* Update the counter and see if we should check the limits. *)
  State.(update inc_limit) >>= fun () ->
  State.(gets limit) >>= fun limit ->
  if limit >= State.max_limit then
    (* Reset the counter *)
    State.(update new_limit) >>= fun () ->
    (* Check the limits. *)
    State.get () >>| fun {start_time; search; nodes; _} ->
    let elapsed =
      int_of_float @@
      Time.(Span.to_ms @@ diff (now ()) start_time) in
    let limits = search.limits in
    match limits.nodes with
    | Some n when nodes >= n -> true
    | _ -> match limits.kind with
      | Infinite | Depth _ -> false
      | Time t -> elapsed >= t
  else return false

(* Will playing this position likely lead to a repetition draw? *)
let check_repetition search pos =
  Position.hash pos |> Map.find search.history |>
  Option.value_map ~default:false ~f:(fun n -> n > 0)

(* Move ordering is critical for optimizing the performance of alpha-beta
   pruning. We use some heuristics to determine which moves are likely
   to be the best, and then search those first, hoping that the worse
   moves get pruned more effectively. *)
module Ordering = struct
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
    State.(gets history) >>| fun history ->
    let killer m = match killer1, killer2 with
      | Some k, _ when Legal.same m k -> killer1_bonus
      | _, Some k when Legal.same m k -> killer2_bonus
      | _ -> 0 in
    let history m =
      let m = Legal.move m in
      let src = Square.to_int @@ Move.src m in
      let dst = Square.to_int @@ Move.dst m in
      let i = src + dst * Square.count in
      Array.unsafe_get history i in
    sort_aux moves ~eval:(fun m ->
        if best m then max_score
        else
          let capture = capture m in
          if capture <> 0 then capture
          else
            let promote = promote m in
            if promote <> 0 then promote
            else
              let killer = killer m in
              if killer <> 0 then killer
              else history m)

  (* Sort for quiescence search. *)
  let qsort moves =
    sort_aux moves ~eval:(fun m ->
        let capture = capture m in
        if capture <> 0 then capture
        else
          let promote = promote m in
          if promote <> 0 then promote
          else 0)
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
    then return @@ if check then -max_score else 0
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
        let finish = return in
        List.filter moves ~f:is_noisy |> Ordering.qsort |>
        State.List.fold_until ~init ~finish ~f:(fun alpha m ->
            Legal.new_position m |>
            go ~alpha:(-beta) ~beta:(-alpha) ~ply:(ply + 1) >>=
            negm >>| fun score -> if score >= beta
            then Stop beta else Continue (max score alpha))
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
  let drawn search pos =
    check_repetition search pos ||
    Position.halfmove pos >= 100 ||
    Position.is_insufficient_material pos

  (* Search from a new position. *)
  let rec go ?(null = false) pos ~alpha ~beta ~ply ~depth =
    State.(gets nodes) >>= fun nodes ->
    State.(gets search) >>= fun search ->
    if drawn search pos then return 0
    else match Tt.lookup search.tt ~pos ~depth ~alpha ~beta with
      | First score -> return score
      | Second (alpha, beta) ->
        let moves = Position.legal_moves pos in
        let check = Position.in_check pos in
        if List.is_empty moves
        then return @@ if check then -max_score else 0
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
    | score when ps.full_window -> return score
    | score when score > ps.alpha && score < beta -> f (-beta)
    | score -> return score

  (* Amount to reduce the depth by. *)
  and reduction_factor = 2

  (* Late move reduction. *)
  and lmr ps pos m ~beta ~ply ~depth ~check =
    let open Plysearch in
    if beta - ps.alpha <= 1
    && not ps.full_window
    && depth > reduction_factor
    && not check
    && is_quiet m
    && not @@ Position.in_check @@ Legal.new_position m
    then State.(gets @@ is_killer m ply) >>= function
      | true -> return None
      | false ->
        Legal.new_position m |> go
          ~alpha:((-ps.alpha) - 1)
          ~beta:(-ps.alpha)
          ~ply:(ply + 1)
          ~depth:(depth - reduction_factor) >>= negm >>| Option.some
    else return None

  (* Combine LMR and PVS. *)
  and lmr_pvs ps pos m ~beta ~ply ~depth ~check =
    let pvs () = Legal.new_position m |> pvs ps ~beta ~ply ~depth in
    lmr ps pos m ~beta ~ply ~depth ~check >>= function
    | Some score when score > ps.alpha -> pvs ()
    | Some score -> return score
    | None -> pvs ()

  (* The actual search, given all the legal moves. *)
  and with_moves ?(null = false) pos moves ~alpha ~beta ~ply ~depth ~check =
    check_limits >>= function
    | true -> return 0
    | false ->
      (* Extend the depth limit if we're in check. Since the number of
         responses to a check is typically very low, the search should
         finish quickly. *)
      let depth = depth + Bool.to_int check in
      if depth <= 0
      then Quiescence.with_moves pos moves ~alpha ~beta ~ply
      else
        let score = Eval.go pos in
        reduce pos moves
          ~score ~alpha ~beta ~ply ~depth ~check ~depth ~null >>= function
        | Some score -> return score
        | None -> let open Continue_or_stop in
          iid pos moves ~alpha ~beta ~ply ~depth ~check >>= fun () ->
          State.(gets search) >>= fun search ->
          let ps = Plysearch.create moves ~alpha in
          Ordering.sort moves ~ply ~pos ~tt:search.tt >>= fun moves ->
          let finish () = return ps.alpha in
          State.List.fold_until moves ~init:() ~finish ~f:(fun () m ->
              if futile m ~score ~alpha:ps.alpha ~beta ~depth ~check
              then return @@ Continue ()
              else lmr_pvs ps pos m ~beta ~ply ~depth ~check >>= fun score ->
                if score >= beta
                then Plysearch.cutoff ps ply depth m >>| fun () -> Stop beta
                else return @@ Continue (Plysearch.better ps m score))
          >>| fun score ->
          Tt.store search.tt pos ~depth ~score ~best:ps.best ~bound:ps.bound;
          score

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

  (* Internal iterative deepening. *)
  and iid pos moves ~alpha ~beta ~ply ~depth ~check =
    if not check
    && beta - alpha > 1
    && depth >= futility_limit then begin
      with_moves pos moves
        ~alpha
        ~beta
        ~ply
        ~depth:(depth - 2)
        ~check >>| ignore
    end else return ()

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
    if score >= beta
    && depth > reduction_factor
    && not @@ Eval.is_endgame pos then
      Position.null_move_unsafe pos |> go
        ~alpha:(-beta)
        ~beta:((-beta) + 1)
        ~ply:(ply + 1)
        ~depth:(depth - 1 - reduction_factor)
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

  (* The search we start from the root position. *)
  let root moves ~depth =
    let open Continue_or_stop in
    State.(gets search) >>= fun search ->
    let pos = search.root in
    Ordering.sort moves ~ply:0 ~pos ~tt:search.tt >>= fun moves ->
    let ps = Plysearch.create moves in
    let finish () = return ps.alpha in
    State.List.fold_until moves ~init:() ~finish ~f:(fun () m ->
        Legal.new_position m |>
        pvs ps ~ply:1 ~depth ~beta:inf >>= fun score ->
        check_limits >>| function
        | true ->
          (* We could've reached the limit without getting a chance
             to improve alpha. *)
          if ps.full_window then Plysearch.better ps m score;
          Stop ps.alpha
        | false ->
          (* Stop if we've found a mating sequence. *)
          Plysearch.better ps m score;
          if score = max_score then Stop score else Continue ())
    >>| fun score ->
    (* Update the transposition table and return the score. *)
    Tt.store search.tt pos ~depth ~score ~best:ps.best ~bound:Exact;
    score
end

type iter = result -> bool

let default_iter : iter = fun _ -> true

(* Use iterative deepening to optimize the search. This works by using TT
   entries from shallower searches in the move ordering for deeper searches,
   which makes pruning more effective. *)
let rec iterdeep ?(depth = 1) st ~iter ~moves ~limit =
  let next = iterdeep ~iter ~depth:(depth + 1) ~moves ~limit in
  let score, st = Monad.State.run (Main.root moves ~depth) st in
  let time =
    int_of_float @@
    Time.(Span.to_ms @@ diff (now ()) st.start_time) in
  (* Last iteration may have eaten up at least half the allocated time,
     so the next (deeper) iteration is likely to take longer. Thus, we
     should abort the search. *)
  let too_long = match st.search.limits.kind with
    | Time n -> time * 2 >= n
    | _ -> false in
  (* Extract the current PV. *)
  let pv = Tt.pv st.search.tt st.search.root depth in
  (* The result for this iteration. *)
  let result = 
    Result.Fields.create ~pv ~score ~nodes:st.nodes ~depth ~time in
  (* Check the depth limit. If it doesn't exist we can just use the largest
     positive integer since it will never reach that depth in our lifetime.
     Also, if we found a mating sequence, then there's no reason to iterate
     again since it will most likely return the same result. *)
  if not (iter result)
  || score = max_score
  || depth >= limit
  || too_long
  then result else next @@ State.new_iter st

let go
    ?(tt = Tt.create ())
    ?(iter = default_iter)
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
    create ~root ~limits ~history ~tt
