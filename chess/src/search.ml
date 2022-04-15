open Core_kernel
open Monads.Std
open Bap_future.Std

module Bb = Bitboard
module Legal = Position.Legal

module Limits = struct
  type kind =
    | Infinite
    | Depth of int
    | Time of int

  type t = {
    infinite : bool;
    nodes    : int option;
    mate     : int option;
    depth    : int option;
    time     : int option;
    stop     : unit future;
  } [@@deriving fields]

  let stopped limits = Future.is_decided @@ stop limits

  let check_nodes = function
    | None -> ()
    | Some n when n >= 1 -> ()
    | Some n ->
      invalid_argf "Invalid node limit %d, must be greater than 0" n ()

  let check_mate = function
    | None -> ()
    | Some n when n >= 1 -> ()
    | Some n ->
      invalid_argf "Invalid mate limit %d, must be greater than 0" n ()

  let check_depth = function
    | None -> ()
    | Some n when n >= 1 -> ()
    | Some n ->
      invalid_argf "Invalid depth limit %d, must be greater than 0" n ()

  let check_movetime = function
    | None -> None
    | (Some n) as movetime when n >= 1 -> movetime
    | Some n ->
      invalid_argf "Invalid movetime %d, must be greater than 0" n ()

  let gametime
      ?(movestogo = None)
      ~wtime
      ~winc
      ~btime
      ~binc
      ~active
      () =
    (* Validate inputs. *)
    if wtime < 1 then
      invalid_argf "Invalid wtime %d, must be greater than 0" wtime ();
    if winc < 0 then
      invalid_argf "Invalid winc %d, must be positive" winc ();
    if btime < 1 then
      invalid_argf "Invalid btime %d, must be greater than 0" btime ();
    if binc < 0 then
      invalid_argf "Invalid binc %d, must be positive" binc ();
    Option.iter movestogo ~f:(function
        | n when n < 0 ->
          invalid_argf "Invalid movestogo %d, must be positive" n ()
        | _ -> ());
    (* Calculate the amount of time to search. *)
    let our_time, our_inc, their_time = match active with
      | Piece.White -> wtime, winc, btime
      | Piece.Black -> btime, binc, wtime in
    let time = match movestogo with
      | Some n -> our_time / (n + 3)
      | None ->
        let ratio =
          Float.(min (max (of_int our_time / of_int their_time) 1.0) 2.0) in
        our_time / int_of_float (20.0 *. ratio) in
    time + our_inc

  let default_depth = 7

  let create
      ?(nodes = None)
      ?(mate = None)
      ?(depth = Some default_depth)
      ?(movetime = None)
      ?(movestogo = None)
      ?(wtime = None)
      ?(winc = None)
      ?(btime = None)
      ?(binc = None)
      ?(infinite = false)
      ~active
      ~stop
      () =
    check_nodes nodes;
    check_mate mate;
    check_depth depth;
    let movetime = check_movetime movetime in
    let gametime = match wtime, btime with
      | None, None -> None
      | Some _, None -> invalid_arg "Missinc btime"
      | None, Some _ -> invalid_arg "Missing wtime"
      | Some wtime, Some btime ->
        let winc, binc = match winc, binc with
          | None, None -> 0, 0
          | Some winc, Some binc -> winc, binc
          | Some _, None -> invalid_arg "Missing binc"
          | None, Some _ -> invalid_arg "Missing winc" in
        Some (gametime ~wtime ~winc ~btime ~binc ~movestogo ~active ()) in
    let time = Option.merge movetime gametime ~f:min in
    if not infinite
    && Option.is_none nodes
    && Option.is_none mate
    && Option.is_none depth
    && Option.is_none time
    then invalid_arg "Limits were explicitly unspecified"
    else {infinite; nodes; mate; depth; time; stop}
end

type limits = Limits.t

(* Constants. *)
let inf = 65535
let mate_score = inf / 2
let max_ply = 64

(* Mate scores should be relative to the distance from the root position.
   Shorter distances are to be preferred. *)
let is_mate score = score >= mate_score - max_ply
let is_mated score = score <= (-mate_score) + max_ply

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
  let lookup tt ~pos ~depth ~ply ~alpha ~beta = match find tt pos with
    | None -> Second (alpha, beta, false)
    | Some entry when Entry.depth entry < depth -> Second (alpha, beta, true)
    | Some Entry.{score; bound; _} ->
      let score =
        if is_mate score then score - ply
        else if is_mated score then score + ply
        else score in
      match bound with
      | Exact -> First score
      | Lower ->
        let alpha = max alpha score in
        if alpha >= beta then First score else Second (alpha, beta, true)
      | Upper ->
        let beta = min beta score in
        if alpha >= beta then First score else Second (alpha, beta, true)

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
type search = {
  limits  : limits;
  root    : Position.t;
  history : int Int64.Map.t;
  tt      : Tt.t;
} [@@deriving fields]

let create_search ~limits ~root ~history ~tt =
  let history =
    (* Make sure that the root position is in our history. *)
    Position.hash root |> Map.update history ~f:(function
        | Some n -> n | None -> 1) in
  Fields_of_search.create ~limits ~root ~history ~tt

let is_quiet m =
  Option.is_none @@ Legal.capture m &&
  Option.is_none @@ Move.promote @@ Legal.move m

let is_noisy = Fn.non is_quiet

(* Our state for the entirety of the search. *)
module State = struct
  module T = struct
    type t = {
      start_time   : Time.t;
      nodes        : int;
      search       : search;
      killer1      : Position.legal Option_array.t;
      killer2      : Position.legal Option_array.t;
      move_history : int array;
      best_score   : int;
    } [@@deriving fields]

    let create search = {
      start_time = Time.now ();
      nodes = 0;
      search;
      killer1 = Option_array.create ~len:max_ply;
      killer2 = Option_array.create ~len:max_ply;
      move_history = Array.create ~len:Square.(count * count) 0;
      best_score = -inf;
    }

    (* Start a new search while reusing the transposition table. *)
    let new_iter best_score st = {st with nodes = 0; best_score}

    (* Increment the number of nodes we've evaluated. *)
    let inc_nodes st = {st with nodes = st.nodes + 1}

    (* Get the first killer move. *)
    let killer1 ply st = Option_array.get st.killer1 ply

    (* Get the second killer move. *)
    let killer2 ply st = Option_array.get st.killer2 ply

    (* Is `m` a killer move? *)
    let is_killer m ply st = match killer1 ply st with
      | Some m' when Legal.same m m' -> true
      | _ -> match killer2 ply st with
        | Some m' -> Legal.same m m'
        | None -> false

    (* Update the killer move for a particular ply. *)
    let killer ply m st = if is_quiet m then begin
        begin match killer1 ply st with
          | None -> ()
          | Some m ->
            Option_array.set_some st.killer2 ply m
        end;
        Option_array.set_some st.killer1 ply m
      end

    (* Update the move history heuristic. *)
    let history m depth st = if is_quiet m then begin
        let m = Legal.move m in
        let src = Square.to_int @@ Move.src m in
        let dst = Square.to_int @@ Move.dst m in
        let i = src + dst * Square.count in
        let d = Array.unsafe_get st.move_history i + (depth * depth) in
        Array.unsafe_set st.move_history i d
      end
  end

  include T
  include Monad.State.Make(T)(Monad.Ident)
end

open State.Syntax

let return = State.return

let check_limits =
  State.get () >>| fun {start_time; search; nodes; _} ->
  let limits = search.limits in
  Limits.stopped limits || begin
    match Limits.nodes limits with
    | Some n when nodes >= n -> true
    | _ -> match Limits.time limits with
      | None -> false
      | Some t ->
        let elapsed =
          int_of_float @@
          Time.(Span.to_ms @@ diff (now ()) start_time) in
        elapsed >= t
  end

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

  (* Score each move according to `eval`. *)
  let score_aux moves ~eval =
    Array.of_list @@ List.map moves ~f:(fun m -> m, eval m)

  (* Returns a thunk that incrementally applies insertion sort to the list.
     In the context of alpha-beta pruning, we may not actually visit all the
     moves for a given position, so it makes no sense to waste time sorting
     the entire thing. *)
  let make_picker moves =
    let current = ref 0 in
    let n = Array.length moves in
    fun () ->
      let c = !current in
      if c < n then
        let best_score = ref (-inf) in
        let best_index = ref c in
        for i = c to n - 1 do
          let _, score = Array.unsafe_get moves i in
          if score > !best_score then begin
            best_score := score;
            best_index := i;
          end
        done;
        let i = !best_index in
        let result = Array.unsafe_get moves i in
        if i > c then begin
          let old = Array.unsafe_get moves c in
          Array.unsafe_set moves i old;
        end;
        incr current;
        Some result
      else None

  (* Score the moves for normal search. *)
  let score moves ~ply ~pos ~tt =
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
    let moves = score_aux moves ~eval:(fun m ->
        if best m then inf
        else let capture = capture m in
          if capture <> 0 then capture
          else let promote = promote m in
            if promote <> 0 then promote
            else let killer = killer m in
              if killer <> 0 then killer
              else history m) in
    (* In case we never improve alpha, return the first available move as an
       option for the player (any arbitrary selection would do). *)
    fst @@ Array.unsafe_get moves 0, make_picker moves

  (* Score the moves for quiescence search. *)
  let qscore moves = make_picker @@ score_aux moves ~eval:(fun m ->
      let capture = capture m in
      if capture <> 0 then capture
      else promote m)

  (* Iterate using the thunk. *)
  let fold_until =
    let open Continue_or_stop in
    let rec aux acc next ~f ~finish = match next () with
      | None -> finish acc
      | Some x -> f acc x >>= function
        | Continue y -> aux y next ~f ~finish
        | Stop z -> return z in
    fun next ~init -> aux init next
end

let negm = Fn.compose return Int.neg

(* Quiescence search is used when we reach our maximum depth for the main
   search. The goal is then to keep searching only "noisy" positions, until
   we reach one that is "quiet", and then return our evaluation. *)
module Quiescence = struct
  let margin = Piece.Kind.value Queen * Eval.material_weight

  let rec go pos ~alpha ~beta ~ply = check_limits >>= function
    | true -> return 0
    | false ->
      let moves = Position.legal_moves pos in
      let check = Position.in_check pos in
      if List.is_empty moves
      then return @@ if check then -mate_score + ply else 0
      else with_moves pos moves ~alpha ~beta ~ply

  and with_moves pos moves ~alpha ~beta ~ply =
    State.(update inc_nodes) >>= fun () ->
    let score = Eval.go pos in
    if score >= beta then return beta
    else if score + margin < alpha && not @@ Eval.is_endgame pos
    then return alpha
    else List.filter moves ~f:is_noisy |>
         Order.qscore |> Order.fold_until
           ~init:(max score alpha)
           ~finish:return
           ~f:(branch ~beta ~ply)

  and branch ~beta ~ply = fun alpha (m, _) ->
    let open Continue_or_stop in
    Legal.new_position m |>
    go ~alpha:(-beta) ~beta:(-alpha) ~ply:(ply + 1) >>= negm >>| fun score ->
    if score >= beta then Stop beta else Continue (max score alpha)
end

(* The main search of the game tree. The core of it is the negamax algorithm
   with alpha-beta pruning (and other enhancements). *)
module Main = struct
  (* The results for a single ply. *)
  type t = {
    mutable best  : Position.legal;
    mutable alpha : int;
    mutable bound : Tt.bound;
  }

  let create ?(alpha = -inf) ~best () = {
    best;
    alpha;
    bound = Tt.Upper;
  }

  (* Beta cutoff. *)
  let cutoff t m ~ply =
    t.best <- m;
    t.bound <- Tt.Lower;
    State.(gets @@ killer ply m)

  (* Alpha may have improved. *)
  let better t m ~score ~depth =
    if score > t.alpha then begin
      t.best <- m;
      t.alpha <- score;
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
      else match Tt.lookup search.tt ~pos ~depth ~ply ~alpha ~beta with
        | First score -> return score
        | Second (alpha, beta, hit) ->
          let moves = Position.legal_moves pos in
          let check = Position.in_check pos in
          (* Checkmate or stalemate. *)
          if List.is_empty moves
          then return @@ if check then -mate_score + ply else 0
          (* Check extension. *)
          else let depth = depth + Bool.to_int check in
            if depth <= 0 then
              (* Depth exhausted, drop down to quiescence search. *)
              Quiescence.with_moves pos moves ~alpha ~beta ~ply
            else match mdp ~alpha ~beta ~ply with
              | First alpha -> return alpha
              | Second (alpha, beta) ->
                (* Use the null move hypothesis to reduce the depth of the
                   search. *)
                let eval = Eval.go pos in
                reduce pos moves
                  ~eval ~alpha ~beta ~ply ~depth
                  ~check ~depth ~null >>= function
                | Some score -> return score
                | None ->
                  (* Search the available moves. *)
                  with_moves pos moves
                    ~alpha ~beta ~ply ~depth ~eval ~check

  (* Search the available moves for the given position. *)
  and with_moves pos moves ~alpha ~beta ~ply ~depth ~eval ~check =
    State.(gets search) >>= fun {tt; _} ->
    Order.score moves ~ply ~pos ~tt >>= fun (best, next) ->
    let t = create ~alpha ~best () in
    let finish _ = return t.alpha in
    let f = branch t ~eval ~beta ~depth ~ply ~check in
    Order.fold_until next ~init:0 ~finish ~f >>| fun score ->
    Tt.store tt pos ~depth ~score ~best:t.best ~bound:t.bound;
    score

  (* Search a branch of the current node. *)
  and branch t ~eval ~beta ~depth ~ply ~check = fun i (m, _) ->
    let open Continue_or_stop in
    (* If this move is obviously bad, then skip it. *)
    if futile m ~eval ~alpha:t.alpha ~beta ~depth ~check
    then return @@ Continue (i + 1)
    else begin
      lmr t m ~i ~beta ~ply ~depth ~check >>= function
      | Some score -> return score
      | None -> Legal.new_position m |> pvs t ~i ~beta ~ply ~depth
    end >>= fun score ->
      (* Update alpha if needed. *)
      better t m ~score ~depth >>= fun () ->
      if score >= beta then
        (* Move was too good. *)
        cutoff t m ~ply >>| fun () -> Stop beta
      else return @@ Continue (i + 1)

  (* Mate distance pruning. *)
  and mdp ~alpha ~beta ~ply =
    let alpha = max alpha (-mate_score + ply) in
    let beta = min beta (mate_score - ply) in
    if alpha >= beta then First alpha
    else Second (alpha, beta)

  (* Try to reduce the depth. *)
  and reduce pos moves ~eval ~alpha ~beta ~ply ~depth ~check ~depth ~null =
    if beta - alpha > 1 || check || null
    then return None
    else match rfp ~depth ~eval ~beta with
      | Some _ as score -> return score
      | None -> nmr pos ~eval ~beta ~ply ~depth >>= function
        | Some _ as beta -> return beta
        | None -> razor pos moves ~eval ~alpha ~beta ~ply ~depth

  (* Reverse futility pruning.

     If our score is within a margin above beta, then it is likely too
     good, and should cause a cutoff.
  *)
  and rfp ~depth ~eval ~beta =
    Option.some_if
      (depth <= futility_limit && eval - rfp_margin depth >= beta)
      eval

  and rfp_margin depth =
    Piece.(Kind.value Pawn) * Eval.material_weight * depth

  (* Null move reduction.

     If we forfeit our right to play a move and our opponent's best
     response still produces a beta cutoff, then we know this position
     is unlikely.
  *)
  and nmr pos ~eval ~beta ~ply ~depth =
    let pawn = Position.pawn pos in
    let king = Position.king pos in
    let active = Position.active_board pos in
    if eval >= beta
    && depth > nmr_limit
    && Bb.(((pawn + king) & active) <> active) then
      Position.null_move_unsafe pos |> go
        ~alpha:(-beta)
        ~beta:((-beta) + 1)
        ~ply:(ply + 1)
        ~depth:(depth - 1 - nmr_limit)
        ~null:true >>= negm >>| fun score ->
      Option.some_if (score >= beta) beta
    else return None

  and nmr_limit = 2

  (* Razoring.

     Drop down to quescience search if we're approaching the horizon and we 
     have little chance to improve alpha.
  *)
  and razor pos moves ~eval ~alpha ~beta ~ply ~depth =
    if depth = 1 && eval + razor_margin <= alpha then
      Quiescence.with_moves pos moves ~alpha ~beta ~ply >>| fun score ->
      Option.some_if (score <= alpha) score
    else return None

  and razor_margin = Piece.Kind.value Rook * Eval.material_weight

  (* Futility pruning.

     If our score is within a margin below alpha, then skip searching
     quiet moves (since they are likely to be "futile" in improving alpha).
  *)
  and futile m ~eval ~alpha ~beta ~depth ~check =
    not check &&
    beta - alpha <= 1 &&
    is_quiet m &&
    not (Position.in_check @@ Legal.new_position m) &&
    depth <= futility_limit &&
    eval + 115 + 90 * depth <= alpha

  and futility_limit = 6

  (* Late move reduction.

     For quiet moves (closer to the end of our ordering), see if a reduced
     depth search will improve alpha or not.
  *)
  and lmr t m ~i ~beta ~ply ~depth ~check =
    (* Least expensive checks first. *)
    if not check
    && depth > lmr_limit
    && i > (if beta - t.alpha > 1 then 3 else 2)
    && is_quiet m
    && not @@ Position.in_check @@ Legal.new_position m
    then State.(gets @@ is_killer m ply) >>= function
      | true -> return None
      | false ->
        Legal.new_position m |> go
          ~alpha:((-t.alpha) - 1)
          ~beta:(-t.alpha)
          ~ply:(ply + 1)
          ~depth:(depth - lmr_limit) >>= negm >>| fun score ->
        Option.some_if (score <= t.alpha) score
    else return None

  and lmr_limit = 2

  (* Principal variation search. *)
  and pvs t pos ~i ~beta ~ply ~depth =
    let f alpha =
      go pos
        ~alpha
        ~beta:(-t.alpha)
        ~ply:(ply + 1)
        ~depth:(depth - 1) >>= negm in
    if i = 0 then f (-beta)
    else f (-t.alpha - 1) >>= fun score ->
      if beta - t.alpha > 1
      && score > t.alpha
      && score < beta then f (-beta)
      else return score

  (* Search from the root position. This follows slightly different rules than
     the generic search:

     1. There is no need to consider beta cutoff. Instead, we cut off the
        search when a mate is found.
     2. We drop straight down into PVS.
     3. In addition to the score, we return the best move.
  *)
  let root moves ~alpha ~beta ~depth =
    let open Continue_or_stop in
    State.(gets search) >>= fun search ->
    let pos = search.root in
    let ply = 0 in
    Order.score moves ~ply ~pos ~tt:search.tt >>= fun (best, next) ->
    let t = create ~alpha ~best () in
    let finish _ = return t.alpha in
    Order.fold_until next ~init:0 ~finish ~f:(fun i (m, _) ->
        Legal.new_position m |>
        pvs t ~i ~ply ~depth ~beta >>= fun score ->
        better t m ~score ~depth >>= fun () ->
        check_limits >>| function
        | true -> Stop t.alpha
        | false when is_mate score -> Stop score
        | false -> Continue (i + 1)) >>| fun score ->
    (* Update the transposition table and return the score. *)
    let best = t.best in
    Tt.store search.tt pos ~depth ~score ~best ~bound:Exact;
    best, score

  (* Use an aspiration window for the search. The basic idea is that if
     we have the score from a shallower search, then we can use that value
     as the basis for a window to search around (the initial values of
     alpha and beta), and hopefully narrow the search space. *)
  let rec aspire ?(delta_low = 250) ?(delta_high = 250) moves depth =
    State.(gets best_score) >>= fun best_score ->
    let alpha = if depth > 1 then best_score - delta_low  else -inf in
    let beta  = if depth > 1 then best_score + delta_high else  inf in
    root moves ~alpha ~beta ~depth >>= fun (best, score) ->
    State.(gets search) >>= fun {limits; _} ->
    (* Search was stopped, or we landed inside the window. *)
    if Limits.stopped limits || (score > alpha && score < beta)
    then return (best, score)
    else (* Result was outside the window, so we need to widen a bit. *)
      let delta_low, delta_high =
        if score >= beta then delta_low, delta_high * 2
        else delta_low * 2, delta_high in
      aspire moves depth ~delta_low ~delta_high
end

type iter = result -> unit

let default_iter : iter = fun _ -> ()

let convert_score score tt root ~pv ~mate ~mated =
  let open Uci.Send.Info in
  let len = List.length pv in
  let full = (len + (len land 1)) / 2 in
  if mate then Mate full
  else if mated then Mate (-full)
  else match Tt.find tt root with
    | None | Some Tt.Entry.{bound = Exact; _} -> Cp (score, None)
    | Some Tt.Entry.{bound = Lower; _} -> Cp (score, Some `lower)
    | Some Tt.Entry.{bound = Upper; _} -> Cp (score, Some `upper)

(* For debugging, make sure that the PV is a legal sequence of moves. *)
let assert_pv pv moves = List.fold pv ~init:moves ~f:(fun moves m ->
    if not @@ List.exists moves ~f:(Legal.same m) then
      failwithf "Found an invalid move %s in the PV"
        (Position.San.of_legal m) ();
    Position.legal_moves @@ Legal.new_position m) |> ignore

(* Use iterative deepening to optimize the search. This works by using TT
   entries from shallower searches in the move ordering for deeper searches,
   which makes pruning more effective. *)
let rec iterdeep ?(prev = None) ?(depth = 1) st ~iter ~moves =
  let next = iterdeep ~iter ~depth:(depth + 1) ~moves in
  let (best, score), st = Monad.State.run (Main.aspire moves depth) st in
  let time =
    int_of_float @@
    Time.(Span.to_ms @@ diff (now ()) st.start_time) in
  let mate = is_mate score in
  let mated = is_mated score in
  let {limits; tt; root; _} = st.search in
  let nodes = st.nodes in
  (* If the search was stopped, use the previous completed result, if
     available. *)
  if Limits.stopped limits then match prev with
    | Some result -> result
    | None ->
      let pv = [best] in
      let score = convert_score score tt root ~pv ~mate ~mated in
      Result.Fields.create ~pv ~score ~nodes ~depth ~time
  else begin
    (* Extract the current PV. *)
    let pv = Tt.pv tt depth best ~mate:(mate || mated) in
    assert_pv pv moves;
    (* The result for this iteration. *)
    let result =
      let score = convert_score score tt root ~pv ~mate ~mated in
      Result.Fields.create ~pv ~score ~nodes ~depth ~time in
    iter result;
    (* Last iteration may have eaten up at least half the allocated time,
       so the next (deeper) iteration is likely to take longer. Thus, we
       should abort the search. *)
    let too_long =
      Limits.time limits |>
      Option.value_map ~default:false ~f:(fun n -> time * 2 >= n) in
    (* Stop searching once we've reached the depth limit. *)
    let max_depth =
      Limits.depth limits |>
      Option.value_map ~default:false ~f:(fun n -> depth >= n) in
    (* Stop searching once we've reached the node limit. *)
    let max_nodes =
      Limits.nodes limits |>
      Option.value_map ~default:false ~f:(fun n -> nodes >= n) in
    (* Stop searching once we've found a mate in X (if applicable). *)
    let mate =
      mate &&
      Limits.mate limits |>
      Option.value_map ~default:false ~f:(fun n -> List.length pv <= n * 2) in
    (* Continue iterating? *)
    if too_long || max_nodes || max_depth || mate then result
    else next ~prev:(Some result) @@ State.new_iter score st
  end

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
    iterdeep ~iter ~moves @@
    State.create @@
    create_search ~root ~limits ~history ~tt
