open Core_kernel
open Chess
open Bap_future.Std
open Monads.Std

module State = struct
  module T = struct
    type t = {
      pos     : Position.t;
      history : (Zobrist.key, int) Hashtbl.t;
      tt      : Search.tt;
      stop    : unit promise option;
      ponder  : unit promise option;
      debug   : bool;
    } [@@deriving fields]
  end

  include T
  include Monad.State.Make(T)(Monad.Ident)

  (* Update the position and history after a move has been made. *)
  let update_position pos = update @@ fun st ->
    Position.hash pos |> Hashtbl.update st.history ~f:(function
        | None -> 1 | Some n -> n + 1);
    {st with pos}

  (* Set the new starting position and history. *)
  let set_position pos = update @@ fun st ->
    Hashtbl.clear st.history;
    Hashtbl.set st.history ~key:(Position.hash pos) ~data:1;
    {st with pos}

  let clear_tt = gets @@ fun {tt; _} -> Search.Tt.clear  tt
  let set_stop stop = update @@ fun st -> {st with stop}
  let set_ponder ponder = update @@ fun st -> {st with ponder}
  let set_debug debug = update @@ fun st -> {st with debug}
end

open State.Syntax

let return = State.return
let cont () = return true
let finish () = return false

let options = Hashtbl.of_alist_exn (module String) Uci.Send.Option.Type.[
    "Clear Hash", Button;
  ]

let uci =
  let open Uci.Send in
  let id = [
    Id (`name (sprintf "ocamlchess v%d.%d" Version.major Version.minor));
    Id (`author "Benjamin Mourad");
  ] in
  fun () ->
    List.iter id ~f:(fun cmd ->
        printf "%s\n%!" @@ to_string cmd);
    Hashtbl.iteri options ~f:(fun ~key:name ~data:typ ->
        printf "%s\n%!" @@ to_string (Option Option.{name; typ}));
    printf "%s\n%!" @@ to_string Uciok

let isready () = printf "%s\n%!" @@ Uci.Send.(to_string Readyok)

let setoption opt =
  let open Uci.Recv.Setoption in
  match Hashtbl.find options opt.name with
  | None ->
    printf "No such option: %s\n%!" opt.name;
    cont ()
  | Some Button -> begin
      match opt.name with
      | "Clear Hash" ->
        State.clear_tt >>= fun () ->
        cont ()
      | _ -> cont ()
    end
  | Some _ -> cont ()

let ucinewgame = State.set_position Position.start

let play_move m =
  State.(gets pos) >>= fun pos ->
  match Position.make_move pos m with
  | exception _ ->
    failwithf "Received illegal move %s for position %s\n%!"
      (Move.to_string m) (Position.Fen.to_string pos) ()
  | m -> State.update_position @@ Position.Legal.child m

let position pos moves = match Position.Valid.check pos with
  | Ok () ->
    State.set_position pos >>= fun () ->
    State.List.iter moves ~f:play_move >>= cont
  | Error err ->
    failwithf "Received invalid position %s: %s\n%!"
      (Position.Fen.to_string pos) (Position.Valid.Error.to_string err) ()

(* For each iteration in the search, send a UCI `info` command about the
   search. *)
let info_of_result root tt result =
  let depth = Search.Result.depth result in
  let seldepth = Search.Result.seldepth result in
  let nodes = Search.Result.nodes result in
  let pv = Search.Result.pv result in
  (* Avoid division by zero here (the search may terminate in under a
     millisecond). *)
  let time = max 1 @@ Search.Result.time result in
  let nps = (nodes * 1000) / time in
  let score = Search.Result.score result in
  let pv = List.map pv ~f:Position.Legal.move in
  printf "%s\n%!" @@ Uci.Send.(to_string @@ Info Info.[
      Depth depth;
      Seldepth seldepth;
      Nodes nodes;
      Score score;
      Nps nps;
      Time time;
      Pv pv;
    ])

(* Current search thread. *)
let search_thread = Atomic.make None

(* Condition variable for `infinite` and `ponder`. *)
let cv, cvm = Condition.create (), Mutex.create ()

let kill stop =
  Mutex.lock cvm;
  Option.iter stop ~f:(fun stop -> Promise.fulfill stop ());
  Condition.signal cv;
  Mutex.unlock cvm

let wait_for_stop stop ponder =
  let pondering () = match ponder with
    | Some p -> Future.is_decided p
    | None -> false in
  Mutex.lock cvm;
  while not (pondering () || Future.is_decided stop) do
    Condition.wait cv cvm;
  done;
  Mutex.unlock cvm

let bestmove result =
  let make ?p m =
    let move = Position.Legal.move m in
    let ponder = Option.map p ~f:Position.Legal.move in
    Uci.Send.Bestmove.{move; ponder} in
  let bestmove = match Search.Result.pv result with
    | [] -> None
    | [m] -> Some (make m)
    | m :: p :: _ -> Some (make m ~p) in
  printf "%s\n%!" @@ Uci.Send.(to_string @@ Bestmove bestmove)

(* The main search routine, should be run in a separate thread. *)
let search ~root ~limits ~history ~tt ~stop ~ponder =
  let result = try
      Search.go () ~root ~limits ~history ~tt ~ponder
        ~iter:(info_of_result root tt)
    with exn ->
      Format.eprintf "Search encountered an exception: %a\n%!" Exn.pp exn;
      Err.exit () in
  (* The UCI protocol says that `infinite` and `ponder` searches must
     wait for a corresponding `stop` or `ponderhit` command before
     sending `bestmove`. *)
  if Search.Limits.infinite limits then
    wait_for_stop stop ponder;
  (* Output the result. *)
  bestmove result;
  (* Thread completed. *)
  Atomic.set search_thread None

(* Abort if there's already a thread running. *)
let check_thread =
  State.(gets stop) >>| fun stop ->
  Atomic.get search_thread |> Option.iter ~f:(fun t ->
      kill stop;
      Thread.join t;
      failwith
        "Error: tried to start a new search while the previous one is \
         still running")

let new_thread ~root ~limits ~history ~tt ~stop ~ponder =
  Atomic.set search_thread @@ Option.return @@
  Thread.create (fun () -> search ~root ~limits ~history ~tt ~stop ~ponder) ()

let go g =
  check_thread >>= fun () ->
  (* Parse the search limits. *)
  let infinite = ref false in
  let nodes = ref None in
  let mate = ref None in
  let depth = ref None in
  let movetime = ref None in
  let wtime = ref None in
  let btime = ref None in
  let winc = ref None in
  let binc = ref None in
  let movestogo = ref None in
  let ponder = ref false in
  let opt r v s = match !r with
    | Some _ -> failwithf "Error in go command: option '%s' already exists" s ()
    | None -> r := Some v in
  (* As a hack, ponder mode will initially be set up as an infinite search.
     Then, when the ponderhit command is sent, the search can continue with
     the normal limits. *)
  let pondering () = ponder := true; infinite := true in
  (* If no parameters were given, then assume an infinite search. This is how
     Stockfish behaves. To be fair, the UCI protocol is very underspecified
     and underdocumented. It begs the question as to why it's still so widely
     supported. *)
  if List.is_empty g then infinite := true
  else List.iter g ~f:(fun go ->
      let open Uci.Recv.Go in
      match go with
      | Infinite      -> infinite := true
      | Nodes n       -> opt nodes n "nodes"
      | Mate n        -> opt mate n "mate"
      | Depth n       -> opt depth n "depth"
      | Movetime t    -> opt movetime t "movetime"
      | Wtime t       -> opt wtime t "wtime"
      | Btime t       -> opt btime t "btime"
      | Winc n        -> opt winc n "winc"
      | Binc n        -> opt binc n "binc"
      | Movestogo n   -> opt movestogo n "movestogo"
      | Ponder        -> pondering ()
      | Searchmoves _ -> failwith "Unsupported command: searchmoves");
  (* Construct the search limits. *)
  State.(gets pos) >>= fun root ->
  let active = Position.active root in
  let stop, stop_promise = Future.create () in
  let ponder, ponder_promise =
    if !ponder then
      let f, p = Future.create () in
      Some f, Some p
    else None, None in
  let limits = Search.Limits.create
      ~nodes:!nodes
      ~mate:!mate
      ~depth:!depth
      ~movetime:!movetime
      ~movestogo:!movestogo
      ~wtime:!wtime
      ~winc:!winc
      ~btime:!btime
      ~binc:!binc
      ~infinite:!infinite
      ~active
      ~stop
      () in
  (* Start the search. *)
  State.(gets history) >>= fun history ->
  State.(gets tt) >>= fun tt ->
  State.set_stop (Some stop_promise) >>= fun () ->
  State.set_ponder ponder_promise >>= fun () ->
  new_thread ~root ~limits ~history ~tt ~stop ~ponder;
  cont ()

let stop = State.update @@ function
  | {stop = Some stop; _} as st ->
    Mutex.lock cvm;
    Promise.fulfill stop ();
    Condition.signal cv;
    Mutex.unlock cvm;
    {st with stop = None}
  | st -> st

let ponderhit = State.update @@ function
  | {ponder = Some ponder; _} as st ->
    Mutex.lock cvm;
    Promise.fulfill ponder ();
    Condition.signal cv;
    Mutex.unlock cvm;
    {st with ponder = None}
  | st -> st

(* This is free software, so no need to register! *)
let register _ = cont ()

(* Interprets a command. Returns true if the main UCI loop shall continue. *)
let recv cmd =
  let open Uci.Recv in
  match cmd with
  | Uci -> cont @@ uci ()
  | Isready -> cont @@ isready ()
  | Setoption opt -> setoption opt
  | Ucinewgame -> ucinewgame >>= cont
  | Position (`fen pos, moves) -> position pos moves
  | Position (`startpos, moves) -> position Position.start moves
  | Go g -> go g
  | Stop -> stop >>= cont
  | Quit -> finish ()
  | Ponderhit -> ponderhit >>= cont
  | Debug `off -> State.set_debug false >>= cont
  | Debug `on -> State.set_debug true >>= cont
  | Register r -> register r

(* Main loop. *)
let rec loop () = match In_channel.(input_line stdin) with
  | None -> return ()
  | Some "" -> loop ()
  | Some line -> match Uci.Recv.of_string line with
    | None -> loop @@ printf "what?\n%!"
    | Some cmd -> recv cmd >>= function
      | false -> return ()
      | true -> loop ()

(* Default history has the starting position. *)
let history = Hashtbl.of_alist_exn (module Int64) [
    Position.(hash start), 1;
  ]

let exec () =
  State.stop @@
  Monad.State.exec (loop ()) @@
  State.Fields.create
    ~pos:Position.start
    ~history
    ~tt:(Search.Tt.create ())
    ~stop:None
    ~ponder:None
    ~debug:false

(* Entry point. *)
let run () =
  (* Run the main interpreter loop. *)
  let stop = try exec () with Failure msg ->
    eprintf "%s\n%!" msg;
    Err.exit () in
  (* Stop the search thread. *)
  Atomic.get search_thread |>
  Option.iter ~f:(fun t -> kill stop; Thread.join t);
