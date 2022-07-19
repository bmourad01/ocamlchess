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
  include Monad.State.T1(T)(Monad.Ident)

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

  let clear_tt = gets @@ fun {tt; _} -> Search.Tt.clear tt
  let set_stop stop = update @@ fun st -> {st with stop}
  let set_ponder ponder = update @@ fun st -> {st with ponder}
  let set_debug debug = update @@ fun st -> {st with debug}
end

open State.Syntax

type 'a state = 'a State.t

let return = State.return
let cont () = return true
let finish () = return false

module Options = struct
  module T = Uci.Send.Option.Type

  type _ t =
    | Spin : {spin : T.spin; mutable value : int} -> int t
    | Check : {default : bool; mutable value : bool} -> bool t
    | Combo : {combo : T.combo; mutable value : string} -> string t
    | String : {default : string; mutable value : string} -> string t
    | Button : unit t

  let spin s = Spin {spin = s; value = s.default}
  let check c = Check {default = c; value = c}
  let combo c = Combo {combo = c; value = c.default}
  let string s = String {default = s; value = s}
  let button = Button

  let to_uci : type a. a t -> T.t = function
    | Spin {spin; _} -> T.Spin spin
    | Check {default; _} -> T.Check default
    | Combo {combo; _} -> T.Combo combo
    | String {default; _} -> T.String default
    | Button -> T.Button

  type 'a callback = 'a t -> 'a -> unit state

  module Callbacks = struct
    let spin : int t -> int -> unit state = fun (Spin c) n ->
      return (c.value <- T.clamp n c.spin)

    let check : bool t -> bool -> unit state = fun (Check c) b ->
      return (c.value <- b)

    let combo : string t -> string -> unit state = fun t v ->
      match t with
      | String _ -> assert false
      | Combo c when not @@ T.is_var v c.combo -> return ()
      | Combo c -> return (c.value <- v)

    let string : string t -> string -> unit state = fun t v ->
      match t with
      | Combo _ -> assert false
      | String s -> return (s.value <- v)

    let button : unit state -> (unit t -> unit -> unit state) =
      fun x -> fun Button () -> x
  end

  let parse ~name ~value ~f = match value with
    | None -> failwithf "Expected value for option %s" name ()
    | Some value -> try f value with _ ->
      failwithf "Failed to parse value %s for option %s" value name ()

  let call :
    type a.
    a t ->
    a callback ->
    name:string ->
    value:string option ->
    unit state = fun t callback ~name ~value -> match t with
    | Spin _   -> callback t @@ parse ~name ~value ~f:Int.of_string
    | Check _  -> callback t @@ parse ~name ~value ~f:Bool.of_string
    | Combo _  -> callback t @@ parse ~name ~value ~f:Fn.id
    | String _ -> callback t @@ parse ~name ~value ~f:Fn.id
    | Button   -> callback t ()

  type entry = E : 'a t * 'a callback -> entry

  module Defaults = struct
    let ponder = false
    let multi_pv = T.{default = 1; min = 1; max = 1}
  end

  let tbl = Hashtbl.of_alist_exn (module String) [
      "MultiPV",    E (spin Defaults.multi_pv, Callbacks.spin);
      "Ponder",     E (check Defaults.ponder, Callbacks.check);
      "Clear Hash", E (button, Callbacks.button State.clear_tt);
    ]

  let spin_value name = match Hashtbl.find_exn tbl name with
    | E (Spin {value; _}, _) -> value
    | _ -> assert false

  let check_value name = match Hashtbl.find_exn tbl name with
    | E (Check {value; _}, _) -> value
    | _ -> assert false

  let combo_value name = match Hashtbl.find_exn tbl name with
    | E (Combo {value; _}, _) -> value
    | _ -> assert false

  let string_value name = match Hashtbl.find_exn tbl name with
    | E (String {value; _}, _) -> value
    | _ -> assert false
end

let uci =
  let open Uci.Send in
  let id = [
    Id (`name (sprintf "ocamlchess v%d.%d" Version.major Version.minor));
    Id (`author "Benjamin Mourad");
  ] in
  fun () ->
    List.iter id ~f:(fun cmd -> Format.printf "%s\n%!" @@ to_string cmd);
    Format.printf "\n%!";
    Hashtbl.iteri Options.tbl ~f:(fun ~key:name ~data:Options.(E (t, _)) ->
        let typ = Options.to_uci t in
        Format.printf "%s\n%!" @@ to_string (Option Option.{name; typ}));
    Format.printf "%s\n%!" @@ to_string Uciok

let isready () = Format.printf "%s\n%!" @@ Uci.Send.(to_string Readyok)

let setoption ({name; value} : Uci.Recv.Setoption.t) =
  let open Uci.Recv.Setoption in
  match Hashtbl.find Options.tbl name with
  | None -> cont @@ Format.printf "No such option: %s\n%!" name
  | Some Options.(E (t, callback)) ->
    Options.call t callback ~name ~value >>= cont

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
  let score = Search.Result.score result in
  let info = match Search.Result.pv result with
    | [] -> Uci.Send.Info.[Depth depth; Score score]
    | pv ->
      let seldepth = Search.Result.seldepth result in
      let nodes = Search.Result.nodes result in
      (* Avoid division by zero here (the search may terminate in under a
         millisecond). *)
      let time = max 1 @@ Search.Result.time result in
      let nps = (nodes * 1000) / time in
      Uci.Send.Info.[
        Depth depth;
        Seldepth seldepth;
        Score score;
        Nodes nodes;
        Nps nps;
        Time time;
        Pv (List.map pv ~f:Position.Legal.move);
      ] in
  Format.printf "%s\n%!" @@ Uci.Send.to_string @@ Info info

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
  let open Future in
  let cond = match ponder with
    | Some ponder -> fun () -> is_decided ponder || is_decided stop
    | None        -> fun () -> is_decided stop in
  Mutex.lock cvm;
  while not @@ cond () do Condition.wait cv cvm done;
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
  Format.printf "%s\n%!" @@ Uci.Send.(to_string @@ Bestmove bestmove)

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
  let moves = ref [] in
  let opt r v s = match !r with
    | Some _ -> failwithf "Error in go command: option '%s' already exists" s ()
    | None -> r := Some v in
  let lst l v s = match !l with
    | _ :: _ -> failwithf "Error in go command: option '%s' already exists" s ()
    | [] -> l := v in
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
      | Searchmoves l -> lst moves l "searchmoves");
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
      ~moves:!moves
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
    | None -> loop @@ Format.printf "what?\n%!"
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
    Format.eprintf "%s\n%!" msg;
    Err.exit () in
  (* Stop the search thread. *)
  Atomic.get search_thread |>
  Option.iter ~f:(fun t -> kill stop; Thread.join t);
