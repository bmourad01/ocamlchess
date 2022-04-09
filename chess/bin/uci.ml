open Core_kernel
open Chess
open Bap_future.Std
open Monads.Std

module State = struct
  module T = struct
    type t = {
      pos     : Position.t;
      history : int Int64.Map.t;
      tt      : Search.Tt.t;
      stop    : unit promise option;
    } [@@deriving fields]
  end

  include T
  include Monad.State.Make(T)(Monad.Ident)

  (* Update the position and history after a move has been made. *)
  let update_position pos = update @@ fun st ->
    let history =
      Position.hash pos |> Map.update st.history ~f:(function
          | None -> 1 | Some n -> n + 1) in
    {st with pos; history}

  (* Set the new starting position and history. *)
  let set_position pos = update @@ fun st ->
    let history = Int64.Map.singleton (Position.hash pos) 1 in
    {st with pos; history}

  let clear_tt = gets @@ fun {tt; _} -> Search.Tt.clear tt
  let set_stop stop = update @@ fun st -> {st with stop}
end

open State.Syntax

let options = Hashtbl.of_alist_exn (module String) Uci.Send.Option.Type.[
    "Clear Hash", Button;
  ]

(* Debug logging. *)
module Debug = struct
  let enabled = ref false
  let set b = enabled := b

  let printf fmt =
    if !enabled then Printf.eprintf fmt
    else Printf.(ifprintf stderr fmt)
end

let return = State.return
let cont () = return true
let finish () = return false

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

let position pos moves =
  let rec apply = function
    | [] -> cont ()
    | m :: rest ->
      State.(gets pos) >>= fun pos ->
      match Position.make_move pos m with
      | exception _ ->
        failwithf "Received illegal move %s for position %s\n%!"
          (Move.to_string m) (Position.Fen.to_string pos) ()
      | legal ->
        let pos = Position.Legal.new_position legal in
        State.update_position pos >>= fun () ->
        apply rest in
  match Position.Valid.check pos with
  | Ok () ->
    State.set_position pos >>= fun () ->
    apply moves
  | Error err ->
    failwithf "Received invalid position %s: %s\n%!"
      (Position.Fen.to_string pos) (Position.Valid.Error.to_string err) ()

let info_of_result root tt result =
  let depth = Search.Result.depth result in
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
      Nodes nodes;
      Score score;
      Nps nps;
      Time time;
      Pv pv;
    ])

(* Don't output the result of the search. *)
let cancel = Atomic.make false

(* Current search thread. *)
let search_thread = Atomic.make None

let kill stop =
  Option.iter stop ~f:(fun stop -> Promise.fulfill stop ());
  Atomic.set cancel true

(* The main search routine, should be run in a separate thread. *)
let search ~root ~limits ~history ~tt ~stop =
  let result =
    (* For each iteration, send a UCI `info` command about the search. *)
    let iter result = info_of_result root tt result in
    try Search.go () ~root ~limits ~history ~tt ~iter
    with exn ->
      (* Notify the user and abort. *)
      printf "Search encountered an exception: %s\n%!" @@ Exn.to_string exn;
      exit 1 in
  (* The UCI protocol says that `infinite` and `ponder` searches must wait for
     a corresponding `stop` or `ponderhit` command before sending `bestmove`.
     So, we will busy-wait in this thread until it happens. *)
  if Search.Limits.infinite limits then
    while not (Future.is_decided stop || Atomic.get cancel) do
      Thread.yield ()
    done;
  begin match Atomic.get cancel with
    | true ->
      (* We canceled the thread, so don't output the result. *)
      Atomic.set cancel false
    | false ->
      (* Send the bestmove. *)
      let ponder = match Search.Result.pv result with
        | _ :: ponder :: _ -> Some (Position.Legal.move ponder)
        | _ -> None in
      let move = Position.Legal.move @@ Search.Result.best result in
      printf "%s\n%!" @@
      Uci.Send.to_string @@
      Uci.Send.(Bestmove Bestmove.{move; ponder})
  end;
  (* Thread completed. *)
  Atomic.set search_thread None

(* Abort if there's already a thread running. *)
let check_thread =
  State.(gets stop) >>| fun stop ->
  Atomic.get search_thread |> Option.iter ~f:(fun t ->
      kill stop;
      Thread.join t;
      failwith "Error: tried to start a new search while the previous one is \
                still running")

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
  (* If no parameters were given, then assume an infinite search. This is how
     Stockfish behaves. To be fair, the UCI protocol is very underspecified
     and underdocumented. It begs the question as to why it's still so widely
     supported. *)
  if List.is_empty g then infinite := true
  else List.iter g ~f:(fun go ->
      let open Uci.Recv.Go in
      match go with
      | Infinite -> infinite := true
      | Nodes n -> nodes := Some n
      | Mate n -> mate := Some n
      | Depth n -> depth := Some n
      | Movetime t -> movetime := Some t
      | Wtime t -> wtime := Some t
      | Btime t -> btime := Some t
      | Winc n -> winc := Some n
      | Binc n -> binc := Some n
      | Movestogo n -> movestogo := Some n
      | Ponder -> failwith "Unsupported command: ponder"
      | Searchmoves _ -> failwith "Unsupported command: searchmoves");
  (* Construct the search limits. *)
  State.(gets pos) >>= fun root ->
  let active = Position.active root in
  let stop, promise = Future.create () in
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
  State.set_stop (Some promise) >>= fun () ->
  Atomic.set search_thread @@
  Option.return @@
  Thread.create (fun () -> search ~root ~limits ~history ~tt ~stop) ();
  cont ()

let stop =
  (* Fulfill the promise if it exists. *)
  State.(gets stop) >>= begin function
    | Some stop -> return @@ Promise.fulfill stop ()
    | None -> return ()
  end >>= fun () ->
  (* Reset. *)
  State.set_stop None

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
  | Ponderhit -> failwith "Unsupported command: ponderhit"
  | Debug _ -> failwith "Unsupported command: debug"
  | Register _ -> failwith "Unsupported command: register"

(* Main loop. *)
let rec loop () = match In_channel.(input_line stdin) with
  | None -> return ()
  | Some "" -> loop ()
  | Some line ->
    let open Uci.Recv in
    match of_string line with
    | None ->
      Debug.printf "Invalid command: %s\n%!" line;
      printf "what?\n%!";
      loop ()
    | Some cmd ->
      Debug.printf "Received command: %s\n%!" @@ to_string cmd;
      recv cmd >>= function
      | false -> return ()
      | true -> loop ()

(* Entry point. *)
let run ~debug =
  Debug.set debug;
  (* Run the main interpreter loop. *)
  let State.{stop; _} =
    Monad.State.exec (loop ()) @@
    State.Fields.create
      ~pos:Position.start
      ~history:(Int64.Map.singleton Position.(hash start) 1)
      ~tt:(Search.Tt.create ())
      ~stop:None in
  (* Stop the search thread. *)
  Atomic.get search_thread |>
  Option.iter ~f:(fun t -> kill stop; Thread.join t);
