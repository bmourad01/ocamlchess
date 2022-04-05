open Core_kernel
open Chess
open Monads.Std

module State = struct
  module T = struct
    type t = {
      pos     : Position.t;
      history : int Int64.Map.t;
      tt      : Search.Tt.t;
      stop    : bool ref;
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
  let stop' {stop; _} = stop := true
end

let threads = ref []

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
        Debug.printf "Received illegal move %s for position %s\n%!"
          (Move.to_string m) (Position.Fen.to_string pos);
        finish ()
      | legal ->
        let pos = Position.Legal.new_position legal in
        State.update_position pos >>= fun () ->
        apply rest in
  match Position.Valid.check pos with
  | Ok () ->
    State.set_position pos >>= fun () ->
    apply moves
  | Error err ->
    Debug.printf "Received invalid position %s: %s\n%!"
      (Position.Fen.to_string pos) (Position.Valid.Error.to_string err);
    finish ()

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

let search ~root ~limits ~history ~tt ~stop =
  (* Make sure that we don't start a search that will stop immediately. *)
  stop := false;
  let result =
    Search.go () ~root ~limits ~history ~tt ~stop ~iter:(fun result ->
        (* For each iteration, send a UCI `info` command about the search. *)
        info_of_result root tt result) in
  stop := false;
  (* Send the bestmove. *)
  let ponder = match Search.Result.pv result with
    | _ :: ponder :: _ -> Some (Position.Legal.move ponder)
    | _ -> None in
  let move = Position.Legal.move @@ Search.Result.best result in
  printf "%s\n%!" @@
  Uci.Send.to_string @@
  Uci.Send.(Bestmove Bestmove.{move; ponder})

let go g =
  (* Parse the search limits. *)
  let infinite = ref false in
  let nodes = ref None in
  let depth = ref None in
  let movetime = ref None in
  let wtime = ref None in
  let btime = ref None in
  let winc = ref None in
  let binc = ref None in
  let moves_to_go = ref None in
  List.iter g ~f:(fun go ->
      let open Uci.Recv.Go in
      match go with
      | Infinite -> infinite := true
      | Nodes n -> nodes := Some n
      | Depth n -> depth := Some n
      | Movetime t -> movetime := Some t
      | Wtime t -> wtime := Some t
      | Btime t -> btime := Some t
      | Winc n -> winc := Some n
      | Binc n -> binc := Some n
      | Movestogo n -> moves_to_go := Some n
      | _ -> ());
  (* Construct the search limits. *)
  let nodes = !nodes in
  let moves_to_go = !moves_to_go in
  let winc = Option.value ~default:0 !winc in
  let binc = Option.value ~default:0 !binc in
  State.(gets pos) >>= fun root ->
  let active = Position.active root in
  let limits = Option.try_with @@ fun () ->
    if !infinite then Search.Limits.of_infinite ~nodes ()
    else match !depth with
      | Some n -> Search.Limits.of_depth n ~nodes
      | None -> match !movetime with
        | Some n -> Search.Limits.of_search_time n ~nodes
        | None -> match !wtime, !btime with
          | Some wtime, Some btime ->
            Search.Limits.of_game_time
              ~wtime ~winc ~btime ~binc ~active ~nodes ~moves_to_go ()
          | _ -> assert false in
  match limits with
  | None ->
    Debug.printf "Ill-formed command: %s\n%!" @@ Uci.Recv.to_string (Go g);
    finish ()
  | Some limits ->
    (* Start the search. *)
    State.(gets history) >>= fun history ->
    State.(gets tt) >>= fun tt ->
    State.(gets stop) >>= fun stop ->
    let t = Thread.create (fun () ->
        search ~root ~limits ~history ~tt ~stop) () in
    threads := t :: !threads;
    cont ()

(* Interprets a command. Returns true if the main UCI loop shall continue. *)
let recv cmd =
  let open Uci.Recv in
  match cmd with
  | Uci -> uci (); cont ()
  | Isready -> isready (); cont ()
  | Setoption opt -> setoption opt
  | Ucinewgame -> ucinewgame >>= cont
  | Position (`fen pos, moves) -> position pos moves
  | Position (`startpos, moves) -> position Position.start moves
  | Go g -> go g
  | Stop -> State.(gets stop') >>= cont
  | Quit -> State.(gets stop') >>= finish
  | cmd ->
    Debug.printf "Unhandled command: %s\n%!" @@ to_string cmd;
    printf "what?\n%!";
    cont ()

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

let rec wait_for_threads () = match !threads with
  | [] -> ()
  | t :: rest ->
    threads := rest;
    Thread.join t;
    wait_for_threads ()

(* Entry point. *)
let run ~debug =
  Debug.set debug;
  Monad.State.eval (loop ()) @@
  State.Fields.create
    ~pos:Position.start
    ~history:(Int64.Map.singleton Position.(hash start) 1)
    ~tt:(Search.Tt.create ())
    ~stop:(ref false);
  wait_for_threads ()
