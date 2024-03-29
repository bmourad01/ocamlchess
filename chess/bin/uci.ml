module M = Mutex
module T = Thread

open Core_kernel [@@warning "-D"]
open Chess
open Bap_future.Std
open Monads.Std

module Child = Position.Child
module Histogram = Position.Histogram
module Line = Search.Result.Line

module State = struct
  module T = struct
    type t = {
      pos       : Position.t;
      histogram : Position.histogram;
      tt        : Search.tt;
      stop      : unit promise option;
      ponder    : unit promise option;
      debug     : bool;
      book      : Book.t option;
    } [@@deriving fields]
  end

  include T
  include Monad.State.Make(T)(Monad.Ident)
  include Monad.State.T1(T)(Monad.Ident)

  (* Update the position and histogram. If this is a new game, then
     clear the histogram and TT. *)
  let set_position ?(new_game = false) pos = update @@ fun st ->
    let histogram =
      let h = if new_game then Histogram.empty else st.histogram in
      Histogram.incr h pos in
    if new_game then Search.Tt.clear st.tt;
    {st with pos; histogram}

  let play_move m = gets pos >>= fun pos ->
    match Position.make_move pos m with
    | Some m -> set_position @@ Child.self m
    | None ->
      failwithf "Received illegal move %s for position %s\n%!"
        (Move.to_string m) (Position.Fen.to_string pos) ()

  let clear_tt = gets @@ fun {tt; _} -> Search.Tt.clear tt
  let set_debug debug = update @@ fun st -> {st with debug}

  let new_stop () =
    let f, p = Future.create () in
    begin update @@ fun st ->
      {st with stop = Some p}
    end >>| fun () -> f

  let new_ponder_when = function
    | false -> return None
    | true ->
      let f, p = Future.create () in
      begin update @@ fun st ->
        {st with ponder = Some p}
      end >>| fun () -> Some f
end

open State.Syntax

type 'a state = 'a State.t

let return = State.return
let cont () = return true
let finish () = return false

module Options = struct
  module T = Uci.Send.Option.Type

  (* Artificial type to resolve ambiguity between the Combo and String
     constructors. *)
  type combo = {v : string} [@@unboxed]

  type _ t =
    | Spin : {spin : T.spin; mutable value : int} -> int t
    | Check : {default : bool; mutable value : bool} -> bool t
    | Combo : {combo : T.combo; mutable value : string} -> combo t
    | String : {default : string; mutable value : string} -> string t
    | Button : unit t

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

    let combo : combo t -> combo -> unit state = fun (Combo c) {v} ->
      return @@ if T.is_var v c.combo then c.value <- v

    let string : string t -> string -> unit state = fun (String s) v ->
      return (s.value <- v)

    let button : unit state -> (unit t -> unit -> unit state) = fun x ->
      fun Button () -> x
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
    | Combo _  -> callback t @@ parse ~name ~value ~f:(fun v -> {v})
    | String _ -> callback t @@ parse ~name ~value ~f:Fn.id
    | Button   -> callback t ()

  type entry = E : 'a t * 'a callback -> entry

  let spin s = E (Spin {spin = s; value = s.default}, Callbacks.spin)
  let check c = E (Check {default = c; value = c}, Callbacks.check)
  let combo c = E (Combo {combo = c; value = c.default}, Callbacks.combo)
  let string s = E (String {default = s; value = s}, Callbacks.string)
  let button c = E (Button, Callbacks.button c)

  module Defaults = struct
    let ponder = false
    let own_book = false
    let book_random = false
    let book_path = "book.bin"
    let multi_pv = T.{default = 1; min = 1; max = 500}
  end

  let tbl = Hashtbl.of_alist_exn (module String) [
      "MultiPV",    spin Defaults.multi_pv;
      "Ponder",     check Defaults.ponder;
      "OwnBook",    check Defaults.own_book;
      "BookRandom", check Defaults.book_random;
      "BookPath",   string Defaults.book_path;
      "Clear Hash", button State.clear_tt;
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

let info_str s = Format.printf "%a\n%!" Uci.Send.pp @@ Info [String s]

module Book = struct
  (* Just compare the file paths. We could have a stronger notion of equivalence
     such as the md5sums of either file. *)
  let same_book b path = String.(path = Book.filename b)

  let load_book () =
    if Options.check_value "OwnBook" then
      let path = Options.string_value "BookPath" in
      State.(gets book) >>= function
      | Some b when same_book b path -> return @@ Some b
      | Some _ | None ->
        info_str "Loading Book";
        match Book.create path with
        | exception exn ->
          failwithf "Error loading book: %s" (Exn.to_string exn) ()
        | b -> State.(update @@ fun st -> {
            st with book = Some b
          }) >>| fun () ->
          info_str "Book Loaded";
          Some b
    else return None

  let book_move m =
    let open Uci.Send in
    info_str "Book Move";
    Format.printf "%a\n%!" pp @@ Bestmove (Some Bestmove.{
        move = Child.move m;
        ponder = None
      })

  let run () =
    load_book () >>= function
    | None -> return false
    | Some book ->
      State.(gets pos) >>| fun pos ->
      let random = Options.check_value "BookRandom" in
      match Book.lookup book pos ~random with
      | Ok m -> book_move m; true
      | Error _ -> false
end

let uci =
  let open Uci.Send in
  let id = [
    Id (`name (sprintf "ocamlchess v%d.%d" Version.major Version.minor));
    Id (`author "Benjamin Mourad");
  ] in
  let opt name t = Option.{name; typ = Options.to_uci t} in
  fun () ->
    List.iter id ~f:(fun cmd -> Format.printf "%a\n%!" pp cmd);
    Hashtbl.iteri Options.tbl ~f:(fun ~key:name ~data:Options.(E (t, _)) ->
        Format.printf "%a\n%!" Option.pp @@ opt name t);
    Format.printf "%a\n%!" pp Uciok

let isready () =
  Book.load_book () >>| fun _ ->
  Format.printf "%a\n%!" Uci.Send.pp Readyok

let setoption ({name; value} : Uci.Recv.Setoption.t) =
  let open Uci.Recv.Setoption in
  match Hashtbl.find Options.tbl name with
  | None -> return @@ Format.printf "No such option: %s\n%!" name
  | Some Options.(E (t, callback)) -> Options.call t callback ~name ~value

let ucinewgame = State.set_position Position.start ~new_game:true

let position pos moves = match Position.Valid.check pos with
  | Ok () ->
    State.set_position pos >>= fun () ->
    State.(List.iter moves ~f:play_move)
  | Error err ->
    failwithf "Received invalid position %s: %s\n%!"
      (Position.Fen.to_string pos) (Position.Valid.Error.to_string err) ()

module Search_thread = struct
  let t = Atomic.make None
  let c = Condition.create ()
  let m = M.create ()

  let signal ps =
    M.lock m;
    List.iter ps ~f:(Option.iter ~f:(fun p -> Promise.fulfill p ()));
    Condition.signal c;
    M.unlock m

  let wait stop ponder =
    let open Future in
    let cond = match ponder with
      | Some ponder -> fun () -> is_decided stop || is_decided ponder
      | None        -> fun () -> is_decided stop in
    M.lock m;
    while not @@ cond () do Condition.wait c m done;
    M.unlock m

  (* For each iteration in the search, send a UCI `info` command about the
     search. *)
  let info_of_result root tt result =
    let depth = Search.Result.depth result in
    let time = max 1 @@ Search.Result.time result in
    let nodes = Search.Result.nodes result in
    let nps = (nodes * 1000) / time in
    Search.Result.lines result |> List.iteri ~f:(fun i line ->
        let pv = Line.pv line |> List.map ~f:Child.move in
        let score = Line.score line in
        let seldepth = Line.seldepth line in
        Format.printf "%a\n%!" Uci.Send.pp @@ Info Uci.Send.Info.[
            Uci.Send.Info.Depth depth;
            Seldepth seldepth;
            Multipv (i + 1);
            Score score;
            Nodes nodes;
            Nps nps;
            Time time;
            Pv pv;
          ])

  let bestmove result =
    let make ?p m =
      let move = Child.move m in
      let ponder = Option.map p ~f:Child.move in
      Uci.Send.Bestmove.{move; ponder} in
    let bestmove =
      Search.Result.pv result |>
      Option.bind ~f:(fun line -> match Line.pv line with
          | m :: p :: _ -> Some (make m ~p)
          | [m] -> Some (make m)
          | [] -> None) in
    Format.printf "%a\n%!" Uci.Send.pp @@ Bestmove bestmove

  let currmove child ~n ~depth =
    let m = Child.move child in
    Format.printf "%a\n%!" Uci.Send.pp @@ Info [
      Depth depth;
      Currmove m;
      Currmovenumber n;
    ]

  (* The main search routine, should be run in a separate thread. *)
  let search ~root ~limits ~histogram ~tt ~stop ~ponder =
    let result = try
        let iter = info_of_result root tt in
        Search.go () ~root ~limits ~histogram ~tt ~ponder ~iter ~currmove
      with exn ->
        Format.eprintf "Search encountered an exception: %a\n%!" Exn.pp exn;
        Err.exit () in
    (* The UCI protocol says that `infinite` and `ponder` searches must
       wait for a corresponding `stop` or `ponderhit` command before
       sending `bestmove`. *)
    if Search.Limits.infinite limits then wait stop ponder;
    (* Output the result. *)
    bestmove result;
    (* Thread completed. *)
    Atomic.set t None

  (* Abort if there's already a thread running. *)
  let check =
    State.(gets stop) >>= fun stop ->
    State.(gets ponder) >>| fun ponder ->
    Atomic.get t |> Option.iter ~f:(fun t ->
        signal [stop; ponder];
        T.join t;
        failwith
          "Error: tried to start a new search while the previous one is \
           still running")

  let start ~root ~limits ~histogram ~tt ~stop ~ponder =
    Atomic.set t @@ Option.return @@
    T.create (fun () -> search ~root ~limits ~histogram ~tt ~stop ~ponder) ()
end

module Go = struct
  type t = {
    mutable infinite  : bool;
    mutable nodes     : int option;
    mutable mate      : int option;
    mutable depth     : int option;
    mutable movetime  : int option;
    mutable wtime     : int option;
    mutable btime     : int option;
    mutable winc      : int option;
    mutable binc      : int option;
    mutable movestogo : int option;
    mutable ponder    : bool;
    mutable moves     : Move.t list;
  } [@@deriving fields]

  let create () =
    Fields.create
      ~infinite:false
      ~nodes:None
      ~mate:None
      ~depth:None
      ~movetime:None
      ~wtime:None
      ~btime:None
      ~winc:None
      ~binc:None
      ~movestogo:None
      ~ponder:false
      ~moves:[]

  let opt t v name ~f ~g = match g t with
    | Some _ -> failwithf "Error in go command: duplicate option '%s'" name ()
    | None -> f t @@ Some v

  let lst t v name ~f ~g = match g t with
    | _ :: _ -> failwithf "Error in go command: duplicate option '%s'" name ()
    | [] -> f t v

  let new_limits t active stop =
    Search.Limits.create () ~active ~stop
      ~nodes:t.nodes
      ~mate:t.mate
      ~depth:t.depth
      ~movetime:t.movetime
      ~movestogo:t.movestogo
      ~wtime:t.wtime
      ~btime:t.btime
      ~binc:t.binc
      ~infinite:t.infinite
      ~moves:t.moves
      ~multipv:(Options.spin_value "MultiPV")

  let parse (g : Uci.Recv.Go.t list) =
    let t = create () in
    (* As a hack, ponder mode will initially be set up as an infinite search.
       Then, when the ponderhit command is sent, the search can continue with
       the normal limits. *)
    let pondering () = t.ponder <- true; t.infinite <- true in
    (* If no parameters were given, then assume an infinite search. This is how
       Stockfish behaves. To be fair, the UCI protocol is very underspecified
       and underdocumented. It begs the question as to why it's still so widely
       supported. *)
    if not @@ List.is_empty g then List.iter g ~f:(function
        | Nodes n       -> opt t n "nodes"       ~f:set_nodes     ~g:nodes
        | Mate n        -> opt t n "mate"        ~f:set_mate      ~g:mate
        | Depth n       -> opt t n "depth"       ~f:set_depth     ~g:depth
        | Movetime n    -> opt t n "movetime"    ~f:set_movetime  ~g:movetime
        | Wtime n       -> opt t n "wtime"       ~f:set_wtime     ~g:wtime
        | Btime n       -> opt t n "btime"       ~f:set_btime     ~g:btime
        | Winc n        -> opt t n "winc"        ~f:set_winc      ~g:winc
        | Binc n        -> opt t n "binc"        ~f:set_binc      ~g:binc
        | Movestogo n   -> opt t n "movestogo"   ~f:set_movestogo ~g:movestogo
        | Searchmoves l -> lst t l "searchmoves" ~f:set_moves     ~g:moves
        | Infinite      -> t.infinite <- true
        | Ponder        -> pondering ())
    else t.infinite <- true; t

  let run g = Search_thread.check >>= Book.run >>= function
    | true -> return ()
    | false ->
      (* Parse the arguments to the command *)
      let t = parse g in
      State.(gets pos) >>= fun root ->
      State.new_stop () >>= fun stop ->
      let limits = new_limits t (Position.active root) stop in
      (* Start the search. *)
      State.new_ponder_when t.ponder >>= fun ponder ->
      State.(gets histogram) >>= fun histogram ->
      State.(gets tt) >>| fun tt ->
      Search_thread.start ~root ~limits ~histogram ~tt ~stop ~ponder
end

let stop = State.update @@ function
  | {stop = (Some _ as p); _} as st ->
    Search_thread.signal [p]; {st with stop = None}
  | st -> st

let ponderhit = State.update @@ function
  | {ponder = (Some _ as p); _} as st ->
    Search_thread.signal [p]; {st with ponder = None}
  | st -> st

(* This is free software, so no need to register! *)
let register _ = return ()

(* Interprets a command. Returns true if the main UCI loop shall continue. *)
let recv cmd = match (cmd : Uci.Recv.t) with
  | Uci -> cont @@ uci ()
  | Isready -> isready () >>= cont
  | Setoption opt -> setoption opt >>= cont
  | Ucinewgame -> ucinewgame >>= cont
  | Position (`fen pos, moves) -> position pos moves >>= cont
  | Position (`startpos, moves) -> position Position.start moves >>= cont
  | Go g -> Go.run g >>= cont
  | Stop -> stop >>= cont
  | Quit -> finish ()
  | Ponderhit -> ponderhit >>= cont
  | Debug `off -> State.set_debug false >>= cont
  | Debug `on -> State.set_debug true >>= cont
  | Register r -> register r >>= cont

(* Main loop. *)
let rec loop () = match In_channel.(input_line stdin) with
  | None -> return ()
  | Some "" -> loop ()
  | Some line -> match Uci.Recv.of_string line with
    | None -> loop @@ Format.printf "Invalid command: %s\n%!" line
    | Some cmd -> recv cmd >>= function
      | false -> return ()
      | true -> loop ()

(* Default histogram has the starting position. *)
let histogram = Histogram.singleton Position.start

let exec () =
  let st =
    Monad.State.exec (loop ()) @@
    State.Fields.create ~histogram
      ~pos:Position.start
      ~tt:(Search.Tt.create ())
      ~stop:None
      ~ponder:None
      ~debug:false
      ~book:None in
  State.[stop st; ponder st]

(* Entry point. *)
let run () =
  (* Run the main interpreter loop. *)
  let ps = try exec () with Failure msg ->
    Format.eprintf "%s\n%!" msg;
    Err.exit () in
  (* Stop the search thread. *)
  Atomic.get Search_thread.t |>
  Option.iter ~f:(fun t ->
      Search_thread.signal ps;
      T.join t);
