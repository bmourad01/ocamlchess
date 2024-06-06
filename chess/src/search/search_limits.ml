open Core_kernel [@@warning "-D"]
open Bap_future.Std
open Search_common

type t = {
  infinite : bool;
  nodes    : int option;
  mate     : int option;
  depth    : int option;
  movetime : int option;
  max_time : int option;
  stop     : unit future;
  moves    : Move.t list;
  multipv  : int;
} [@@deriving fields]

let stopped limits = Future.is_decided @@ stop limits

let check_nodes = function
  | None -> ()
  | Some n when n >= 1 -> ()
  | Some n ->
    invalid_argf "Invalid node limit %d, must be greater than 0" n ()

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

let check_multipv n =
  if n < 1 then
    invalid_argf "Invalid multipv %d, must be greater than 0" n ()

let manage_time
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

let default_depth = 9

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
    ?(moves = [])
    ?(multipv = 1)
    ~active
    ~stop
    () =
  check_nodes nodes;
  check_depth depth;
  check_multipv multipv;
  let movetime = check_movetime movetime in
  let max_time = match wtime, btime with
    | None, None -> None
    | Some _, None -> invalid_arg "Missinc btime"
    | None, Some _ -> invalid_arg "Missing wtime"
    | Some wtime, Some btime ->
      let winc, binc = match winc, binc with
        | None, None -> 0, 0
        | Some winc, Some binc -> winc, binc
        | Some _, None -> invalid_arg "Missing binc"
        | None, Some _ -> invalid_arg "Missing winc" in
      Some (manage_time ~wtime ~winc ~btime ~binc ~movestogo ~active ()) in
  if not infinite
  && Option.is_none nodes
  && Option.is_none mate
  && Option.is_none depth
  && Option.is_none movetime
  && Option.is_none max_time
  && List.is_empty moves
  then invalid_arg "Limits were explicitly unspecified"
  else {
    infinite;
    nodes;
    mate;
    depth;
    movetime;
    max_time;
    stop;
    moves;
    multipv
  }
