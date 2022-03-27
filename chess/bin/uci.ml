open Core_kernel
open Chess
open Monads.Std

module State = struct
  module T = struct
    type t = {
      position : Position.t;
      tt : Search.Tt.t;
    } [@@deriving fields]
  end

  include T
  include Monad.State.Make(T)(Monad.Ident)

  let set_position position = update @@ fun st -> {st with position}
end

open State.Syntax

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
  let response = [
    Id (`name "ocamlchess v0.1");
    Id (`author "Benjamin Mourad");
    Uciok;
  ] in
  fun () -> List.iter response ~f:(fun cmd ->
      printf "%s\n%!" @@ to_string cmd)

let isready () = printf "%s\n%!" @@ Uci.Send.(to_string Readyok)

let ucinewgame =
  State.update @@ fun st ->
  Search.Tt.clear st.tt;
  {st with position = Position.start}

let position pos moves =
  let rec apply = function
    | [] -> cont ()
    | m :: rest ->
      State.(gets position) >>= fun pos ->
      match Position.make_move pos m with
      | exception _ ->
        Debug.printf "Received illegal move %s for position %s\n%!"
          (Move.to_string m) (Position.Fen.to_string pos);
        finish ()
      | legal ->
        let pos = Position.Legal.new_position legal in
        State.set_position pos >>= fun () -> apply rest in
  match Position.Valid.check pos with
  | Ok () -> State.set_position pos >>= fun () -> apply moves
  | Error err ->
    Debug.printf "Received invalid position %s: %s\n%!"
      (Position.Fen.to_string pos) (Position.Valid.Error.to_string err);
    finish ()

(* Interprets a command. Returns true if the main UCI loop shall continue. *)
let recv cmd =
  let open Uci.Recv in
  match cmd with
  | Quit -> finish ()
  | Uci -> uci (); cont ()
  | Isready -> isready (); cont ()
  | Ucinewgame -> ucinewgame >>= cont
  | Position (`fen pos, moves) -> position pos moves
  | Position (`startpos, moves) -> position Position.start moves
  | cmd ->
    Debug.printf "Unhandled command: %s\n%!" @@ to_string cmd;
    cont ()

(* Main loop. *)
let rec loop () = match In_channel.(input_line stdin) with
  | None -> return ()
  | Some "" -> loop ()
  | Some line ->
    let open Uci.Recv in
    match of_string line with
    | None ->
      printf "Invalid command: %s\n%!" line;
      loop ()
    | Some cmd ->
      Debug.printf "Received command: %s\n%!" @@ to_string cmd;
      recv cmd >>= function
      | false -> return ()
      | true -> loop ()

(* Entry point. *)
let run ~debug =
  Debug.set debug;
  let position = Position.start in
  let tt = Search.Tt.create () in
  Monad.State.eval (loop ()) @@
  State.Fields.create ~position ~tt
