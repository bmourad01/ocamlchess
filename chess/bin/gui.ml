open Core_kernel
open Chess
open Monads.Std

module Bb = Bitboard
module Child = Position.Child

module Window = struct
  type t

  type event = [
    | `Mouse_button_pressed of int * int
  ]

  external create : int -> int -> string -> t = "ml_window_create"
  external size : t -> (int * int) = "ml_window_size"
  external is_open : t -> bool = "ml_window_is_open"
  external poll_event : t -> event option = "ml_window_poll_event"
  external close : t -> unit = "ml_window_close"
  external clear : t -> unit = "ml_window_clear"

  external promote :
    int -> int -> string -> Piece.color -> Move.promote = "ml_promote"

  external paint_board :
    t -> Position.t -> int64 -> Square.t option -> Move.t option -> unit =
    "ml_window_paint_board"
end

module State = struct
  module T = struct
    type t = {
      window : Window.t;
      game   : Game.t;
      moves  : Position.child list;
      sel    : (Square.t * Position.child list) option;
      prev   : Position.child option;
      white  : Player.e option;
      black  : Player.e option;
    } [@@deriving fields]
  end

  include T
  include Monad.State.Make(T)(Monad.Ident)
end

open State.Syntax

let screen_to_sq window mx my =
  let sx, sy = Window.size window in
  let tw, th = sx / Square.File.count, sy / Square.Rank.count in
  let rec loop_y y =
    if y >= Square.Rank.count then None
    else
      let rec loop_x x =
        if x >= Square.File.count then None
        else
          let px = x * tw in
          let py = y * th in
          if mx >= px && mx < (px + tw) && my >= py && my < (py + th)
          then Square.create ~rank:(Square.Rank.count - 1 - y) ~file:x
          else succ x |> loop_x in
      match loop_x 0 with
      | None -> succ y |> loop_y
      | Some _ as sq -> sq in
  loop_y 0

let find_move sq mv =
  let m = Child.move mv in
  Square.(sq = Move.dst m)

let find_promote sq k mv =
  let m = Child.move mv in
  Square.(sq = Move.dst m) &&
  Option.exists (Move.promote m) ~f:(Move.Promote.equal k)

let promote_w = 320
let promote_h = 80
let promote_name = "Pick a promotion piece"

let click mx my = State.update @@ fun ({window; game; moves; sel; _} as st) ->
  match screen_to_sq window mx my with
  | None -> st
  | Some sq -> match sel with
    | Some (sq', _) when Square.(sq = sq') -> {st with sel = None}
    | None ->
      let moves = List.filter moves ~f:(fun m ->
          Square.(sq = Move.src (Child.move m))) in
      let sel = if List.is_empty moves then None else Some (sq, moves) in
      {st with sel}
    | Some (_, moves) -> match List.find moves ~f:(find_move sq) with
      | None -> st
      | Some m ->
        let m = match Move.promote @@ Child.move m with
          | None -> m
          | Some _ ->
            let c = Position.active @@ Child.parent m in
            let k = Window.promote promote_w promote_h promote_name c in
            List.find_exn moves ~f:(find_promote sq k) in
        let game = Game.add_move game m in
        let moves = Position.children @@ Child.self m in
        {st with game; moves; sel = None; prev = Some m}

let poll = State.(gets window) >>= fun window ->
  match Window.poll_event window with
  | Some (`Mouse_button_pressed (mx, my)) -> click mx my
  | None -> State.return ()

let is_fifty_move pos = Position.halfmove pos >= 100

let print_result : Game.result -> unit = function
  | Checkmate c ->
    let c = Piece.Color.opposite c in
    Format.printf "Checkmate, %a wins\n%!" Piece.Color.pp_hum c
  | Draw `Stalemate -> Format.printf "Draw by stalemate.\n%!"
  | Draw `Insufficient_material ->
    Format.printf "Draw by insufficient material\n%!"
  | Draw `Seventy_five_move_rule ->
    Format.printf "Draw by seventy-five move rule\n%!"
  | Draw `Fivefold_repetition ->
    Format.printf "Draw by fivefold repetition\n%!"
  | _ -> ()

let human_move = State.(gets game) >>= fun game ->
  if Game.is_over game then State.return ()
  else poll >>= fun () ->
    State.(gets game) >>| fun game ->
    print_result @@ Game.result game

let update_player_state c player pst =
  let player = Player.(T (update player pst)) in
  State.update @@ fun st -> match c with
  | Piece.White -> {st with white = Some player}
  | Piece.Black -> {st with black = Some player}

let ai_move c player = State.(gets game) >>= fun game ->
  let pos = Game.position game in
  let Player.(T player) = player in
  let m, st = try Player.choose player pos with
    | Player.No_moves ->
      failwith "Tried to play AI move with no legal moves."
    | Player.Invalid_move (_, m) ->
      failwithf "Tried to play invalid move %s."
        (Move.to_string @@ Child.move m) () in
  update_player_state c player st >>= fun () ->
  let game = Game.add_move game m in
  let moves = Position.children @@ Child.self m in
  begin State.update @@ fun st ->
    {st with game; moves; sel = None; prev = Some m}
  end >>| fun () -> print_result @@ Game.result game

let human_or_ai_move c = function
  | None -> human_move
  | Some player -> State.(gets game) >>= fun game ->
    if Game.is_over game then State.return ()
    else ai_move c player

let display_board ?(bb = 0L) ?(sq = None) ?(prev = None) pos window =
  Window.clear window;
  Window.paint_board window pos bb sq prev

(* Prompt the user to quit when ready. *)
let prompt_end window =
  Format.printf "Enter any key to quit: %!";
  ignore @@ In_channel.(input_line stdin);
  Window.close window

(* For debugging, make sure that Zobrist hashing works. *)
let assert_hash new_pos =
  let h =
    Position.Fen.to_string new_pos |>
    Position.Fen.of_string_exn |>
    Position.hash in
  let h' = Position.hash new_pos in
  if Int64.(h = h') then h' else failwithf
      "New position has hash %016LX, but %016LX was expected. \
       Position: %s" h' h (Position.Fen.to_string new_pos) ()

let assert_pawn_hash new_pos =
  let h =
    Position.Fen.to_string new_pos |>
    Position.Fen.of_string_exn |>
    Position.pawn_hash in
  let h' = Position.pawn_hash new_pos in
  if Int64.(h = h') then h' else failwithf
      "New position has pawn hash %016LX, but %016LX was expected. \
       Position: %s" h' h (Position.Fen.to_string new_pos) ()

let rec main_loop ~delay () = State.(gets window) >>= fun window ->
  if Window.is_open window then
    (* Process input if the game is still playable. *)
    State.(gets game) >>= fun game ->
    let pos = Game.position game in
    let active = Position.active pos in
    begin match active with
      | White -> State.(gets white)
      | Black -> State.(gets black)
    end >>= human_or_ai_move active >>= fun () ->
    (* New position? *)
    State.(gets game) >>= fun game ->
    let new_pos = Game.position game in
    State.(gets moves) >>= fun moves ->
    State.(gets prev) >>= fun prev ->
    (* Print information about position change. *)
    if not @@ Position.same_hash pos new_pos then begin
      let mv = Option.value_exn prev in
      let m = Child.move mv in
      Format.printf "%a (%a): %a\n%!"
        Move.pp m Position.San.pp mv Position.pp new_pos;
      Format.printf "Hash: %016LX\n%!" @@ assert_hash new_pos;
      Format.printf "Pawn hash: %016LX\n%!" @@ assert_pawn_hash new_pos;
      See.go mv |> Option.iter ~f:(fun see ->
          Format.printf "Static Exchange Evaluation: %d\n%!" see);
      Format.printf "%d legal moves\n%!" @@ List.length moves;
      Format.printf "\n%!";
    end;
    let prev = Option.map prev ~f:Child.move in
    (* Get the valid squares for our selected piece to move to. *)
    State.(gets sel) >>= begin function
      | None -> State.return (Bb.(to_int64 empty), None)
      | Some (sq, moves) ->
        let bb =
          List.fold moves ~init:Bb.empty ~f:(fun acc mv ->
              let m = Child.move mv in
              Bb.(acc ++ Move.dst m)) in
        State.return (Bb.to_int64 bb, Some sq)
    end >>= fun (bb, sq) ->
    display_board new_pos window ~bb ~sq ~prev;
    (* Nothing more to do if the game is over. *)
    if Game.is_over game then State.return ()
    else (delay (); main_loop ~delay ())
  else State.return ()

let start_with_game_over_check delay =
  try
    State.(gets game) >>= fun game ->
    State.(gets window) >>= fun window ->
    display_board (Game.position game) window;
    if not @@ Game.is_over game then main_loop ~delay ()
    else begin
      print_result @@ Game.result game;
      Format.printf "\n%!";
      State.return () 
    end
  with exn ->
    Format.printf "GUI encountered an exception: %a\n%!" Exn.pp exn;
    Err.exit ()

let () = Callback.register "piece_at_square" Position.piece_at_square
let () = Callback.register "string_of_square" Square.to_string

let window_size = 640

external init_fonts : string -> string -> unit = "ml_init_fonts"
external init_named_values : unit -> unit = "ml_init_named_values"

let assets = Sys.getenv "HOME" ^ "/.local/share/ocamlchess/assets/"
let piece_font = assets ^ "FreeSerif.ttf"
let text_font = assets ^ "FreeSans.ttf"

let run pos ~white ~black ~delay =
  init_fonts piece_font text_font;
  init_named_values ();
  let window = Window.create window_size window_size "chess" in
  let moves = Position.children pos in
  let white_name = match white with
    | None -> Format.printf "White is human\n%!"; "human"
    | Some Player.(T player) ->
      Format.printf "White is AI: %s\n%!" @@ Player.name player;
      Player.name player in
  let black_name = match black with
    | None -> Format.printf "Black is human\n%!"; "human"
    | Some Player.(T player) ->
      printf "Black is AI: %s\n%!" @@ Player.name player;
      Player.name player in
  let game = Game.create ()
      ~event:(Some "ocamlchess")
      ~site:(Some "gui")
      ~date:(Some (Date.today ~zone:Time.Zone.utc))
      ~round:(Some 1)
      ~white:(Some white_name)
      ~black:(Some black_name)
      ~start:pos in
  Format.printf "\n%!";
  Format.printf "Initial position: %a\n%!" Position.pp pos;
  Format.printf "Hash: %016LX\n%!" @@ Position.hash pos;
  Format.printf "Pawn hash: %016LX\n%!" @@ Position.pawn_hash pos;
  Format.printf "%d legal moves\n%!" @@ List.length moves;
  Format.printf "\n%!";
  let State.T.{game; _} =
    let sel = None in
    let prev = None in
    Monad.State.exec (start_with_game_over_check delay) @@
    State.Fields.create ~window ~game ~moves ~sel ~prev ~white ~black in
  Format.printf "PGN of game:\n\n%a\n%!" Game.pp game;
  prompt_end window
