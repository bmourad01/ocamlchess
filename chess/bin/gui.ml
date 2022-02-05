open Core_kernel
open Chess
open Monads.Std

module Bb = Bitboard
module Legal = Position.Legal

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
  external display : t -> unit = "ml_window_display"

  external paint_board :
    t -> Position.t -> int64 -> Square.t option -> Move.t option -> unit =
    "ml_window_paint_board"
end

type endgame =
  | Checkmate of Piece.color
  | Insufficient_material
  | Fifty_move
  | Stalemate

module State = struct
  module T = struct
    type t = {
      window : Window.t;
      pos : Position.t;
      legal : Position.legal list;
      sel : (Square.t * Position.legal list) option;
      prev : Position.legal option;
      endgame : endgame option;
      white : Player.e option;
      black : Player.e option;
    } [@@deriving fields]
  end

  include T
  include Monad.State.T1(T)(Monad.Ident)
  include Monad.State.Make(T)(Monad.Ident)
end

open State.Syntax

type 'a state = 'a State.t

let (>>) m n = m >>= fun _ -> n

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

(* FIXME: maybe do this part in the GUI? *)

let promote_prompt () =
  eprintf "Choose promotion piece (n|b|r|q): %!"

let rec promote () = match In_channel.(input_line stdin) with
  | Some "n" -> Piece.Knight
  | Some "b" -> Piece.Bishop
  | Some "r" -> Piece.Rook
  | Some "q" -> Piece.Queen
  | _ -> eprintf "Invalid promotion, try again: %!"; promote ()

let find_move sq mv =
  let m = Legal.move mv in
  Square.(sq = Move.dst m)

let find_promote sq k mv =
  let m = Legal.move mv in
  Square.(sq = Move.dst m) &&
  Option.exists (Move.promote m) ~f:(Piece.Kind.equal k)

let click mx my = State.update @@ fun ({window; legal; sel; _} as st) ->
  match screen_to_sq window mx my with
  | None -> st
  | Some sq -> match sel with
    | Some (sq', _) when Square.(sq = sq') -> {st with sel = None}
    | None ->
      let moves =
        List.filter legal ~f:(fun m ->
            Square.(sq = Move.src (Legal.move m))) in
      let sel = if List.is_empty moves then None else Some (sq, moves) in
      {st with sel}
    | Some (_, moves) -> match List.find moves ~f:(find_move sq) with
      | None -> st
      | Some m ->
        let pos = Legal.new_position m in
        let m, pos = match Move.promote @@ Legal.move m with
          | None -> m, pos
          | Some _ ->
            promote_prompt ();
            let k = promote () in
            let m = List.find_exn moves ~f:(find_promote sq k) in
            m, Legal.new_position m in
        let legal = Position.legal_moves pos in
        {st with pos; legal; sel = None; prev = Some m}

let poll = State.(gets window) >>= fun window ->
  match Window.poll_event window with
  | Some (`Mouse_button_pressed (mx, my)) -> click mx my
  | None -> State.return ()

let is_fifty_move pos = Position.halfmove pos >= 100

let print_endgame = function
  | Insufficient_material -> printf "Draw by insufficient material\n%!"
  | Fifty_move -> printf "Draw by fifty-move rule\n%!"
  | Stalemate -> printf "Draw by stalemate\n%!"
  | Checkmate c ->
    printf "Checkmate, %s wins\n%!" @@ Piece.Color.to_string_hum c

let check_endgame = State.update @@ fun ({pos; legal; _} as st) ->
  let endgame =
    let in_check = Position.in_check pos in
    if not in_check && Position.is_insufficient_material pos
    then Some Insufficient_material
    (* else if not in_check && is_fifty_move pos then Some Fifty_move *)
    else if List.is_empty legal then
      if in_check
      then Some (Checkmate (Position.inactive pos))
      else Some Stalemate
    else None in
  {st with endgame}

let check_and_print_endgame =
  check_endgame >> State.(gets endgame) >>| fun endgame ->
  Option.iter endgame ~f:print_endgame;
  endgame

let human_move = State.(gets endgame) >>= function
  | Some _ -> State.return None
  | None -> poll >> check_and_print_endgame

let update_player_state :
  type a. Piece.color -> a Player.t -> a -> unit state = fun c player pst ->
  State.update @@ fun st ->
  let player = Player.(T (update player ~f:(fun _ -> pst))) in
  match c with
  | White -> {st with white = Some player}
  | Black -> {st with black = Some player}

let ai_move c player = State.(gets pos) >>= fun pos ->
  let Player.(T player) = player in
  let m, st = Player.choose player pos in
  update_player_state c player st >>
  let pos = Legal.new_position m in
  let legal = Position.legal_moves pos in
  begin State.update @@ fun st ->
    {st with pos; legal; sel = None; prev = Some m}
  end >> check_and_print_endgame

let human_or_ai_move c = function
  | None -> human_move
  | Some player -> State.(gets endgame) >>= function
    | None -> ai_move c player
    | Some _ -> State.return None

let display_board ?(bb = 0L) ?(sq = None) ?(prev = None) pos window =
  Window.clear window;
  Window.paint_board window pos bb sq prev;
  Window.display window

(* Prompt the user to quit when ready. *)
let prompt_end window =
  printf "Enter any key to quit: %!";
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
      "\nNew position has hash %016LX, but %016LX was expected. \
       Position:\n%s\n%!" h' h (Position.Fen.to_string new_pos) ()

let rec main_loop ~delay () = State.(gets window) >>= fun window ->
  if Window.is_open window then
    (* Process input if the game is still playable. *)
    State.(gets pos) >>= fun pos ->
    let active = Position.active pos in
    begin match active with
      | White -> State.(gets white)
      | Black -> State.(gets black)
    end >>= human_or_ai_move active >>= fun endgame ->
    (* New position? *)
    State.(gets pos) >>= fun new_pos ->
    State.(gets legal) >>= fun legal ->
    State.(gets prev) >>= fun prev ->
    (* Print information about position change. *)
    let prev = if not @@ Position.same_hash pos new_pos then begin
        let mv = Option.value_exn prev in
        let m = Legal.move mv in
        printf "%s (%s): %s\n%!"
          (Move.to_string m) (Position.San.of_legal mv)
          (Position.Fen.to_string new_pos);
        printf "Hash: %016LX\n%!" @@ assert_hash new_pos;
        printf "%d legal moves\n%!" @@ List.length legal;
        printf "\n%!";
        Some m
      end else None in
    (* Get the valid squares for our selected piece to move to. *)
    State.(gets sel) >>= begin function
      | None -> State.return (Bb.(to_int64 empty), None)
      | Some (sq, moves) ->
        let bb =
          List.fold moves ~init:Bb.empty ~f:(fun acc mv ->
              let m = Legal.move mv in
              Bb.(acc ++ Move.dst m)) in
        State.return (Bb.to_int64 bb, Some sq)
    end >>= fun (bb, sq) ->
    display_board new_pos window ~bb ~sq ~prev;
    (* Nothing more to do if the game is over. *)
    match endgame with
    | Some _ -> State.return @@ prompt_end window
    | None -> delay (); main_loop ~delay ()
  else State.return ()

let start_with_endgame_check delay = check_and_print_endgame >>= function
  | None -> main_loop ~delay ()
  | Some _ ->
    State.(gets window) >>= fun window ->
    State.(gets pos) >>| fun pos ->
    if Window.is_open window then begin
      display_board pos window;
      prompt_end window
    end

let () = Callback.register "piece_at_square" Position.piece_at_square
let () = Callback.register "string_of_square" Square.to_string

let window_size = 640

external init_fonts : unit -> unit = "ml_init_fonts"
external init_named_values : unit -> unit = "ml_init_named_values"

let go pos ~white ~black ~delay =
  init_fonts ();
  init_named_values ();
  let window = Window.create window_size window_size "chess" in
  let legal = Position.legal_moves pos in
  begin match white with
    | None -> printf "White is human\n%!"
    | Some Player.(T player) ->
      printf "White is AI: %s\n%!" @@ Player.name player
  end;
  begin match black with
    | None -> printf "Black is human\n%!"
    | Some Player.(T player) ->
      printf "Black is AI: %s\n%!" @@ Player.name player
  end;
  printf "\n%!";
  printf "Starting position: %s\n%!" @@ Position.Fen.to_string pos;
  printf "Hash: %016LX\n%!" @@ Position.hash pos;
  printf "%d legal moves\n%!" @@ List.length legal;
  printf "\n%!";
  Monad.State.eval (start_with_endgame_check delay) @@
  State.Fields.create ~window ~pos ~legal
    ~sel:None ~prev:None ~endgame:None
    ~white ~black
