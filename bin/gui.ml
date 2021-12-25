open Core_kernel
open Chess
open Monads.Std

module Lm = Position.Legal_move
module Lms = Position.Legal_moves

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
      legal : Position.legal_moves;
      sel : (Square.t * Position.legal_move list) option;
      prev : Move.t option;
      endgame : endgame option;
      white : Player.t option;
      black : Player.t option;
    } [@@deriving fields]
  end

  include T
  include Monad.State.Make(T)(Monad.Ident)
end

open State.Syntax

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

let rec promote () = try match In_channel.(input_line stdin) with
  | Some "n" -> Piece.Knight
  | Some "b" -> Piece.Bishop
  | Some "r" -> Piece.Rook
  | Some "q" -> Piece.Queen
  | _ -> assert false
  with _ ->
    eprintf "Invalid promotion, try again: %!";
    promote ()

let find_move sq mv =
  let m = Lm.move mv in
  Square.(sq = Move.dst m)

let find_promote sq k mv =
  let m = Lm.move mv in
  Square.(sq = Move.dst m) &&
  Option.exists (Move.promote m) ~f:(Piece.Kind.equal k)

let click mx my = State.update @@ fun ({window; legal; sel; _} as st) ->
  match screen_to_sq window mx my with
  | None -> st
  | Some sq -> match sel with
    | Some (sq', _) when Square.(sq = sq') -> {st with sel = None}
    | None ->
      let moves =
        Lms.moves legal |> List.filter ~f:(fun mv ->
            Square.(sq = Move.src (Lm.move mv))) in
      let sel = if List.is_empty moves then None else Some (sq, moves) in
      {st with sel}
    | Some (_, moves) -> match List.find moves ~f:(find_move sq) with
      | None -> st
      | Some mv ->
        let m, pos = Lm.decomp mv in
        let m, pos = match Move.promote m with
          | None -> m, pos
          | Some _ ->
            promote_prompt ();
            let k = promote () in
            List.find_exn moves ~f:(find_promote sq k) |>
            Lm.decomp in
        let legal = Position.legal_moves pos in
        {st with pos; legal; sel = None; prev = Some m}

let poll = State.(gets window) >>= fun window ->
  match Window.poll_event window with
  | Some (`Mouse_button_pressed (mx, my)) -> click mx my
  | None -> State.return ()

let is_insufficient_material pos =
  let king = Position.king pos in
  let active = Position.active_board pos in
  let occupied = Position.all_board pos in
  Bitboard.((king = occupied) || ((king & active) = king))

let is_fifty_move pos = Position.halfmove pos >= 100

let print_endgame = function
  | Insufficient_material -> printf "Draw by insufficient material\n%!"
  | Fifty_move -> printf "Draw by fifty-move rule\n%!"
  | Stalemate -> printf "Draw by stalemate\n%!"
  | Checkmate c ->
    printf "Checkmate, %s wins\n%!" @@ Piece.Color.to_string_hum c

let check_endgame = State.update @@ fun ({pos; legal; _} as st) ->
  let endgame =
    if is_insufficient_material pos then Some Insufficient_material
    else if is_fifty_move pos then Some Fifty_move
    else if List.is_empty @@ Lms.moves legal then
      if Position.in_check pos
      then Some (Checkmate (Position.enemy pos))
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

let ai_move player = State.(gets legal) >>= fun legal ->
  let m, pos = Lm.decomp @@ Player.choose player legal in
  let legal = Position.legal_moves pos in
  begin State.update @@ fun st ->
    {st with pos; legal; sel = None; prev = Some m}
  end >> check_and_print_endgame

let human_or_ai_move = function
  | None -> human_move
  | Some player -> State.(gets endgame) >>= function
    | None -> ai_move player
    | Some _ -> State.return None

let display_board
    ?(bb = 0L)
    ?(sq = None)
    ?(prev = None)
    pos window =
  Window.clear window;
  Window.paint_board window pos bb sq prev;
  Window.display window

(* Prompt the user to quit when ready. *)
let prompt_end window =
  printf "Enter any key to quit: %!";
  ignore @@ In_channel.(input_line stdin);
  Window.close window

let rec main_loop ~delay () = State.(gets window) >>= fun window ->
  if Window.is_open window then
    (* Process input if the game is still playable. *)
    State.(gets pos) >>= fun pos ->
    begin match Position.active pos with
      | White -> State.(gets white)
      | Black -> State.(gets black)
    end >>= human_or_ai_move >>= fun endgame ->
    (* New position? *)
    State.(gets pos) >>= fun pos' ->
    State.(gets legal) >>= fun legal ->
    State.(gets prev) >>= fun prev ->
    (* Print information about position change. *)
    if Position.(pos' <> pos) then begin
      printf "%s: %s\n%!"
        (Option.value_map prev ~default:"(none)" ~f:Move.to_string)
        (Position.Fen.to_string pos');
      printf "%d legal moves\n%!" (List.length @@ Lms.moves legal);
      printf "\n%!"
    end;
    (* Get the valid squares for our selected piece to move to. *)
    State.(gets sel) >>= begin function
      | None -> State.return (Bitboard.(to_int64 empty), None)
      | Some (sq, moves) ->
        let bb =
          List.fold moves ~init:Bitboard.empty ~f:(fun acc mv ->
              let m = Lm.move mv in
              Bitboard.(acc ++ Move.dst m)) in
        State.return (Bitboard.to_int64 bb, Some sq)
    end >>= fun (bb, sq) ->
    display_board pos' window ~bb ~sq ~prev;
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

external init_fonts : unit -> bool = "ml_init_fonts"

let go pos ~white ~black ~delay =
  if init_fonts () then
    let window = Window.create window_size window_size "chess" in
    let legal = Position.legal_moves pos in
    printf "Starting position: %s\n%!" (Position.Fen.to_string pos);
    printf "%d legal moves\n%!" (List.length @@ Lms.moves legal);
    printf "\n%!";
    Monad.State.eval (start_with_endgame_check delay) @@
    State.Fields.create ~window ~pos ~legal
      ~sel:None ~prev:None ~endgame:None
      ~white ~black
