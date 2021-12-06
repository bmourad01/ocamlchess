open Core_kernel
open Chess

module Window = struct
  type t

  type event = [
    | `MouseButtonPressed of int * int
  ]

  external create : int -> int -> string -> t = "ml_window_create"
  external size : t -> (int * int) = "ml_window_size"
  external is_open : t -> bool = "ml_window_is_open"
  external poll_event : t -> event option = "ml_window_poll_event"
  external clear : t -> unit = "ml_window_clear"
  external display : t -> unit = "ml_window_display"

  external paint_board :
    t -> Position.t -> int64 -> Square.t option -> Move.t option -> unit =
    "ml_window_paint_board"
end

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

let click window pos legal sel prev mx my =
  match screen_to_sq window mx my with
  | None -> pos, legal, sel, prev
  | Some sq -> match sel with
    | None ->
      let moves =
        List.filter legal ~f:(fun (m, _) -> Square.(sq = Move.src m)) in
      let sel = if List.is_empty moves then None else Some (sq, moves) in
      pos, legal, sel, prev
    | Some (sq', _) when Square.(sq = sq') ->
      pos, legal, None, prev
    | Some (_, moves) ->
      let move =
        List.find moves ~f:(fun (m, _) -> Square.(sq = Move.dst m)) in
      match move with
      | None -> pos, legal, sel, prev
      | Some (m, pos) ->
        let m, pos = match Move.promote m with
          | None -> m, pos
          | Some _ ->
            promote_prompt ();
            let k = promote () in
            List.find_exn moves ~f:(fun (m, _) ->
                Square.(sq = Move.dst m) &&
                Option.exists (Move.promote m) ~f:(Piece.Kind.equal k)) in
        let legal = Position.legal_moves pos in
        pos, legal, None, Some m

let poll window pos legal sel prev =
  match Window.poll_event window with
  | None -> pos, legal, sel, prev
  | Some (`MouseButtonPressed (mx, my)) ->
    click window pos legal sel prev mx my

let is_insufficient_material pos =
  let king = Position.king pos in
  let active = Position.active_board pos in
  let occupied = Position.all_board pos in
  Bitboard.((king = occupied) || ((king & active) = king))

let is_fifty_move pos = Position.halfmove pos >= 100

let in_check pos =
  let active_board = Position.active_board pos in
  let king = Position.king pos in
  let enemy = Piece.Color.opposite @@ Position.active pos in
  let attacks = Position.Attacks.all pos enemy ~ignore_same:true in
  Bitboard.((active_board & king & attacks) <> empty)

let print_endgame = function
  | `Insufficient_material -> printf "Draw by insufficient material\n%!"
  | `Fifty_move -> printf "Draw by fifty-move rule\n%!"
  | `Checkmate c -> printf "Checkmate, %s wins\n%!" @@ Piece.Color.to_string_hum c
  | `Stalemate -> printf "Draw by stalemate\n%!"

let check_endgame pos legal =
  if is_insufficient_material pos then Some `Insufficient_material
  else if is_fifty_move pos then Some `Fifty_move
  else if List.is_empty legal then
    if in_check pos
    then Some (`Checkmate (Position.active pos |> Piece.Color.opposite))
    else Some `Stalemate
  else None 

let rec main_loop window pos legal sel prev endgame =
  if Window.is_open window then begin
    (* Process input if the game is still playable. *)
    let pos', legal, sel, prev =
      if Option.is_some endgame then pos, legal, sel, prev
      else poll window pos legal sel prev in
    (* Check if the game is over. *)
    let endgame = match endgame with
      | Some _ -> endgame
      | None ->
        let endgame = check_endgame pos' legal in
        Option.iter endgame ~f:print_endgame;
        endgame in
    (* Print information about position change. *)
    if Position.(pos' <> pos) then begin
      printf "%s: %s\n%!"
        (Option.value_map prev ~default:"(none)" ~f:Move.to_string)
        (Position.Fen.to_string pos');
      printf "%d legal moves\n%!" (List.length legal);
      printf "\n%!"
    end;
    (* Get the valid squares for our selected piece to move to. *)
    let bb, sq = match sel with
      | None -> Bitboard.(to_int64 empty), None
      | Some (sq, moves) ->
        let bb =
          List.fold moves ~init:Bitboard.empty ~f:(fun acc (m, _) ->
              Bitboard.(acc ++ Move.dst m)) in
        Bitboard.to_int64 bb, Some sq in
    (* Display the board. *)
    Window.clear window;
    Window.paint_board window pos' bb sq prev;
    Window.display window;
    main_loop window pos' legal sel prev endgame
  end

let () = Callback.register "piece_at_square" Position.piece_at_square
let () = Callback.register "string_of_square" Square.to_string

let window_size = 640

external init_fonts : unit -> bool = "ml_init_fonts"

let () =
  if init_fonts () then
    let window = Window.create window_size window_size "chess" in
    let pos =
      if Array.length Sys.argv > 1
      then Position.Fen.of_string_exn Sys.argv.(1)
      else Position.start in
    let legal = Position.legal_moves pos in
    printf "Starting position: %s\n%!" (Position.Fen.to_string pos);
    printf "%d legal moves\n%!" (List.length legal);
    printf "\n%!";
    main_loop window pos legal None None None
