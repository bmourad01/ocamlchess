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

let poll window pos legal sel prev =
  match Window.poll_event window with
  | None -> pos, legal, sel, prev
  | Some (`MouseButtonPressed (mx, my)) ->
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
          let legal = Position.legal_moves pos in
          pos, legal, None, Some m

let rec main_loop window pos legal sel prev =
  if Window.is_open window then begin
    let pos', legal, sel, prev = poll window pos legal sel prev in
    if Position.(pos' <> pos) then begin
      printf "%s: %s\n%!"
        (Option.value_map prev ~default:"(none)" ~f:Move.to_string)
        (Position.Fen.to_string pos');
      printf "%d legal moves\n%!" (List.length legal);
      printf "\n%!"
    end;
    let bb, sq = match sel with
      | None -> Bitboard.(to_int64 empty), None
      | Some (sq, moves) ->
        let bb =
          List.fold moves ~init:Bitboard.empty ~f:(fun acc (m, _) ->
              Bitboard.(acc ++ Move.dst m)) in
        Bitboard.to_int64 bb, Some sq in
    Window.clear window;
    Window.paint_board window pos' bb sq prev;
    Window.display window;
    main_loop window pos' legal sel prev
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
    main_loop window pos legal None None
