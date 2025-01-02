(* Standard Algebraic Notation (SAN). *)

open Core_kernel [@@warning "-D"]
open Position_common
open Position_movegen_helpers

let disambiguate ppf parent k ~src ~dst =
  let a = Position_analysis.create parent in
  (* More than one checker means it's a king move, which is unambiguous. *)
  if a.Position_analysis.num_checkers <= 1 then
    let rank, file = Square.decomp src in
    (* Find all the other pieces of the same kind and generate their move
       bitboards. *)
    collect_kind parent k |> List.filter ~f:(fun (sq, c) ->
        Square.(sq <> src) && Piece.Color.(c = parent.active)) |>
    List.map ~f:(fun (sq, _) -> sq, Position_movegen.bb_of_kind sq k a) |>
    List.filter ~f:(fun (_, b) -> Bb.(dst @ b)) |> function
    | [] -> ()
    | moves ->
      let search x f =
        not @@ List.exists moves ~f:(fun (sq, _) -> f sq = x) in
      (* First try to distinguish by file, then by rank, and finally the
         departing square. *)
      if search file Square.file then
        Format.fprintf ppf "%c%!" @@ Square.File.to_char file
      else if search rank Square.rank then
        Format.fprintf ppf "%c%!" @@ Square.Rank.to_char rank
      else Format.fprintf ppf "%a%!" Square.pp src

let pp ppf child =
  let src, dst, promote = Move.decomp @@ Position_child.move child in
  let pos = Position_child.self child in
  let num_checkers = Bb.count @@ checkers pos in
  let checkmate = num_checkers <> 0 && List.is_empty @@ children pos in
  begin match Position_child.castle_side child with
    (* Castling *)
    | Some Cr.Kingside -> Format.fprintf ppf "O-O%!"
    | Some Cr.Queenside -> Format.fprintf ppf "O-O-O%!"
    | None ->
      let p = piece_at_square_exn pos dst in
      let p = match promote with
        | Some _ -> Piece.with_kind p Pawn
        | None -> p in
      (* Piece being moved *)
      let dis = disambiguate ppf ~src ~dst @@ Position_child.parent child in
      begin match Piece.kind p with
        | Piece.Pawn -> if Uopt.is_none child.capture
          then Format.fprintf ppf "%a%!" Square.pp dst
          else Format.fprintf ppf "%c%!" @@ Square.file_char src
        | Piece.Knight -> Format.fprintf ppf "N%!"; dis Knight
        | Piece.Bishop -> Format.fprintf ppf "B%!"; dis Bishop
        | Piece.Rook   -> Format.fprintf ppf "R%!"; dis Rook
        | Piece.Queen  -> Format.fprintf ppf "Q%!"; dis Queen
        | Piece.King   -> Format.fprintf ppf "K%!"
      end;
      (* Capture *)
      Uopt.to_option child.capture |>
      Option.iter ~f:(fun _ -> Format.fprintf ppf "x%!");
      (* Destination *)
      if not (Piece.is_pawn p && Uopt.is_none child.capture) then
        Format.fprintf ppf "%a%!" Square.pp dst;
      (* Promotion *)
      Option.iter promote ~f:(function
          | Move.Promote.Knight -> Format.fprintf ppf "=N%!"
          | Move.Promote.Bishop -> Format.fprintf ppf "=B%!"
          | Move.Promote.Rook   -> Format.fprintf ppf "=R%!"
          | Move.Promote.Queen  -> Format.fprintf ppf "=Q%!");
  end;
  (* Checkmate or check *)
  if checkmate then Format.fprintf ppf "#%!"
  else if num_checkers = 1 then Format.fprintf ppf "+%!"
  else if num_checkers = 2 then Format.fprintf ppf "++%!"

let of_child child = Format.asprintf "%a%!" pp child

let of_move pos m = Option.(make_move pos m >>| of_child)

let of_move_exn pos m = match of_move pos m with
  | Some s -> s
  | None ->
    invalid_argf "Illegal move %s for position %s"
      (Move.to_string m) (Position_fen.to_string pos) ()

let of_string s pos =
  children pos |> List.find ~f:(fun m -> String.equal s @@ of_child m)
