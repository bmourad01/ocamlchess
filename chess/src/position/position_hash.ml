open Core_kernel [@@warning "-D"]
open Position_common
open Monads.Std

(* Helpers for updating the hash. *)
module Update = struct
  let[@inline] flip h x = Int64.(h lxor x)
  let[@inline] piece c k sq h = flip h @@ Zobrist.piece c k sq
  let[@inline] castle c s h = flip h @@ Zobrist.castle c s
  let[@inline] en_passant_sq sq h = flip h @@ Zobrist.en_passant sq
  let[@inline] active_player h = flip h Zobrist.white_to_move

  let[@inline] en_passant pos h =
    if Uopt.is_some pos.en_passant then
      let ep = Uopt.unsafe_value pos.en_passant in
      if has_pawn_threat pos ep then en_passant_sq ep h else h
    else h

  let[@inline] castle_test cr c s h =
    if Cr.mem cr c s then castle c s h else h
end

module M = Monad.State.Make(Int64)(Monad.Ident)

(* Get the hash of a position. *)
let of_position pos = Monad.State.exec begin
    let open M.Syntax in
    (* Piece placement.. *)
    collect_all pos |> M.List.iter ~f:(fun (sq, p) ->
        let c, k = Piece.decomp p in
        M.update @@ Update.piece c k sq) >>= fun () ->
    (* Castling rights *)
    M.update @@ Update.castle_test pos.castle White Kingside  >>= fun () ->
    M.update @@ Update.castle_test pos.castle White Queenside >>= fun () ->
    M.update @@ Update.castle_test pos.castle Black Kingside  >>= fun () ->
    M.update @@ Update.castle_test pos.castle Black Queenside >>= fun () ->
    (* En passant *)
    M.update @@ Update.en_passant pos >>= fun () ->
    (* White to move. *)
    match pos.active with
    | White -> M.update Update.active_player
    | Black -> M.return ()
  end 0L

let of_pawn_king pos = Monad.State.exec begin
    let open M.Syntax in
    collect_kind pos Pawn |> M.List.iter ~f:(fun (sq, c) ->
        M.update @@ Update.piece c Pawn sq) >>= fun () ->
    collect_kind pos King |> M.List.iter ~f:(fun (sq, c) ->
        M.update @@ Update.piece c King sq)
  end 0L
