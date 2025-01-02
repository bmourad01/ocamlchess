open Core_kernel [@@warning "-D"]
open Position_common
open Monads.Std

module Error = struct
  type t =
    | Empty_board
    | Full_board
    | Invalid_number_of_kings of Piece.color * int
    | Kings_not_separated
    | Inactive_in_check of Piece.color
    | Invalid_number_of_checkers of Piece.color * int
    | Invalid_two_checkers of Piece.color * Piece.kind * Piece.kind
    | Invalid_number_of_pawns of Piece.color * int
    | Pawns_in_back_rank of Piece.color
    | Missing_pawn_en_passant of Piece.color
    | Invalid_en_passant_square of Square.t
    | Invalid_extra_pieces of Piece.color * int
    | Invalid_number_of_pieces of Piece.color * int
    | Invalid_castling_rights of Piece.color * Piece.kind
    | En_passant_wrong_halfmove
    | Invalid_halfmove
    | Invalid_fullmove

  let pp ppf = function
    | Empty_board -> Format.fprintf ppf "Board is empty%!"
    | Full_board -> Format.fprintf ppf "Board is full%!"
    | Invalid_number_of_kings (c, n) ->
      Format.fprintf ppf "Invalid number of %a kings (%d)%!"
        Piece.Color.pp_hum c n
    | Kings_not_separated ->
      Format.fprintf ppf "Kings must be separated by at least one square%!"
    | Inactive_in_check c ->
      Format.fprintf ppf "Inactive player %a is in check%!"
        Piece.Color.pp_hum c
    | Invalid_number_of_checkers (c, n) ->
      Format.fprintf ppf "Player %a has %d checkers, max is two%!"
        Piece.Color.pp_hum c n
    | Invalid_two_checkers (c, k1, k2) ->
      Format.fprintf ppf "Player %a has invalid two checkers: %a and %a%!"
        Piece.Color.pp_hum c Piece.Kind.pp_hum k1 Piece.Kind.pp_hum k2
    | Invalid_number_of_pawns (c, n) ->
      Format.fprintf ppf "Invalid number of %a pawns (%d)%!"
        Piece.Color.pp_hum c n
    | Pawns_in_back_rank c ->
      Format.fprintf ppf "Player %a has pawns in the back rank%!"
        Piece.Color.pp_hum c
    | Missing_pawn_en_passant c ->
      Format.fprintf ppf "Missing %a pawn in front of en passant square%!"
        Piece.Color.pp_hum c
    | Invalid_en_passant_square sq ->
      Format.fprintf ppf "Invalid en passant square %a%!" Square.pp sq
    | Invalid_extra_pieces (c, n) ->
      Format.fprintf ppf "Invalid number of extra %a pieces (%d)%!"
        Piece.Color.pp_hum c n
    | Invalid_number_of_pieces (c, n) ->
      Format.fprintf ppf "Invalid number of %a pieces (%d)%!"
        Piece.Color.pp_hum c n
    | Invalid_castling_rights (c, k) ->
      Format.fprintf ppf "Invalid castling rights, %a %a moved%!"
        Piece.Color.pp_hum c Piece.Kind.pp_hum k
    | En_passant_wrong_halfmove ->
      Format.fprintf ppf
        "En passant square is set, but halfmove clock is not zero%!"
    | Invalid_halfmove -> Format.fprintf ppf "Invalid halfmove clock%!"
    | Invalid_fullmove -> Format.fprintf ppf "Invalid fullmove clock%!"

  let to_string t = Format.asprintf "%a%!" pp t
end

type error = Error.t

module E = Monad.Result.Make(Error)(Monad.Ident)

open E.Syntax

module Trivial = struct
  let check_empty pos =
    if Bb.(all_board pos = empty) then E.fail Empty_board else E.return ()

  let check_full pos =
    if Bb.(all_board pos = full) then E.fail Empty_board else E.return ()

  let check_sixteen pos c =
    let n = Bb.count @@ board_of_color pos c in
    if n > 16 then E.fail @@ Invalid_number_of_pieces (c, n)
    else E.return ()

  let go pos =
    check_empty pos >>= fun () ->
    check_full pos >>= fun () ->
    check_sixteen pos White >>= fun () ->
    check_sixteen pos Black
end

module King = struct
  let check_count pos =
    let wk = Bb.(count (pos.white & pos.king)) in
    if wk <> 1 then E.fail @@ Invalid_number_of_kings (White, wk)
    else
      let bk = Bb.(count (pos.black & pos.king)) in
      if bk <> 1 then E.fail @@ Invalid_number_of_kings (Black, bk)
      else E.return ()

  let check_sep pos =
    let k1 = Bb.(first_set_exn (pos.white & pos.king)) in
    let k2 = Bb.(first_set_exn (pos.black & pos.king)) in
    if Square.chebyshev k1 k2 <= 1
    then E.fail Kings_not_separated
    else E.return ()

  let go pos =
    check_count pos >>= fun () ->
    check_sep pos
end

module Checks = struct
  let check_inactive_in_check pos =
    let b = inactive_board pos in
    let attacks = Position_attacks.all pos pos.active in
    if Bb.((b & pos.king & attacks) <> empty)
    then E.fail @@ Inactive_in_check (inactive pos)
    else E.return ()

  let check_checkers pos =
    let checkers = checkers pos in
    let num_checkers = Bb.count checkers in
    if num_checkers >= 3
    then E.fail @@ Invalid_number_of_checkers (pos.active, num_checkers)
    else
      let checkers = Bb.fold checkers ~init:[] ~f:(fun acc sq ->
          which_kind_exn pos sq :: acc) in
      match checkers with
      | [Pawn; Pawn] ->
        E.fail @@ Invalid_two_checkers (pos.active, Pawn, Pawn)
      | [Pawn; Knight] | [Knight; Pawn] ->
        E.fail @@ Invalid_two_checkers (pos.active, Pawn, Knight)
      | [Pawn; Bishop] | [Bishop; Pawn] ->
        E.fail @@ Invalid_two_checkers (pos.active, Pawn, Bishop)
      | [Knight; Knight] ->
        E.fail @@ Invalid_two_checkers (pos.active, Knight, Knight)
      | [Bishop; Bishop] ->
        E.fail @@ Invalid_two_checkers (pos.active, Bishop, Bishop)
      | _ -> E.return ()

  let go pos = 
    check_inactive_in_check pos >>= fun () ->
    check_checkers pos
end

module Pawn = struct
  let check_count pos =
    let wp = Bb.(count (pos.white & pos.pawn)) in
    if wp > 8 then E.fail @@ Invalid_number_of_pawns (White, wp)
    else
      let bp = Bb.(count (pos.black & pos.pawn)) in
      if bp > 8 then E.fail @@ Invalid_number_of_pawns (Black, bp)
      else E.return ()

  let check_back_rank pos =
    let mask = Bb.(rank_1 + rank_8) in
    if Bb.((pos.white & pos.pawn & mask) <> empty)
    then E.fail @@ Pawns_in_back_rank White
    else if Bb.((pos.black & pos.pawn & mask) <> empty)
    then E.fail @@ Pawns_in_back_rank Black
    else E.return ()

  let check_en_passant pos =
    let ep = pos.en_passant in
    if Uopt.is_none ep then E.return ()
    else
      let ep = Uopt.unsafe_value ep in
      let rank, file = Square.decomp ep in
      if rank = Square.Rank.three then
        let sq = Square.create_exn ~rank:(succ rank) ~file in
        let p = piece_at_square_uopt pos sq in
        if Uopt.is_none p then E.fail @@ Missing_pawn_en_passant White
        else
          let p = Uopt.unsafe_value p in
          if Piece.(is_white p && is_pawn p) then E.return ()
          else E.fail @@ Missing_pawn_en_passant White
      else if rank = Square.Rank.six then
        let sq = Square.create_exn ~rank:(pred rank) ~file in
        let p = piece_at_square_uopt pos sq in
        if Uopt.is_none p then E.fail @@ Missing_pawn_en_passant Black
        else
          let p = Uopt.unsafe_value p in
          if Piece.(is_black p && is_pawn p) then E.return ()
          else E.fail @@ Missing_pawn_en_passant Black
      else E.fail @@ Invalid_en_passant_square ep

  let check_promotions pos c b =
    let num_pawn   = Bb.(count (pos.pawn & b)) in
    let num_knight = Bb.(count (pos.knight & b)) in
    let num_bishop = Bb.(count (pos.bishop & b)) in
    let num_rook   = Bb.(count (pos.rook & b)) in
    let num_queen  = Bb.(count (pos.queen & b)) in
    let extra =
      max 0 (num_knight - 2) +
      max 0 (num_bishop - 2) +
      max 0 (num_rook   - 2) +
      max 0 (num_queen  - 1) in
    if extra > (8 - num_pawn) then E.fail @@ Invalid_extra_pieces (c, extra)
    else E.return ()

  let go pos =
    check_count pos >>= fun () ->
    check_back_rank pos >>= fun () ->
    check_en_passant pos >>= fun () ->
    check_promotions pos White pos.white >>= fun () ->
    check_promotions pos Black pos.black
end

module Castling = struct
  let check_king_moved pos c b =
    if Cr.(mem pos.castle c Kingside || mem pos.castle c Queenside) then
      let sq = match c with
        | White -> Square.e1
        | Black -> Square.e8 in
      if Bb.(sq @ (b & pos.king)) then E.return ()
      else E.fail @@ Invalid_castling_rights (c, King)
    else E.return ()

  let check_rook_moved pos c b s sq =
    if Cr.mem pos.castle c s then
      if Bb.(sq @ (b & pos.rook)) then E.return ()
      else E.fail @@ Invalid_castling_rights (c, Rook)
    else E.return ()

  let go pos =
    check_king_moved pos White pos.white >>= fun () ->
    check_king_moved pos Black pos.black >>= fun () ->
    check_rook_moved pos White pos.white Kingside  Square.h1 >>= fun () ->
    check_rook_moved pos White pos.white Queenside Square.a1 >>= fun () ->
    check_rook_moved pos Black pos.black Kingside  Square.h8 >>= fun () ->
    check_rook_moved pos Black pos.black Queenside Square.a8
end

module Half_and_fullmove = struct
  let check_en_passant pos =
    if Uopt.is_some pos.en_passant && pos.halfmove <> 0
    then E.fail En_passant_wrong_halfmove
    else E.return ()

  let check_both pos =
    if pos.halfmove >
       ((pos.fullmove - 1) * 2) + (Piece.Color.to_int pos.active)
    then E.fail Invalid_halfmove
    else if pos.halfmove < 0 then E.fail Invalid_halfmove
    else if pos.fullmove < 1 then E.fail Invalid_fullmove
    else E.return ()

  let go pos =
    check_en_passant pos >>= fun () ->
    check_both pos
end

let check pos =
  Trivial.go pos >>= fun () ->
  King.go pos >>= fun () ->
  Checks.go pos >>= fun () ->
  Pawn.go pos >>= fun () ->
  Castling.go pos >>= fun () ->
  Half_and_fullmove.go pos
