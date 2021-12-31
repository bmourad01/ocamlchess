open Core_kernel
open Monads.Std

module Pre = Precalculated
module Bb = Bitboard

module T = struct
  (* We'll use mutable fields since, when applying moves, this has a
     performance advantage over a typical state monad pattern (where
     we are making a new copy every time we update a field). *)
  type t = {
    mutable white : Bb.t;
    mutable black : Bb.t;
    mutable pawn : Bb.t;
    mutable knight : Bb.t;
    mutable bishop : Bb.t;
    mutable rook : Bb.t;
    mutable queen : Bb.t;
    mutable king : Bb.t;
    mutable active : Piece.color;
    mutable castle : Castling_rights.t;
    mutable en_passant : Square.t option;
    mutable halfmove : int;
    mutable fullmove : int;
  } [@@deriving compare, equal, fields, sexp]

  let copy pos = {
    white      = pos.white;
    black      = pos.black;
    pawn       = pos.pawn;
    knight     = pos.knight;
    bishop     = pos.bishop;
    rook       = pos.rook;
    queen      = pos.queen;
    king       = pos.king;
    active     = pos.active;
    castle     = pos.castle;
    en_passant = pos.en_passant;
    halfmove   = pos.halfmove;
    fullmove   = pos.fullmove;
  }
end

include T

let enemy pos = Piece.Color.opposite pos.active

(* Bitboard accessors *)

let[@inline] all_board pos = Bb.(pos.white + pos.black)

let[@inline] board_of_color pos = function
  | Piece.White -> pos.white
  | Piece.Black -> pos.black

let[@inline] active_board pos = board_of_color pos pos.active
let[@inline] enemy_board pos = board_of_color pos @@ enemy pos

let[@inline] board_of_kind pos = function
  | Piece.Pawn -> pos.pawn
  | Piece.Knight -> pos.knight
  | Piece.Bishop -> pos.bishop
  | Piece.Rook -> pos.rook
  | Piece.Queen -> pos.queen
  | Piece.King -> pos.king

let[@inline] board_of_piece pos p =
  let c, k = Piece.decomp p in
  Bb.(board_of_color pos c & board_of_kind pos k)

let[@inline] is_en_passant pos sq = match pos.en_passant with
  | Some sq' -> Square.(sq = sq')
  | None -> false

(* Piece lookup *)

let which_color pos sq =
  let open Bb.Syntax in
  if sq @ pos.white then Some Piece.White
  else if sq @ pos.black then Some Piece.Black
  else None

let which_color_exn pos sq =
  let open Bb.Syntax in
  if sq @ pos.white then Piece.White
  else if sq @ pos.black then Piece.Black
  else invalid_arg @@
    sprintf "No piece exists at square %s" (Square.to_string sq)

let which_kind pos sq =
  let open Bb.Syntax in
  if sq @ pos.pawn then Some Piece.Pawn
  else if sq @ pos.knight then Some Piece.Knight
  else if sq @ pos.bishop then Some Piece.Bishop
  else if sq @ pos.rook then Some Piece.Rook
  else if sq @ pos.queen then Some Piece.Queen
  else if sq @ pos.king then Some Piece.King
  else None

let which_kind_exn pos sq =
  let open Bb.Syntax in
  if sq @ pos.pawn then Piece.Pawn
  else if sq @ pos.knight then Piece.Knight
  else if sq @ pos.bishop then Piece.Bishop
  else if sq @ pos.rook then Piece.Rook
  else if sq @ pos.queen then Piece.Queen
  else if sq @ pos.king then Piece.King
  else invalid_arg @@
    sprintf "No piece exists at square %s" (Square.to_string sq)

let find_color pos c =
  board_of_color pos c |> Bb.fold ~init:[] ~f:(fun acc sq ->
      which_kind pos sq |> Option.value_map ~default:acc
        ~f:(fun k -> (sq, k) :: acc))

let find_active pos = find_color pos pos.active

let find_kind pos k =
  board_of_kind pos k |> Bb.fold ~init:[] ~f:(fun acc sq ->
      which_color pos sq |> Option.value_map ~default:acc
        ~f:(fun c -> (sq, c) :: acc))

let find_piece pos p =
  board_of_piece pos p |> Bb.fold ~init:[] ~f:(fun acc sq -> sq :: acc)

let piece_at_square pos sq =
  let open Option.Monad_infix in
  which_color pos sq >>= fun c -> which_kind pos sq >>| Piece.create c

let piece_at_square_exn pos sq =
  Piece.create (which_color_exn pos sq) (which_kind_exn pos sq)

let all_pieces pos =
  all_board pos |> Bb.fold ~init:[] ~f:(fun acc sq ->
      piece_at_square pos sq |> Option.value_map ~default:acc
        ~f:(fun p -> (sq, p) :: acc))

(* Attack patterns. *)

module Attacks = struct
  (* Useful when excluding squares that are occupied by our color. *)
  let[@inline] ignore_color pos c b = Bb.(b - board_of_color pos c)

  (* Generate for a particular color and kind *)
  let[@inline] gen ?(ignore_same = true) pos c k f =
    let open Bb.Syntax in
    Piece.create c k |> board_of_piece pos |> Bb.fold
      ~init:Bb.empty ~f:(fun acc sq -> acc + f sq) |>
    fun b -> if ignore_same then ignore_color pos c b else b

  let[@inline] pawn ?(ignore_same = true) pos c =
    gen pos c Pawn ~ignore_same @@ fun sq -> Pre.pawn_capture sq c

  let[@inline] knight ?(ignore_same = true) pos c =
    gen pos c Knight Pre.knight ~ignore_same

  (* Get the occupied squares for the board. `king_danger` indicates that the
     king of the opposite color should be ignored, so that sliding attacks
     can "see through" the enemy king. This is useful when the king is blocking
     the attack of a sliding piece. *)
  let[@inline] occupied pos c king_danger =
    let open Bb.Syntax in
    if king_danger then
      let p = Piece.(create (Color.opposite c) King) in
      all_board pos - board_of_piece pos p
    else all_board pos

  let[@inline] bishop ?(ignore_same = true) ?(king_danger = false) pos c =
    let occupied = occupied pos c king_danger in
    gen pos c Bishop ~ignore_same @@ fun sq -> Pre.bishop sq occupied

  let[@inline] rook ?(ignore_same = true) ?(king_danger = false) pos c =
    let occupied = occupied pos c king_danger in
    gen pos c Rook ~ignore_same @@ fun sq -> Pre.rook sq occupied

  let[@inline] queen ?(ignore_same = true) ?(king_danger = false) pos c =
    let occupied = occupied pos c king_danger in
    gen pos c Queen ~ignore_same @@ fun sq -> Pre.queen sq occupied

  let[@inline] king ?(ignore_same = true) pos c =
    gen pos c King Pre.king ~ignore_same

  let[@inline] pre_of_kind sq occupied c = function
    | Piece.Pawn -> Pre.pawn_capture sq c
    | Piece.Knight -> Pre.knight sq
    | Piece.Bishop -> Pre.bishop sq occupied
    | Piece.Rook -> Pre.rook sq occupied
    | Piece.Queen -> Pre.queen sq occupied
    | Piece.King -> Pre.king sq

  let[@inline] aux ?(ignore_same = true) ?(king_danger = false) pos c ~f =
    let open Bb.Syntax in
    let occupied = occupied pos c king_danger in
    find_color pos c |> List.fold ~init:Bb.empty ~f:(fun acc (sq, k) ->
        if f k then acc + pre_of_kind sq occupied c k else acc) |>
    fun b -> if ignore_same then ignore_color pos c b else b

  let[@inline] all ?(ignore_same = true) ?(king_danger = false) pos c =
    aux pos c ~ignore_same ~king_danger ~f:(fun _ -> true)

  let[@inline] sliding ?(ignore_same = true) ?(king_danger = false) pos c =
    aux pos c ~ignore_same ~king_danger ~f:(function
        | Piece.(Bishop | Rook | Queen) -> true
        | _ -> false)

  let[@inline] non_sliding ?(ignore_same = true) pos c =
    aux pos c ~ignore_same ~f:(function
        | Piece.(Bishop | Rook | Queen) -> false
        | _ -> true)
end

let in_check pos =
  let active_board = active_board pos in
  let attacks = Attacks.all pos (enemy pos) ~ignore_same:true in
  Bb.((active_board & pos.king & attacks) <> empty)

(* Attacks of all piece kinds, starting from the king, intersected with
   the squares occupied by enemy pieces. *)
let[@inline] checkers pos ~king_sq ~enemy_board ~occupied =
  let open Bb.Syntax in
  let p = Pre.pawn_capture king_sq pos.active & pos.pawn in
  let n = Pre.knight king_sq & pos.knight in
  let bishop = Pre.bishop king_sq occupied in
  let rook = Pre.rook king_sq occupied in
  let bq = bishop & (pos.bishop + pos.queen) in
  let rq = rook & (pos.rook + pos.queen) in
  let k = Pre.king king_sq & pos.king in
  (p + n + bq + rq + k) & enemy_board

(* Validation *)

module Valid = struct
  module Error = struct
    type t =
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

    let to_string = function
      | Invalid_number_of_kings (c, n) ->
        sprintf "Invalid number of %s kings (%d)"
          (Piece.Color.to_string_hum c) n
      | Kings_not_separated ->
        sprintf "Kings must be separated by at least one square"
      | Inactive_in_check c ->
        sprintf "Inactive player %s is in check" @@
        Piece.Color.to_string_hum c
      | Invalid_number_of_checkers (c, n) ->
        sprintf "Player %s has %d checkers, max is two"
          (Piece.Color.to_string_hum c) n
      | Invalid_two_checkers (c, k1, k2) ->
        sprintf "Player %s has invalid two checkers: %s and %s"
          (Piece.Color.to_string_hum c)
          (Piece.Kind.to_string_hum k1)
          (Piece.Kind.to_string_hum k2)
      | Invalid_number_of_pawns (c, n) ->
        sprintf "Invalid number of %s pawns (%d)"
          (Piece.Color.to_string_hum c) n
      | Pawns_in_back_rank c ->
        sprintf "Player %s has pawns in the back rank" @@
        Piece.Color.to_string_hum c
      | Missing_pawn_en_passant c ->
        sprintf "Missing %s pawn in front of en passant square" @@
        Piece.Color.to_string_hum c
      | Invalid_en_passant_square sq ->
        sprintf "Invalid en passant square %s" @@
        Square.to_string sq
      | Invalid_extra_pieces (c, n) ->
        sprintf "Invalid number of extra %s pieces (%d)"
          (Piece.Color.to_string_hum c) n
      | Invalid_number_of_pieces (c, n) ->
        sprintf "Invalid number of %s pieces (%d)"
          (Piece.Color.to_string_hum c) n
      | Invalid_castling_rights (c, k) ->
        sprintf "Invalid castling rights, %s %s moved"
          (Piece.Color.to_string_hum c)
          (Piece.Kind.to_string_hum k)
      | En_passant_wrong_halfmove ->
        sprintf "En passant square is set, but halfmove clock is not zero"
      | Invalid_halfmove -> sprintf "Invalid halfmove clock"
      | Invalid_fullmove -> sprintf "Invalid fullmove clock"
  end

  type error = Error.t

  module E = Monad.Result.Make(Error)(Monad.Ident)

  open E.Syntax

  let (>>) m n = m >>= fun _ -> n

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
      check_count pos >>
      check_sep pos
  end

  module Checks = struct
    let check_inactive_in_check pos =
      let b = enemy_board pos in
      let attacks = Attacks.all pos pos.active ~ignore_same:true in
      if Bb.((b & pos.king & attacks) <> empty)
      then E.fail @@ Inactive_in_check (enemy pos)
      else E.return ()

    let check_checkers pos =
      let active_board = active_board pos in
      let king_sq = Bb.(first_set_exn (active_board & pos.king)) in
      let enemy_board = enemy_board pos in
      let occupied = Bb.(active_board + enemy_board) in
      let checkers = checkers pos ~king_sq ~enemy_board ~occupied in
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
      check_inactive_in_check pos >>
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

    let check_en_passant pos = match pos.en_passant with
      | None -> E.return ()
      | Some ep ->
        let rank, file = Square.decomp ep in
        if rank = Square.Rank.three then
          let sq = Square.create_exn ~rank:(succ rank) ~file in
          match piece_at_square pos sq with
          | Some p when Piece.is_white p && Piece.is_pawn p -> E.return ()
          | _ -> E.fail @@ Missing_pawn_en_passant White
        else if rank = Square.Rank.six then
          let sq = Square.create_exn ~rank:(pred rank) ~file in
          match piece_at_square pos sq with
          | Some p when Piece.is_black p && Piece.is_pawn p -> E.return ()
          | _ -> E.fail @@ Missing_pawn_en_passant Black
        else E.fail @@ Invalid_en_passant_square ep

    let check_promotions pos c b =
      let num_pawn = Bb.(count (pos.pawn & b)) in
      let num_knight = Bb.(count (pos.knight & b)) in
      let num_bishop = Bb.(count (pos.bishop & b)) in
      let num_rook = Bb.(count (pos.rook & b)) in
      let num_queen = Bb.(count (pos.queen & b)) in
      let extra =
        max 0 (num_knight - 2) +
        max 0 (num_bishop - 2) +
        max 0 (num_rook   - 2) +
        max 0 (num_queen  - 1) in
      if extra > (8 - num_pawn) then E.fail @@ Invalid_extra_pieces (c, extra)
      else if extra <> 0 then
        let c' = Piece.Color.opposite c in
        let n = Bb.count @@ board_of_color pos c' in
        if n >= 16 then E.fail @@ Invalid_number_of_pieces (c', n)
        else E.return ()
      else E.return ()

    let go pos =
      check_count pos >>
      check_back_rank pos >>
      check_en_passant pos >>
      check_promotions pos White pos.white >>
      check_promotions pos Black pos.black
  end

  module Castling = struct
    let check_king_moved pos c b =
      if Castling_rights.mem pos.castle c `king ||
         Castling_rights.mem pos.castle c `queen
      then
        let sq = match c with
          | White -> Square.e1
          | Black -> Square.e8 in
        if Bb.(sq @ (b & pos.king)) then E.return ()
        else E.fail @@ Invalid_castling_rights (c, King)
      else E.return ()

    let check_rook_moved pos c b s sq =
      if Castling_rights.mem pos.castle c s then
        if Bb.(sq @ (b & pos.rook)) then E.return ()
        else E.fail @@ Invalid_castling_rights (c, Rook)
      else E.return ()

    let go pos =
      check_king_moved pos White pos.white >>
      check_king_moved pos Black pos.black >>
      check_rook_moved pos White pos.white `king Square.h1 >>
      check_rook_moved pos White pos.white `queen Square.a1 >>
      check_rook_moved pos Black pos.black `king Square.h8 >>
      check_rook_moved pos Black pos.black `queen Square.a8
  end

  module Half_and_fullmove = struct
    let check_en_passant pos = match pos.en_passant with
      | Some _ when pos.halfmove <> 0 -> E.fail En_passant_wrong_halfmove
      | _ -> E.return ()

    let check_both pos =
      if pos.halfmove >
         ((pos.fullmove - 1) * 2) + (Piece.Color.to_int pos.active)
      then E.fail Invalid_halfmove
      else if pos.halfmove < 0 then E.fail Invalid_halfmove
      else if pos.fullmove < 1 then E.fail Invalid_fullmove
      else E.return ()

    let go pos =
      check_en_passant pos >>
      check_both pos
  end

  let check pos =
    King.go pos >>
    Checks.go pos >>
    Pawn.go pos >>
    Castling.go pos >>
    Half_and_fullmove.go pos
end

(* FEN parsing/unparsing *)

module Fen = struct
  module Error = struct
    type t =
      | Invalid_number_of_ranks of int
      | Invalid_file_increment of int * Square.t
      | Rank_full of int
      | Invalid_piece_symbol of char * Square.t
      | Unspecified_squares of int * int
      | Invalid_active_color of string
      | Invalid_castling_rights of string
      | Invalid_en_passant of string
      | Invalid_halfmove of string
      | Invalid_fullmove of string
      | Invalid_position of Valid.error
      | Invalid_number_of_sections of int

    let to_string = function
      | Invalid_number_of_ranks n -> sprintf "Invalid number of ranks %d" n
      | Invalid_file_increment (n, sq) ->
        sprintf "Invalid file increment %d on square %s" n @@
        Square.to_string sq
      | Rank_full rank -> sprintf "Piece placement on full rank %d" (rank + 1)
      | Invalid_piece_symbol (sym, sq) ->
        sprintf "Invalid piece symbol '%c' placed at square %s" sym @@
        Square.to_string sq
      | Unspecified_squares (rank, n) ->
        sprintf "Rank %d has %d unspecified square(s)" (rank + 1) n
      | Invalid_active_color s -> sprintf "Invalid active color '%s'" s
      | Invalid_castling_rights s -> sprintf "Invalid castling rights '%s'" s
      | Invalid_en_passant s -> sprintf "Invalid en passant square '%s'" s
      | Invalid_halfmove s -> sprintf "Invalid halfmove clock '%s'" s
      | Invalid_fullmove s -> sprintf "Invalid fullmove clock '%s'" s
      | Invalid_position e ->
        sprintf "Invalid position; %s" @@ Valid.Error.to_string e
      | Invalid_number_of_sections n ->
        sprintf "Invalid number of sections %d" n
  end

  type error = Error.t

  module E = struct
    include Monad.Result.Make(Error)(Monad.Ident)

    module String = struct
      let fold =
        let rec loop s i acc ~f ~len =
          if i = len then return acc
          else
            f acc s.[i] >>= fun acc ->
            loop s (i + 1) acc ~f ~len in
        fun s ~init ~f -> loop s 0 init ~f ~len:(String.length s)
    end

    module List = struct
      include List

      let iteri =
        let rec loop i ~f = function
          | [] -> return ()
          | x :: xs ->
            f i x >>= fun () ->
            loop (i + 1) xs ~f in
        fun l ~f -> loop 0 l ~f
    end
  end

  open E.Syntax

  let start = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

  let string_of_placement pos =
    let rec aux rank file skip acc =
      if rank < 0 then acc
      else if file > 7 then
        let acc = if skip > 0 then acc ^ Int.to_string skip else acc in
        let acc = if rank > 0 then acc ^ "/" else acc in
        aux (rank - 1) 0 0 acc
      else match piece_at_square pos @@ Square.create_unsafe ~rank ~file with
        | None -> aux rank (file + 1) (skip + 1) acc
        | Some p ->
          let acc = if skip > 0 then acc ^ Int.to_string skip else acc in
          let acc = acc ^ String.of_char @@ Piece.to_fen p in
          aux rank (file + 1) 0 acc in
    aux 7 0 0 ""

  let string_of_active = function
    | Piece.White -> "w"
    | Piece.Black -> "b"

  let string_of_castle = Castling_rights.to_string
  let string_of_en_passant = Option.value_map ~default:"-" ~f:Square.to_string

  let to_string (pos : t) =
    sprintf "%s %s %s %s %d %d" (string_of_placement pos)
      (string_of_active pos.active)
      (string_of_castle pos.castle)
      (string_of_en_passant pos.en_passant)
      pos.halfmove pos.fullmove

  let parse_placement s =
    let color_tbl = Array.create Bb.empty ~len:Piece.Color.count in
    let kind_tbl = Array.create Bb.empty ~len:Piece.Kind.count in
    (* Split the ranks so we can parse them individually. *)
    begin match String.split s ~on:'/' with
      | [_; _; _; _; _; _; _; _] as ranks -> E.return @@ List.rev ranks
      | ranks -> E.fail @@ Invalid_number_of_ranks (List.length ranks)
    end >>= fun ranks ->
    (* The main entry to parsing the rank. *)
    let rec parse_rank rank file sym = match Char.get_digit sym with
      | Some inc -> skip_file rank file inc
      | None -> place_piece rank file sym
    (* Advance the file (we hit a numeric symbol). *)
    and skip_file rank file inc =
      let file' = file + inc in
      if file' > Square.File.count then E.fail @@
        Invalid_file_increment (inc, Square.create_exn ~rank ~file)
      else E.return file'
    (* Place a piece on the board (we hit an alphabetical symbol). *)
    and place_piece rank file sym =
      if file > Square.File.h then E.fail @@ Rank_full rank
      else
        let sq = Square.create_unsafe ~rank ~file in
        match Piece.of_fen sym with
        | Some p ->
          let c = Piece.(color p |> Color.to_int) in
          let k = Piece.(kind p |> Kind.to_int) in
          color_tbl.(c) <- Bb.(color_tbl.(c) ++ sq);
          kind_tbl.(k) <- Bb.(kind_tbl.(k) ++ sq);
          E.return (file + 1)
        | None -> E.fail @@ Invalid_piece_symbol (sym, sq) in
    (* Parse each rank individually. *)
    E.List.iteri ranks ~f:(fun rank s ->
        let init = Square.File.a and f = parse_rank rank in
        E.String.fold s ~init ~f >>= fun file ->
        let diff = Square.File.count - file in
        (* All eight squares of the rank must be specified. *)
        if diff <> 0 then E.fail @@ Unspecified_squares (rank, diff)
        else E.return ()) >>= fun () ->
    (* Return the individual bitboards. *)
    E.return @@ Piece.(
        color_tbl.(Color.white),
        color_tbl.(Color.black),
        kind_tbl.(Kind.pawn),
        kind_tbl.(Kind.knight),
        kind_tbl.(Kind.bishop),
        kind_tbl.(Kind.rook),
        kind_tbl.(Kind.queen),
        kind_tbl.(Kind.king))

  let parse_active = function
    | "w" -> E.return Piece.White
    | "b" -> E.return Piece.Black
    | s -> E.fail @@ Invalid_active_color s

  let parse_castle s = try E.return @@ Castling_rights.of_string_exn s with
    | _ -> E.fail @@ Invalid_castling_rights s

  let parse_en_passant = function
    | "-" -> E.return None
    | s -> try E.return @@ Some (Square.of_string_exn s) with
      | _ -> E.fail @@ Invalid_en_passant s

  let parse_halfmove s = try
      E.return @@ Int.of_string s
    with _ -> E.fail @@ Invalid_halfmove s

  let parse_fullmove s = try
      E.return @@ Int.of_string s
    with _ -> E.fail @@ Invalid_fullmove s

  let validate_and_map pos = Error.(
      Valid.check pos |>
      Result.map_error ~f:(fun e -> Invalid_position e)) >>= fun () ->
    E.return pos

  let of_string ?(validate = true) s = match String.split s ~on:' ' with
    | [placement; active; castle; en_passant; halfmove; fullmove] ->
      parse_placement placement >>=
      fun (white, black, pawn, knight, bishop, rook, queen, king) ->
      parse_active active >>= fun active ->
      parse_castle castle >>= fun castle ->
      parse_en_passant en_passant >>= fun en_passant ->
      parse_halfmove halfmove >>= fun halfmove ->
      parse_fullmove fullmove >>= fun fullmove ->
      let pos = Fields.create
          ~white ~black ~pawn ~knight ~bishop ~rook ~queen ~king
          ~active ~castle ~en_passant ~halfmove ~fullmove in
      if validate then validate_and_map pos else E.return pos
    | sections -> E.fail @@ Invalid_number_of_sections (List.length sections)

  let of_string_exn ?(validate = true) s = match of_string s ~validate with
    | Ok pos -> pos
    | Error err -> invalid_arg @@ sprintf
        "Failed to parse FEN string '%s': %s" s (Error.to_string err)
end

let start = Fen.(of_string_exn start)

(* P for Position. Since all the fields are mutable, this will act like a
   pseudo-state monad. *)
module P = Monad.Reader.Make(T)(Monad.Ident)

(* Handling moves *)

module Apply = struct
  open P.Syntax

  let[@inline] (>>) m n = m >>= fun _ -> n

  let[@inline] map_color c ~f = P.read () >>| fun pos -> match c with
    | Piece.White -> set_white pos @@ f @@ white pos
    | Piece.Black -> set_black pos @@ f @@ black pos

  let[@inline] map_kind k ~f = P.read () >>| fun pos -> match k with
    | Piece.Pawn -> set_pawn pos @@ f @@ pawn pos
    | Piece.Knight -> set_knight pos @@ f @@ knight pos
    | Piece.Bishop -> set_bishop pos @@ f @@ bishop pos
    | Piece.Rook -> set_rook pos @@ f @@ rook pos
    | Piece.Queen -> set_queen pos @@ f @@ queen pos
    | Piece.King -> set_king pos @@ f @@ king pos

  (* Helper for setting both the color and the kind fields of the board. *)
  let[@inline] map_piece p ~f = 
    let c, k = Piece.decomp p in
    map_color c ~f >> map_kind k ~f

  let[@inline] set_square p sq = map_piece p ~f:Bb.(fun b -> b ++ sq)
  let[@inline] clear_square p sq = map_piece p ~f:Bb.(fun b -> b -- sq)

  (* Assume that if `p` exists, then it occupies square `sq`. *)
  let[@inline] clear_square_capture p sq = match p with
    | None -> P.return None
    | Some p -> map_piece p ~f:Bb.(fun b -> b -- sq) >>
      P.return @@ Some (Piece.kind p, sq)

  (* The halfmove clock is reset after captures or pawn moves, and
     incremented otherwise. *)
  let[@inline] update_halfmove sq sq' = P.read () >>| fun pos ->
    let open Bb.Syntax in
    let is_pawn = sq @ pos.pawn in
    let is_capture =
      (sq' @ all_board pos) || (is_pawn && is_en_passant pos sq') in
    set_halfmove pos @@ if is_pawn || is_capture then 0 else succ pos.halfmove

  module CR = Castling_rights

  let clear_white_castling_rights = P.read () >>| fun pos ->
    set_castle pos @@ CR.(minus pos.castle white)

  let clear_black_castling_rights = P.read () >>| fun pos ->
    set_castle pos @@ CR.(minus pos.castle black)

  let white_kingside_castle =
    clear_square Piece.white_rook Square.h1 >>
    set_square Piece.white_rook Square.f1 >>
    clear_white_castling_rights

  let white_queenside_castle =
    clear_square Piece.white_rook Square.a1 >>
    set_square Piece.white_rook Square.d1 >>
    clear_white_castling_rights

  let black_kingside_castle =
    clear_square Piece.black_rook Square.h8 >>
    set_square Piece.black_rook Square.f8 >>
    clear_black_castling_rights

  let black_queenside_castle =
    clear_square Piece.black_rook Square.a8 >>
    set_square Piece.black_rook Square.d8 >>
    clear_black_castling_rights

  (* If we're castling our king on this move, then we need to move the rook
     as well as clear our rights. *)
  let[@inline] king_moved_or_castled sq sq' =
    P.read () >>= fun {active; _} -> match active with
    | Piece.White when Square.(sq = e1 && sq' = g1) -> white_kingside_castle
    | Piece.White when Square.(sq = e1 && sq' = c1) -> white_queenside_castle
    | Piece.Black when Square.(sq = e8 && sq' = g8) -> black_kingside_castle
    | Piece.Black when Square.(sq = e8 && sq' = c8) -> black_queenside_castle
    | Piece.White -> clear_white_castling_rights
    | Piece.Black -> clear_black_castling_rights

  (* If we're moving or capturing a rook, then clear the castling rights for
     that particular side. *)
  let[@inline] rook_moved_or_captured sq = function
    | Piece.White when Square.(sq = h1) -> P.read () >>| fun pos ->
      set_castle pos @@ CR.(minus pos.castle white_kingside)
    | Piece.White when Square.(sq = a1) -> P.read () >>| fun pos ->
      set_castle pos @@ CR.(minus pos.castle white_queenside)
    | Piece.Black when Square.(sq = h8) -> P.read () >>| fun pos ->
      set_castle pos @@ CR.(minus pos.castle black_kingside)
    | Piece.Black when Square.(sq = a8) -> P.read () >>| fun pos ->
      set_castle pos @@ CR.(minus pos.castle black_queenside)
    | _ -> P.return ()

  (* Rook moved from a square. *)
  let[@inline] rook_moved sq = P.read () >>= fun {active; _} ->
    rook_moved_or_captured sq active

  (* Rook was captured at a square. Assume that it is the enemy's color. *)
  let[@inline] rook_captured p' sq' = match p' with
    | Some p when Piece.is_rook p -> rook_moved_or_captured sq' @@ Piece.color p
    | _ -> P.return ()

  (* Handle castling-related details. *)
  let[@inline] update_castle p p' sq sq' = match Piece.kind p with
    | King -> king_moved_or_castled sq sq'
    | Rook -> rook_moved sq >> rook_captured p' sq' 
    | _ -> rook_captured p' sq'

  (* Update the en passant square if a pawn double push occurred. We're
     skipping the check on whether the file changed, since our assumption is
     that the move is legal. For the check if `p` is a pawn or not, we assume
     that it belongs to the active color. *) 
  let[@inline] update_en_passant p sq sq' = begin
    if Piece.is_pawn p then
      let rank = Square.rank sq and rank', file = Square.decomp sq' in
      P.read () >>| fun {active; _} -> match active with
      | Piece.White when Square.Rank.(rank = two && rank' = four) ->
        Some (Square.create_unsafe ~rank:(pred rank') ~file)
      | Piece.Black when Square.Rank.(rank = seven && rank' = five) ->
        Some (Square.create_unsafe ~rank:(succ rank') ~file)
      | _ -> None
    else P.return None end >>= fun ep ->
    P.read () >>| (Fn.flip set_en_passant @@ ep)

  (* After each halfmove, give the turn to the other player. *)
  let flip_active = P.read () >>| fun pos ->
    set_active pos @@ Piece.Color.opposite pos.active

  (* Since white moves first, increment the fullmove clock after black
     has moved. *)
  let update_fullmove = P.read () >>| fun pos -> match pos.active with
    | Black -> set_fullmove pos @@ succ pos.fullmove
    | White -> ()

  (* Update the piece for the destination square if we're promoting. *)
  let[@inline] do_promote p = function
    | Some k -> P.read () >>| fun {active; _} -> Piece.create active k
    | None -> P.return p

  let[@inline] move_with_en_passant p sq' ep =
    set_square p sq' >>
    (* Check if this was an en passant capture. *)
    let open Piece in
    if Option.exists ep ~f:(Square.equal sq') && is_pawn p then
      let rank, file = Square.decomp sq' in
      begin P.read () >>| fun {active; _} -> match active with
        | White -> Square.create_unsafe ~rank:(rank - 1) ~file, black_pawn
        | Black -> Square.create_unsafe ~rank:(rank + 1) ~file, white_pawn
      end >>= fun (sq, p) -> clear_square p sq >> P.return @@ Some (Pawn, sq)
    else P.return None

  (* Perform a halfmove `m` for piece `p`. Assume it has already been checked
     for legality. *)
  let[@inline] move p p' m =
    Move.decomp m |> fun (sq, sq', promote) ->
    P.read () >>= fun {en_passant = ep; _} ->
    (* Do the stuff that relies on the initial state. *)
    update_halfmove sq sq' >>
    update_en_passant p sq sq' >>
    update_castle p p' sq sq' >>
    (* Move the piece. *)
    clear_square p sq >> clear_square_capture p' sq' >>= fun capture ->
    do_promote p promote >>= fun p ->
    move_with_en_passant p sq' ep >>= fun ep_capture ->
    (* Prepare for the next move. *)
    update_fullmove >> flip_active >>
    (* Return the capture that was made, if any. *)
    P.return @@ Option.merge capture ep_capture ~f:(fun _ _ ->
        invalid_arg "Encountered direct and en passant capture in the same \
                     move")
end


(* Relevant info about the position for generating moves. *)
module Info = struct
  type t = {
    pos : T.t;
    king_sq : Square.t;
    occupied : Bb.t;
    active_board : Bb.t;
    enemy_board : Bb.t;
    enemy_attacks : Bb.t;
    pinners : Bb.t Map.M(Square).t;
    num_checkers : int;
    check_mask : Bb.t;
    en_passant_check_mask : Bb.t;
    enemy_sliders : (Square.t * Piece.kind) list;
  } [@@deriving fields]
end

(* I for Info *)
module I = Monad.Reader.Make(Info)(Monad.Ident)

module Legal = struct
  module T = struct
    type t = {
      move : Move.t;
      new_position : T.t;
      capture : (Piece.kind * Square.t) option;
      is_en_passant : bool;
    } [@@deriving compare, equal, sexp, fields]
  end

  include T
  include Comparable.Make(T)
end

type legal = Legal.t [@@deriving compare, equal, sexp]

module Moves = struct
  open I.Syntax

  module Pawn = struct
    let[@inline] push sq = I.read () >>| fun {pos; occupied; _} ->
      let open Bb.Syntax in
      Pre.pawn_advance sq pos.active - occupied

    let[@inline] push2 rank file = I.read () >>| fun {pos; occupied; _} ->
      let open Bb.Syntax in
      match pos.active with
      | Piece.White when Square.Rank.(rank = two) ->
        !!(Square.create_unsafe ~rank:Square.Rank.four ~file) - occupied
      | Piece.Black when Square.Rank.(rank = seven) ->
        !!(Square.create_unsafe ~rank:Square.Rank.five ~file) - occupied
      | _ -> Bb.empty

    (* Check if our pawn or the captured pawn are along a pin ray. If so,
       then this capture would be illegal, since it would lead to a discovery
       on the king. En passant moves arise rarely across all chess positions,
       so we can do a bit of heavy calculation here. *)
    let[@inline] en_passant sq ep diag = I.read () >>|
      fun {pos; king_sq; occupied; enemy_sliders; _} ->
      (* Get the position of the pawn which made a double advance. *)
      let pw =
        let rank, file = Square.decomp ep in
        match pos.active with
        | Piece.White -> Square.create_unsafe ~rank:(rank - 1) ~file
        | Piece.Black -> Square.create_unsafe ~rank:(rank + 1) ~file in
      (* Remove our pawn and the captured pawn from the board. *)
      let open Bb in
      let occupied = occupied -- sq -- pw in
      let init = diag ++ ep and finish = ident in
      (* Check if an appropriate diagonal attack from the king would reach
         that corresponding piece. *)
      List.fold_until enemy_sliders ~init ~finish ~f:(fun acc -> function
          | sq, Piece.Bishop when sq @ (Pre.bishop king_sq occupied) -> Stop diag
          | sq, Piece.Rook when sq @ (Pre.rook king_sq occupied) -> Stop diag
          | sq, Piece.Queen when sq @ (Pre.queen king_sq occupied) -> Stop diag
          | _ -> Continue acc)

    let[@inline] capture sq = I.read () >>= fun {pos; enemy_board; _} ->
      let open Bb.Syntax in
      let diag' = Pre.pawn_capture sq pos.active in
      let diag = diag' & enemy_board in
      match pos.en_passant with
      | Some ep when ep @ diag' -> en_passant sq ep diag
      | _ -> I.return diag

    let promote_kinds = Piece.[Knight; Bishop; Rook; Queen]

    (* We need to multiply the move by the number of pieces we can
       promote to. *)
    let[@inline] promote src dst = List.map promote_kinds ~f:(fun k ->
        Move.create src dst ~promote:(Some k))
  end

  module Knight = struct
    let[@inline] jump sq = I.read () >>| fun {active_board; _} ->
      Bb.(Pre.knight sq - active_board)
  end

  module Bishop = struct
    let[@inline] slide sq = I.read () >>| fun {occupied; active_board; _} ->
      Bb.(Pre.bishop sq occupied - active_board)
  end

  module Rook = struct
    let[@inline] slide sq = I.read () >>| fun {occupied; active_board; _} ->
      Bb.(Pre.rook sq occupied - active_board)
  end

  module Queen = struct
    let[@inline] slide sq = I.read () >>| fun {occupied; active_board; _} ->
      Bb.(Pre.queen sq occupied - active_board)
  end

  module King = struct
    (* Note that `enemy_attacks` includes squares occupied by enemy pieces.
       Therefore, the king may not attack those squares. *)
    let[@inline] move sq =
      I.read () >>| fun {active_board; enemy_attacks; _} ->
      Bb.(Pre.king sq - active_board - enemy_attacks)

    let castle = I.read () >>|
      fun {pos; occupied; enemy_attacks; num_checkers; _} ->
      if num_checkers > 0 then Bb.empty
      else
        let open Bb.Syntax in
        let c sq s =
          let m, b = Pre.castle pos.castle pos.active s in
          (* Check the actual squares to move our pieces to. *)
          let ok = Bb.count (b - occupied - enemy_attacks) = 2 in
          let ok = match s with
            | `king -> ok
            | `queen ->
              (* For queenside, the extra b-file square needs to be
                 unoccupied, so check the mask. *)
              ok && Bb.count (m - occupied) = 3 in
          if ok then !!sq else Bb.empty in
        match pos.active with
        | Piece.White -> c Square.g1 `king + c Square.c1 `queen
        | Piece.Black -> c Square.g8 `king + c Square.c8 `queen
  end

  (* Use this mask to restrict the movement of pinned pieces. *)
  let[@inline] pin_mask sq = I.read () >>| fun {king_sq; pinners; _} ->
    match Map.find pinners sq with
    | None -> Bb.full
    | Some p ->
      if Bb.count p > 1 then Bb.empty
      else
        let sq' = Bb.first_set_exn p in
        let mask = Pre.between king_sq sq' in
        Bb.(mask ++ sq')

  (* Use this mask to restrict the movement of pieces when we are in check. *)
  let check_mask = I.read () >>| Info.check_mask

  (* Special case for pawns, with en passant capture being an option to escape
     check. *)
  let[@inline] check_mask_pawn capture = I.read () >>|
    fun {num_checkers; check_mask; en_passant_check_mask; _} ->
    if num_checkers <> 1 then check_mask
    else Bb.(check_mask + (capture & en_passant_check_mask))

  (* Pawn has special case for check mask. *)
  let[@inline] make_pawn sq b capture =
    pin_mask sq >>= fun pin ->
    check_mask_pawn capture >>| fun chk ->
    Bb.(b & pin & chk)

  (* All other pieces (except the king). *)
  let[@inline] make sq b =
    pin_mask sq >>= fun pin ->
    check_mask >>| fun chk ->
    Bb.(b & pin & chk)

  let[@inline] pawn sq =
    let open Pawn in
    let open Bb.Syntax in
    push sq >>= fun push -> begin
      (* Only allow double push if a single push is available. *)
      if Bb.(push = empty) then I.return push
      else
        let rank, file = Square.decomp sq in
        push2 rank file >>| (+) push
    end >>= fun push ->
    capture sq >>= fun capture ->
    make_pawn sq (push + capture) capture

  let[@inline] knight sq = Knight.jump sq >>= make sq 
  let[@inline] bishop sq = Bishop.slide sq >>= make sq
  let[@inline] rook sq = Rook.slide sq >>= make sq 
  let[@inline] queen sq = Queen.slide sq >>= make sq

  let[@inline] king sq =
    let open King in
    let open Bb.Syntax in
    move sq >>= fun move ->
    castle >>| fun castle ->
    move + castle

  (* Create a copy of the current position which can then be freely
     mutated. Then, apply the move to this position. *)
  let[@inline] make_move pos p acc move =
    let new_position = copy pos in
    let dst = Move.dst move in
    let p' = piece_at_square pos dst in
    let capture = Monad.Reader.run (Apply.move p p' move) new_position in
    let is_en_passant = Option.exists pos.en_passant ~f:(Square.equal dst) in
    Legal.Fields.create ~move ~new_position ~capture ~is_en_passant :: acc
  
  (* Get the new positions from the bitboard of squares we can move to. *)
  let[@inline] exec src k init b = I.read () >>| fun {pos; _} ->
    let is_promote = match k with
      | Piece.Pawn -> begin
          (* If we're promoting, then the back rank should be the only
             available squares. *)
          match pos.active with
          | Piece.White -> Bb.((b & rank_8) = b)
          | Piece.Black -> Bb.((b & rank_1) = b)
        end
      | _ -> false in
    let f = make_move pos (Piece.create pos.active k) in
    if is_promote then Bb.fold b ~init ~f:(fun init dst ->
        Pawn.promote src dst |> List.fold ~init ~f)
    else Bb.fold b ~init ~f:(fun acc dst -> Move.create src dst |> f acc)

  let[@inline] any sq = function
    | Piece.Pawn -> pawn sq
    | Piece.Knight -> knight sq
    | Piece.Bishop -> bishop sq
    | Piece.Rook -> rook sq
    | Piece.Queen -> queen sq
    | Piece.King -> king sq

  let go = I.read () >>= fun {pos; king_sq; num_checkers; _} ->
    (* If the king has more than one attacker, then it is the only piece
       we can move. *)
    if num_checkers > 1 then king king_sq >>= exec king_sq King []
    else find_active pos |> I.List.fold ~init:[] ~f:(fun acc (sq, k) ->
        any sq k >>= exec sq k acc)
end

(* For each enemy sliding piece, calculate its attack set. Then,
   intersect it with the same attack set from our king's square.
   Then, intersect with the squares between the sliding piece and our
   king. Any of our pieces that are in this intersection are thus
   pinned. *)
let[@inline] pinners ~active_board ~king_sq ~enemy_sliders ~occupied =
  let open Bb.Syntax in
  let update pinners sq p k mask =
    match Bb.first_set (p & k & mask) with
    | None -> pinners
    | Some sq' -> Map.update pinners sq' ~f:(function
        | Some b -> b ++ sq
        | None -> !!sq)
  and mask = active_board -- king_sq
  and init = Map.empty (module Square) in
  List.fold enemy_sliders ~init ~f:(fun pinners (sq, k) ->
      let mask = mask & Pre.between king_sq sq in
      match k with
      | Piece.Bishop ->
        let b, k = Pre.(bishop sq occupied, bishop king_sq occupied) in
        update pinners sq b k mask
      | Piece.Rook ->
        let r, k = Pre.(rook sq occupied, rook king_sq occupied) in
        update pinners sq r k mask
      | Piece.Queen ->
        let q, k = Pre.(queen sq occupied, queen king_sq occupied) in
        update pinners sq q k mask
      | _ -> pinners) 

(* Generate the masks which may restrict movement in the event of a check. *)
let[@inline] check_masks pos ~num_checkers ~checkers ~king_sq ~enemy =
  if num_checkers <> 1
  then Bb.full, Bb.empty
  else
    (* Test if the checker is a sliding piece. If so, then we can try to
       block the attack. Otherwise, they may only be captured. *)
    let open Bb.Syntax in
    let sq = Bb.first_set_exn checkers in
    match which_kind_exn pos sq with
    | Bishop | Rook | Queen ->
      checkers + Pre.between king_sq sq, Bb.empty
    | Pawn ->
      (* Edge case for being able to get out of check via en passant
         capture. *)
      let rank, file = Square.decomp sq in
      let ep = match (enemy : Piece.color) with
        | White -> Square.create ~rank:(pred rank) ~file
        | Black -> Square.create ~rank:(succ rank) ~file in
      checkers, Option.value_map ep ~default:Bb.empty ~f:(fun ep ->
          if Option.exists pos.en_passant ~f:(Square.equal ep)
          then !!ep else Bb.empty)
    |  _ -> checkers, Bb.empty

(* Populate info needed for generating legal moves. *)
let[@inline] create_info pos =
  (* First, find our king. *)
  let king_sq = Bb.(first_set_exn (pos.king & active_board pos)) in
  (* Most general info. *)
  let enemy = Piece.Color.opposite pos.active in
  let occupied = all_board pos in
  let active_board = active_board pos in
  let enemy_board = enemy_board pos in
  (* We're considering attacked squares only for king moves. These squares
     should include enemy pieces which may block an enemy attack, since it
     would be illegal for the king to attack those squares. *)
  let enemy_attacks =
    Attacks.all pos enemy ~ignore_same:false ~king_danger:true in
  let enemy_pieces = find_color pos enemy in
  let enemy_sliders =
    List.filter enemy_pieces ~f:(fun (_, k) -> Piece.Kind.is_sliding k) in
  let pinners = pinners ~active_board ~king_sq ~enemy_sliders ~occupied in
  let checkers = checkers pos ~king_sq ~enemy_board ~occupied in
  (* Number of checkers is important for how we can decide to get out of
     check. *)
  let num_checkers = Bb.count checkers in
  let check_mask, en_passant_check_mask =
    check_masks pos ~num_checkers ~checkers ~king_sq ~enemy in
  Info.Fields.create
    ~pos ~king_sq ~occupied ~active_board ~enemy_board
    ~enemy_attacks  ~pinners ~num_checkers ~check_mask
    ~en_passant_check_mask ~enemy_sliders

(* Generate all legal moves from the position. *)
let legal_moves pos =
  create_info pos |> Monad.Reader.run Moves.go

include Comparable.Make(T)
