open Core_kernel
open Monads.Std

(* To avoid allocation, we use the unboxed options. *)
module Uopt = struct
  include Uopt

  (* Required for `@@deriving compare, equal, sexp` *)

  let compare cmp x y = Option.compare cmp (to_option x) (to_option y)
  let equal eq x y = Option.equal eq (to_option x) (to_option y)
  let sexp_of_t f x = Option.sexp_of_t f @@ to_option x
  let t_of_sexp f x = of_option @@ Option.t_of_sexp f x

  (* Convenience functions. *)

  let[@inline] map x ~f = if is_none x then x else some @@ f @@ unsafe_value x

  let[@inline] value_map x ~default ~f =
    if is_none x then default else f @@ unsafe_value x
end

module Pre = Precalculated
module Bb = Bitboard
module Cr = Castling_rights

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
    mutable castle : Cr.t;
    mutable en_passant : Square.t Uopt.t;
    mutable halfmove : int;
    mutable fullmove : int;
    mutable hash : int64;
  } [@@deriving compare, equal, fields, sexp]

  let[@inline] en_passant pos = Uopt.to_option pos.en_passant

  (* We make an explicit copy because our move generator will return
     a new position (thus adhering to a functional style). *)
  let[@inline] copy pos = {
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
    hash       = pos.hash;
  }
end

include T

let[@inline] inactive pos = Piece.Color.opposite pos.active

(* Bitboard accessors *)

let[@inline] all_board pos = Bb.(pos.white + pos.black)

let[@inline] board_of_color pos = function
  | Piece.White -> pos.white
  | Piece.Black -> pos.black

let[@inline] active_board pos = board_of_color pos pos.active
let[@inline] inactive_board pos = board_of_color pos @@ inactive pos

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

let[@inline] is_en_passant pos sq =
  Uopt.is_some pos.en_passant &&
  Square.(sq = Uopt.unsafe_value pos.en_passant) 

let[@inline] en_passant_pawn_aux active ep = match active with
  | Piece.White -> Square.(with_rank_unsafe ep Rank.five)
  | Piece.Black -> Square.(with_rank_unsafe ep Rank.four)

let[@inline] en_passant_pawn_uopt pos =
  Uopt.map pos.en_passant ~f:(en_passant_pawn_aux pos.active)

let[@inline] en_passant_pawn pos = Uopt.to_option @@ en_passant_pawn_uopt pos

(* Piece lookup *)

let which_color pos sq =
  let open Bb.Syntax in
  if sq @ pos.white then Uopt.some Piece.White
  else if sq @ pos.black then Uopt.some Piece.Black
  else Uopt.none

let which_color_exn pos sq =
  let open Bb.Syntax in
  if sq @ pos.white then Piece.White
  else if sq @ pos.black then Piece.Black
  else invalid_argf "No piece exists at square %s" (Square.to_string sq) ()

let which_kind pos sq =
  let open Bb.Syntax in
  if sq @ pos.pawn then Uopt.some Piece.Pawn
  else if sq @ pos.knight then Uopt.some Piece.Knight
  else if sq @ pos.bishop then Uopt.some Piece.Bishop
  else if sq @ pos.rook then Uopt.some Piece.Rook
  else if sq @ pos.queen then Uopt.some Piece.Queen
  else if sq @ pos.king then Uopt.some Piece.King
  else Uopt.none

let which_kind_exn pos sq =
  let open Bb.Syntax in
  if sq @ pos.pawn then Piece.Pawn
  else if sq @ pos.knight then Piece.Knight
  else if sq @ pos.bishop then Piece.Bishop
  else if sq @ pos.rook then Piece.Rook
  else if sq @ pos.queen then Piece.Queen
  else if sq @ pos.king then Piece.King
  else invalid_argf "No piece exists at square %s" (Square.to_string sq) ()

let collect_color pos c =
  board_of_color pos c |> Bb.fold ~init:[] ~f:(fun acc sq ->
      (sq, which_kind_exn pos sq) :: acc)

let collect_active pos = collect_color pos pos.active
let collect_inactive pos = collect_color pos @@ inactive pos

let collect_kind pos k =
  board_of_kind pos k |> Bb.fold ~init:[] ~f:(fun acc sq ->
      (sq, which_color_exn pos sq) :: acc)

let collect_piece pos p =
  board_of_piece pos p |> Bb.fold ~init:[] ~f:(fun acc sq -> sq :: acc)

let piece_at_square_uopt pos sq =
  let c = which_color pos sq in
  if Uopt.is_none c then Uopt.none
  else
    let c = Uopt.unsafe_value c in
    let k = which_kind pos sq in
    if Uopt.is_none k then
      failwithf "Square %s is set for color %s, but no piece kind is available"
        (Square.to_string sq) (Piece.Color.to_string_hum c) ()
    else
      let k = Uopt.unsafe_value k in
      Uopt.some @@ Piece.create c k

let piece_at_square pos sq = Uopt.to_option @@ piece_at_square_uopt pos sq

let piece_at_square_exn pos sq =
  Piece.create (which_color_exn pos sq) (which_kind_exn pos sq)

let collect_all pos =
  all_board pos |> Bb.fold ~init:[] ~f:(fun acc sq ->
      let c = which_color_exn pos sq and k = which_kind_exn pos sq in
      (sq, Piece.create c k) :: acc)

(* Zobrist hashing *)

module Hash = struct
  (* Setup Zobrist keys. *)
  module Keys = struct
    (* This is the seed used in Stockfish. *)
    let seed = 1070372L

    let piece_keys =
      Array.create 0L
        ~len:(Piece.Color.count * Piece.Kind.count * Square.count)

    let[@inline] piece_key_idx c k sq = let open Piece in
      c + (k * Color.count) + (sq * Color.count * Kind.count)

    let[@inline] piece_key c k sq =
      Array.unsafe_get piece_keys @@ piece_key_idx c k sq

    let en_passant_keys = Array.create ~len:Square.File.count 0L

    let[@inline] en_passant_key file =
      Array.unsafe_get en_passant_keys file

    let castle_keys =
      Array.create ~len:(Piece.Color.count * Cr.Side.count) 0L

    let[@inline] castle_key_idx c s = c + s * Piece.Color.count

    let[@inline] castle_key c s =
      Array.unsafe_get castle_keys @@ castle_key_idx c s

    let white_to_move_key =
      let rng = Utils.Prng.create seed in
      (* Pieces *)
      for k = 0; to Piece.Kind.count - 1 do
        for sq = 0 to Square.count - 1 do
          piece_keys.(piece_key_idx Piece.Color.white k sq) <- rng#rand;
          piece_keys.(piece_key_idx Piece.Color.black k sq) <- rng#rand;
        done
      done;
      (* En passant file *)
      for file = 0 to Square.File.count - 1 do
        en_passant_keys.(file) <- rng#rand
      done;
      (* Castling rights *)
      for c = 0 to Piece.Color.count - 1 do
        for s = 0 to Cr.Side.count - 1 do
          castle_keys.(castle_key_idx c s) <- rng#rand
        done
      done;
      (* White to move *)
      rng#rand
  end

  (* Update individual fields. *)
  module Update = struct
    (* Fields are updated by exclusive-OR. *)
    let[@inline] flip h x = Int64.(h lxor x)

    let[@inline] active_player h = flip h Keys.white_to_move_key

    let[@inline] piece c k sq h =
      let c = Piece.Color.to_int c in
      let k = Piece.Kind.to_int k in
      let sq = Square.to_int sq in
      flip h @@ Keys.piece_key c k sq

    let[@inline] en_passant_file file h = flip h @@ Keys.en_passant_key file

    let[@inline] en_passant ep h = Uopt.value_map ep ~default:h
        ~f:(fun ep -> en_passant_file (Square.file ep) h)

    let[@inline] castle c s h =
      let c = Piece.Color.to_int c in
      let s = Cr.Side.to_int s in
      flip h @@ Keys.castle_key c s

    let[@inline] castle_test cr c s h =
      if Cr.mem cr c s then castle c s h else h
  end

  (* Get the hash of a position. *)
  let of_position pos = Fn.flip Monad.State.exec 0L @@
    let module H = Monad.State.Make(Int64)(Monad.Ident) in
    let open H.Syntax in
    (* White to move. *)
    begin match pos.active with
      | White -> H.update @@ Update.active_player
      | Black -> H.return ()
    end >>= fun () ->
    (* Pieces. *)
    collect_all pos |> H.List.iter ~f:(fun (sq, p) ->
        let c, k = Piece.decomp p in
        H.update @@ Update.piece c k sq) >>= fun () ->
    (* En passant *)
    H.update @@ Update.en_passant pos.en_passant >>= fun () ->
    (* Castling rights *)
    H.update @@ Update.castle_test pos.castle White Kingside  >>= fun () ->
    H.update @@ Update.castle_test pos.castle White Queenside >>= fun () ->
    H.update @@ Update.castle_test pos.castle Black Kingside  >>= fun () ->
    H.update @@ Update.castle_test pos.castle Black Queenside 
end

let same_hash pos1 pos2 = Int64.(pos1.hash = pos2.hash)

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

  (* Get the occupied squares for the board. Kingside_danger` indicates that the
     king of the opposite color should be ignored, so that sliding attacks
     can "see through" the inactive king. This is useful when the king is blocking
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
    | Piece.Pawn   -> Pre.pawn_capture sq c
    | Piece.Knight -> Pre.knight sq
    | Piece.Bishop -> Pre.bishop sq occupied
    | Piece.Rook   -> Pre.rook sq occupied
    | Piece.Queen  -> Pre.queen sq occupied
    | Piece.King   -> Pre.king sq

  let[@inline] aux ?(ignore_same = true) ?(king_danger = false) pos c ~f =
    let open Bb.Syntax in
    let occupied = occupied pos c king_danger in
    collect_color pos c |> List.fold ~init:Bb.empty ~f:(fun acc (sq, k) ->
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
  let attacks = Attacks.all pos (inactive pos) ~ignore_same:true in
  Bb.((active_board & pos.king & attacks) <> empty)

(* Relevant info about the position for generating moves, as well as performing
   sanity checks. *)

module Analysis = struct
  module T = struct
    type t = {
      pos : T.t;
      king_sq : Square.t;
      en_passant_pawn : Square.t Uopt.t;
      occupied : Bb.t;
      active_board : Bb.t;
      inactive_board : Bb.t;
      inactive_attacks : Bb.t;
      pinners : Bb.t array;
      num_checkers : int;
      check_mask : Bb.t;
      en_passant_check_mask : Bb.t;
      inactive_sliders : (Square.t * Piece.kind) list;
    } [@@deriving fields]
  end

  (* Attacks of all piece kinds, starting from the king, intersected with
     the squares occupied by inactive pieces. *)
  let[@inline] checkers pos ~king_sq ~inactive_board ~occupied =
    let open Bb.Syntax in
    let p = Pre.pawn_capture king_sq pos.active & pos.pawn in
    let n = Pre.knight king_sq & pos.knight in
    let bishop = Pre.bishop king_sq occupied in
    let rook = Pre.rook king_sq occupied in
    let bq = bishop & (pos.bishop + pos.queen) in
    let rq = rook & (pos.rook + pos.queen) in
    let k = Pre.king king_sq & pos.king in
    (p + n + bq + rq + k) & inactive_board

  (* For each inactive sliding piece, calculate its attack set. Then,
     intersect it with the same attack set from our king's square.
     Then, intersect with the squares between the sliding piece and our
     king. Any of our pieces that are in this intersection are thus
     pinned. *)
  let[@inline] pinners ~active_board ~king_sq ~inactive_sliders ~occupied =
    let open Bb.Syntax in
    let bishop  = lazy (Pre.bishop king_sq occupied) in
    let rook    = lazy (Pre.rook   king_sq occupied) in
    let queen   = lazy (Pre.queen  king_sq occupied) in
    let mask    = active_board -- king_sq in
    let pinners = Array.create ~len:Square.count Bb.empty in
    List.iter inactive_sliders ~f:(fun (sq, k) ->
        let mask = mask & Pre.between king_sq sq in
        let checker, king = match k with
          | Piece.Bishop -> Pre.bishop sq occupied, Lazy.force bishop
          | Piece.Rook   -> Pre.rook   sq occupied, Lazy.force rook
          | Piece.Queen  -> Pre.queen  sq occupied, Lazy.force queen
          | _ -> Bb.(empty, empty) in
        Bb.first_set (checker & king & mask) |>
        Option.iter ~f:(fun pinner ->
            let i = Square.to_int pinner in
            let b = Array.unsafe_get pinners i ++ sq in
            Array.unsafe_set pinners i b));
    pinners

  (* Generate the masks which may restrict movement in the event of a check. *)
  let[@inline] checks pos ~en_passant_pawn:pw ~num_checkers ~checkers ~king_sq =
    if num_checkers <> 1 then Bb.(full, empty)
    else
      (* Test if the checker is a sliding piece. If so, then we can try to
         block the attack. Otherwise, they may only be captured. *)
      let open Bb.Syntax in
      let sq = Bb.first_set_exn checkers in
      match which_kind_exn pos sq with
      | Bishop | Rook | Queen -> checkers + Pre.between king_sq sq, Bb.empty
      | Pawn ->
        (* Edge case for being able to get out of check via en passant
           capture. *)
        if Uopt.is_none pw then checkers, Bb.empty
        else
          let ep, pw = Uopt.(unsafe_value pos.en_passant, unsafe_value pw) in
          if Square.(sq = pw) then checkers, !!ep
          else checkers, Bb.empty
      |  _ -> checkers, Bb.empty

  (* Populate info needed for generating legal moves. *)
  let[@inline] create pos =
    (* First, find our king. *)
    let king_sq = Bb.(first_set_exn (pos.king & active_board pos)) in
    (* Square of the en passant pawn. *)
    let en_passant_pawn = en_passant_pawn_uopt pos in
    (* Most general info. *)
    let inactive = inactive pos in
    let occupied = all_board pos in
    let active_board = active_board pos in
    let inactive_board = inactive_board pos in
    let inactive_pieces = collect_color pos inactive in
    (* We're considering attacked squares only for king moves. These squares
       should include inactive pieces which may block an inactive attack, since
       it would be illegal for the king to attack those squares. *)
    let inactive_attacks = let open Bb in 
      let occupied = occupied -- king_sq in
      List.fold inactive_pieces ~init:empty ~f:(fun acc (sq, k) ->
          acc + Attacks.pre_of_kind sq occupied inactive k) in
    (* Sliding pieces will be used to calculate pins. *)
    let inactive_sliders =
      List.filter inactive_pieces ~f:(fun (_, k) -> Piece.Kind.is_sliding k) in
    (* Pinned pieces. *)
    let pinners = pinners ~active_board ~king_sq ~inactive_sliders ~occupied in
    (* Pieces checking our king. *)
    let checkers = checkers pos ~king_sq ~inactive_board ~occupied in
    (* Number of checkers is important for how we can decide to get out of
       check. *)
    let num_checkers = Bb.count checkers in
    (* Masks which will may allow us to escape check. *)
    let check_mask, en_passant_check_mask =
      checks pos ~en_passant_pawn ~num_checkers ~checkers ~king_sq in
    (* Construct the analyzed position. *)
    T.Fields.create
      ~pos ~king_sq ~en_passant_pawn ~occupied ~active_board
      ~inactive_board ~inactive_attacks ~pinners ~num_checkers
      ~check_mask ~en_passant_check_mask ~inactive_sliders

  include T
end

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
        Format.asprintf "Invalid en passant square %a" Square.pp sq
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
      let b = inactive_board pos in
      let attacks = Attacks.all pos pos.active ~ignore_same:true in
      if Bb.((b & pos.king & attacks) <> empty)
      then E.fail @@ Inactive_in_check (inactive pos)
      else E.return ()

    let check_checkers pos =
      let active_board = active_board pos in
      let king_sq = Bb.(first_set_exn (active_board & pos.king)) in
      let inactive_board = inactive_board pos in
      let occupied = Bb.(active_board + inactive_board) in
      let checkers = Analysis.checkers pos ~king_sq ~inactive_board ~occupied in
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
      check_king_moved pos White pos.white >>
      check_king_moved pos Black pos.black >>
      check_rook_moved pos White pos.white Kingside  Square.h1 >>
      check_rook_moved pos White pos.white Queenside Square.a1 >>
      check_rook_moved pos Black pos.black Kingside  Square.h8 >>
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

  let emit_placement buf pos =
    let adds = Buffer.add_string buf and addc = Buffer.add_char buf in
    let rec aux rank file skip =
      if rank < 0 then ()
      else if file > 7 then begin
        if skip > 0 then adds @@ Int.to_string skip;
        if rank > 0 then addc '/';
        aux (rank - 1) 0 0
      end else
        let p = piece_at_square_uopt pos @@ Square.create_exn ~rank ~file in
        if Uopt.is_none p then aux rank (file + 1) (skip + 1)
        else
          let p = Uopt.unsafe_value p in
          if skip > 0 then adds @@ Int.to_string skip;
          addc @@ Piece.to_fen p;
          aux rank (file + 1) 0 in
    aux 7 0 0

  let emit_active buf = function
    | Piece.White -> Buffer.add_char buf 'w'
    | Piece.Black -> Buffer.add_char buf 'b'

  let emit_castle buf cr = Buffer.add_string buf @@ Cr.to_string cr

  let emit_en_passant buf ep =
    Buffer.add_string buf @@
    Uopt.value_map ep ~default:"-" ~f:Square.to_string

  let to_string pos =
    let buf = Buffer.create 100 in
    let sep () = Buffer.add_char buf ' ' in
    emit_placement buf pos; sep ();
    emit_active buf @@ active pos; sep ();
    emit_castle buf @@ castle pos; sep ();
    emit_en_passant buf @@ pos.en_passant; sep ();
    Buffer.add_string buf @@ Int.to_string @@ halfmove pos; sep ();
    Buffer.add_string buf @@ Int.to_string @@ fullmove pos;
    Buffer.contents buf

  let parse_placement s =
    (* Split the ranks so we can parse them individually. *)
    begin match String.split s ~on:'/' with
      | [_; _; _; _; _; _; _; _] as ranks -> E.return @@ List.rev ranks
      | ranks -> E.fail @@ Invalid_number_of_ranks (List.length ranks)
    end >>= fun ranks ->
    (* Tables for our bitboards. *)
    let color_tbl = Array.create Bb.empty ~len:Piece.Color.count in
    let kind_tbl = Array.create Bb.empty ~len:Piece.Kind.count in
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
        else E.return ()) >>| fun () ->
    (* Return the bitboard tables. *)
    color_tbl, kind_tbl

  let parse_active = function
    | "w" -> E.return Piece.White
    | "b" -> E.return Piece.Black
    | s -> E.fail @@ Invalid_active_color s

  let parse_castle s = try E.return @@ Cr.of_string_exn s with
    | _ -> E.fail @@ Invalid_castling_rights s

  let parse_en_passant = function
    | "-" -> E.return Uopt.none
    | s -> try E.return @@ Uopt.some @@ Square.of_string_exn s with
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
      parse_placement placement >>= fun (color_tbl, kind_tbl) ->
      let white  = color_tbl.(Piece.Color.white) in
      let black  = color_tbl.(Piece.Color.black) in
      let pawn   =  kind_tbl.(Piece.Kind.pawn)   in
      let knight =  kind_tbl.(Piece.Kind.knight) in
      let bishop =  kind_tbl.(Piece.Kind.bishop) in
      let rook   =  kind_tbl.(Piece.Kind.rook)   in
      let queen  =  kind_tbl.(Piece.Kind.queen)  in
      let king   =  kind_tbl.(Piece.Kind.king)   in
      parse_active active >>= fun active ->
      parse_castle castle >>= fun castle ->
      parse_en_passant en_passant >>= fun en_passant ->
      parse_halfmove halfmove >>= fun halfmove ->
      parse_fullmove fullmove >>= fun fullmove ->
      let pos = Fields.create
          ~white ~black ~pawn ~knight ~bishop ~rook ~queen ~king
          ~active ~castle ~en_passant ~halfmove ~fullmove ~hash:0L in
      set_hash pos @@ Hash.of_position pos;      
      if validate then validate_and_map pos else E.return pos
    | sections -> E.fail @@ Invalid_number_of_sections (List.length sections)

  let of_string_exn ?(validate = true) s =
    of_string s ~validate |> Result.map_error ~f:Error.to_string |> function
    | Error e -> invalid_argf "Failed to parse FEN string '%s': %s" s e ()
    | Ok pos -> pos
end

let pp ppf pos = Format.fprintf ppf "%s" @@ Fen.to_string pos

let start = Fen.(of_string_exn start)

(* Handling moves

   NOTE: We originally used a state monad for the Makemove module, and a reader
   monad for the Moves module. For performance reasons, we then switched to a
   reader monad for Makemove, with mutable fields in the position datatype.
   This pattern acted as a "pseudo" state monad. However, we've gotten rid of
   the monadic code entirely since the compiler had trouble with inlining and
   specialization, as there were a lot of anonymous functions left in the
   compiled code. While the monadic style was much more concise and elegant,
   our priority for this code weighs far more in favor of performance.
*)

module Makemove = struct
  (* `en_passant_pawn` is the pawn that is "in front" of the en passant
     square. This field is only valid if the move we are making is in fact an
     en passant move.

     `castle` is the side on which castling occurred, if any.

     `piece` is the piece being moved (which belongs to the active color).

     `direct_capture` is the inactive piece, if any, that will be captured
     by the "direct" move (e.g. it is not an en passant capture).
  *)
  type context = {
    en_passant_pawn : Square.t Uopt.t;
    castle_side : Cr.side Uopt.t;
    piece : Piece.t;
    direct_capture : Piece.t Uopt.t;
  } [@@deriving fields]

  let[@inline] update_hash pos ~f = set_hash pos @@ f pos.hash

  let[@inline] map_color c pos ~f = match c with
    | Piece.White -> set_white pos @@ f @@ white pos
    | Piece.Black -> set_black pos @@ f @@ black pos

  let[@inline] map_kind k pos ~f = match k with
    | Piece.Pawn   -> set_pawn pos   @@ f @@ pawn pos
    | Piece.Knight -> set_knight pos @@ f @@ knight pos
    | Piece.Bishop -> set_bishop pos @@ f @@ bishop pos
    | Piece.Rook   -> set_rook pos   @@ f @@ rook pos
    | Piece.Queen  -> set_queen pos  @@ f @@ queen pos
    | Piece.King   -> set_king pos   @@ f @@ king pos

  (* Helper for setting both the color and the kind fields of the board. *)
  let[@inline] map_piece p sq pos ~f = 
    let c, k = Piece.decomp p in
    map_color c pos ~f;
    map_kind k pos ~f;
    update_hash pos ~f:(Hash.Update.piece c k sq);
    k

  let[@inline] set sq = Bb.(fun b -> b ++ sq)
  let[@inline] clr sq = Bb.(fun b -> b -- sq)

  let[@inline] set_square p sq pos = map_piece p sq pos ~f:(set sq) |> ignore
  let[@inline] clear_square p sq pos = map_piece p sq pos ~f:(clr sq) |> ignore

  (* Assume that if `p` exists, then it occupies square `sq`. *)
  let[@inline] clear_square_capture sq direct_capture pos =
    if Uopt.is_some direct_capture then
      let p = Uopt.unsafe_value direct_capture in
      map_piece p sq pos ~f:(clr sq) |> Uopt.some
    else Uopt.none

  (* The halfmove clock is reset after captures and pawn moves, and incremented
     otherwise. *)
  let[@inline] update_halfmove ctx pos = set_halfmove pos @@
    if Uopt.is_some ctx.en_passant_pawn
    || Uopt.is_some ctx.direct_capture
    || Piece.is_pawn ctx.piece
    then 0 else succ pos.halfmove

  (* Castling rights change monotonically, so the only time we update the hash
     is when we take away rights. *)
  let[@inline] castle_hash c s pos =
    if Cr.mem pos.castle c s then update_hash pos ~f:(Hash.Update.castle c s)

  let[@inline] clear_white_castling_rights pos =
    castle_hash White Kingside pos;
    castle_hash White Queenside pos;
    set_castle pos @@ Cr.(minus pos.castle white)

  let[@inline] clear_black_castling_rights pos =
    castle_hash Black Kingside pos;
    castle_hash Black Queenside pos;
    set_castle pos @@ Cr.(minus pos.castle black)

  let[@inline] white_kingside_castle pos =
    clear_square Piece.white_rook Square.h1 pos;
    set_square Piece.white_rook Square.f1 pos;
    clear_white_castling_rights pos

  let[@inline] white_queenside_castle pos =
    clear_square Piece.white_rook Square.a1 pos;
    set_square Piece.white_rook Square.d1 pos;
    clear_white_castling_rights pos

  let[@inline] black_kingside_castle pos =
    clear_square Piece.black_rook Square.h8 pos;
    set_square Piece.black_rook Square.f8 pos;
    clear_black_castling_rights pos

  let[@inline] black_queenside_castle pos =
    clear_square Piece.black_rook Square.a8 pos;
    set_square Piece.black_rook Square.d8 pos;
    clear_black_castling_rights pos

  (* If we're castling our king on this move, then we need to move the rook as
     well as clear our rights. *)
  let[@inline] king_moved_or_castled castle pos =
    if Uopt.is_some castle then match pos.active, Uopt.unsafe_value castle with
      | Piece.White, Cr.Kingside  -> white_kingside_castle pos
      | Piece.White, Cr.Queenside -> white_queenside_castle pos
      | Piece.Black, Cr.Kingside  -> black_kingside_castle pos
      | Piece.Black, Cr.Queenside -> black_queenside_castle pos
    else match pos.active with
      | Piece.White -> clear_white_castling_rights pos
      | Piece.Black -> clear_black_castling_rights pos

  (* If we're moving or capturing a rook, then clear the castling rights for
     that particular side. *)
  let[@inline] rook_moved_or_captured sq c pos = match c with
    | Piece.White when Square.(sq = h1) ->
      castle_hash White Kingside pos;
      set_castle pos @@ Cr.(minus pos.castle white_kingside)
    | Piece.White when Square.(sq = a1) ->
      castle_hash White Queenside pos;
      set_castle pos @@ Cr.(minus pos.castle white_queenside)
    | Piece.Black when Square.(sq = h8) ->
      castle_hash Black Kingside pos;
      set_castle pos @@ Cr.(minus pos.castle black_kingside)
    | Piece.Black when Square.(sq = a8) ->
      castle_hash Black Queenside pos;
      set_castle pos @@ Cr.(minus pos.castle black_queenside)
    | _ -> ()

  (* Rook moved from a square. *)
  let[@inline] rook_moved src pos =
    rook_moved_or_captured src pos.active pos

  (* Rook was captured at a square. Assume that it is the inactive's color. *)
  let[@inline] rook_captured dst direct_capture pos =
    if Uopt.is_some direct_capture then
      let p = Uopt.unsafe_value direct_capture in
      if Piece.is_rook p then rook_moved_or_captured dst (Piece.color p) pos

  (* Handle castling-related details. *)
  let[@inline] update_castle ctx src dst pos = match Piece.kind ctx.piece with
    | Rook -> rook_moved src pos; rook_captured dst ctx.direct_capture pos
    | King -> king_moved_or_castled ctx.castle_side pos
    | _ -> rook_captured dst ctx.direct_capture pos

  (* Update the en passant square if a pawn double push occurred. *) 
  let[@inline] update_en_passant p src dst pos =
    set_en_passant pos @@ if Piece.is_pawn p then
      let rank, file = Square.decomp src in
      let rank' = Square.rank dst in
      match pos.active with
      | Piece.White when rank' - rank = 2 ->
        update_hash pos ~f:(Hash.Update.en_passant_file file);
        Uopt.some Square.(with_rank_unsafe dst Rank.three)
      | Piece.Black when rank - rank' = 2 ->
        update_hash pos ~f:(Hash.Update.en_passant_file file);
        Uopt.some Square.(with_rank_unsafe dst Rank.six)
      | _ -> Uopt.none
    else Uopt.none

  (* After each halfmove, give the turn to the other player. *)
  let[@inline] flip_active pos =
    update_hash pos ~f:Hash.Update.active_player;
    set_active pos @@ inactive pos

  (* Since white moves first, increment the fullmove clock after black
     has moved. *)
  let[@inline] update_fullmove pos = match pos.active with
    | Black -> set_fullmove pos @@ succ pos.fullmove
    | White -> ()

  (* Update the piece for the destination square if we're promoting. *)
  let[@inline] do_promote p k pos = match k with
    | Some k -> Piece.create pos.active k
    | None -> p

  (* Clear the square of the pawn captured by en passant. *)
  let[@inline] en_passant_capture pw pos =
    if Uopt.is_some pw then
      let sq = Uopt.unsafe_value pw in
      let p = Piece.create (inactive pos) Pawn in
      clear_square p sq pos;
      Uopt.some Piece.Pawn
    else Uopt.none

  let[@inline] go src dst promote ctx pos =
    (* Do the stuff that relies on the initial state. *)
    update_hash pos ~f:(Hash.Update.en_passant pos.en_passant);
    update_halfmove ctx pos;
    update_en_passant ctx.piece src dst pos;
    update_castle ctx src dst pos;
    (* Clear the old placement. *)
    clear_square ctx.piece src pos;
    let capture = clear_square_capture dst ctx.direct_capture pos in
    (* Set the new placement. *)
    let p = do_promote ctx.piece promote pos in
    set_square p dst pos;
    let capture = if Uopt.is_some capture then capture
      else en_passant_capture ctx.en_passant_pawn pos in
    (* Prepare for the next move. *)
    update_fullmove pos;
    flip_active pos;
    (* Return the capture that was made, if any. *)
    capture 
end

module Legal = struct
  module T = struct
    type t = {
      move : Move.t;
      new_position : T.t;
      capture : Piece.kind Uopt.t;
      is_en_passant : bool;
      castle_side : Cr.side Uopt.t;
    } [@@deriving compare, equal, sexp, fields]

    let capture legal = Uopt.to_option legal.capture
    let castle_side legal = Uopt.to_option legal.castle_side

    let capture_square legal =
      if Uopt.is_none legal.capture then None
      else Option.some @@
        let dst = Move.dst legal.move in
        if not legal.is_en_passant then dst
        else Fn.flip en_passant_pawn_aux dst @@ inactive legal.new_position
  end

  let best moves ~eval =
    let open Option.Monad_infix in
    List.filter_map moves ~f:(fun m -> eval m >>| fun score -> (m, score)) |>
    List.sort ~compare:(fun (_, a) (_, b) -> Int.compare b a) |>
    List.fold_until ~init:([], 0) ~finish:fst
      ~f:(fun (acc, score') (m, score) -> match acc with
          | [] -> Continue (m :: acc, score)
          | _ when score' > score -> Stop acc
          | _ -> Continue (m :: acc, score'))

  include T
  include Comparable.Make(T)
end

type legal = Legal.t [@@deriving compare, equal, sexp]

module Moves = struct
  open Analysis

  module Pawn = struct
    let[@inline] push sq {pos; occupied; _} =
      Bb.(Pre.pawn_advance sq pos.active - occupied)

    let[@inline] push2 rank file {pos; occupied; _} =
      let open Bb.Syntax in
      match pos.active with
      | Piece.White when Square.Rank.(rank = two) ->
        !!(Square.create_unsafe ~rank:Square.Rank.four ~file) - occupied
      | Piece.Black when Square.Rank.(rank = seven) ->
        !!(Square.create_unsafe ~rank:Square.Rank.five ~file) - occupied
      | _ -> Bb.empty

    (* Check if our pawn or the captured pawn is along a pin ray. If so,
       then this capture would be illegal, since it would lead to a discovery
       on the king. En passant moves arise rarely across all chess positions,
       so we can do a bit of heavy calculation here. *)
    let[@inline] en_passant sq ep pw diag
        {king_sq; occupied; inactive_sliders; _} = let open Bb.Syntax in
      (* Remove our pawn and the captured pawn from the board, but pretend that
         the en passant square is occupied. This covers the case where we can
         capture the pawn, but it may leave our pawn pinned. *)
      let occupied = occupied -- sq -- pw ++ ep in
      let init = diag ++ ep and finish = ident in
      (* Check if an appropriate diagonal attack from the king would reach
         that corresponding piece. *)
      let bishop = lazy (Pre.bishop king_sq occupied) in
      let rook   = lazy (Pre.rook   king_sq occupied) in
      let queen  = lazy (Pre.queen  king_sq occupied) in
      (List.fold_until [@specialised]) inactive_sliders ~init ~finish
        ~f:(fun acc -> function
            | sq, Piece.Bishop when sq @ (Lazy.force bishop) -> Stop diag
            | sq, Piece.Rook   when sq @ (Lazy.force rook)   -> Stop diag
            | sq, Piece.Queen  when sq @ (Lazy.force queen)  -> Stop diag
            | _ -> Continue acc)

    let[@inline] capture sq
        ({pos; en_passant_pawn = pw; inactive_board; _} as a) =
      let open Bb.Syntax in
      let capture = Pre.pawn_capture sq pos.active in
      let diag = capture & inactive_board in
      if Uopt.is_some pw then
        let ep, pw = Uopt.(unsafe_value pos.en_passant, unsafe_value pw) in
        if ep @ capture then en_passant sq ep pw diag a
        else diag
      else diag

    let promote_kinds = Piece.[Knight; Bishop; Rook; Queen]

    (* We need to multiply the move by the number of pieces we can
       promote to. *)
    let[@inline] promote src dst =
      List.map promote_kinds ~f:(Move.create_with_promote src dst)
  end

  module Knight = struct
    let[@inline] jump sq {active_board; _} = Bb.(Pre.knight sq - active_board)
  end

  module Bishop = struct
    let[@inline] slide sq {occupied; active_board; _} =
      Bb.(Pre.bishop sq occupied - active_board)
  end

  module Rook = struct
    let[@inline] slide sq {occupied; active_board; _} =
      Bb.(Pre.rook sq occupied - active_board)
  end

  module Queen = struct
    let[@inline] slide sq {occupied; active_board; _} =
      Bb.(Pre.queen sq occupied - active_board)
  end

  module King = struct
    (* Note that `inactive_attacks` includes squares occupied by inactive pieces.
       Therefore, the king may not attack those squares. *)
    let[@inline] move sq {active_board; inactive_attacks; _} =
      Bb.(Pre.king sq - (active_board + inactive_attacks))

    let castle {pos; occupied; inactive_attacks; num_checkers; _} =
      let open Bb in
      (* Cannot castle out of check. *)
      if Int.(num_checkers = 0) then
        let active = pos.active in
        let m = occupied + inactive_attacks in
        let[@inline] ks_castle sq =
          let b = Pre.castle pos.castle active Kingside in
          if Int.equal 2 @@ count (b - m) then !!sq else empty in
        let[@inline] qs_castle sq bsq =
          if not (bsq @ occupied) then
            let b = Pre.castle pos.castle active Queenside in
            if Int.equal 2 @@ count (b - m) then !!sq else empty
          else empty in
        let ks, qs, bsq = match active with
          | Piece.White -> Square.(g1, c1, b1)
          | Piece.Black -> Square.(g8, c8, b8) in
        ks_castle ks + qs_castle qs bsq
      else empty
  end

  (* Use this mask to restrict the movement of pinned pieces. *)
  let[@inline] pin_mask sq {king_sq; pinners; _} = let open Bb in
    let p = Array.unsafe_get pinners @@ Square.to_int sq in
    match count p with
    | 1 ->
      let pinner = first_set_exn p in
      Pre.between king_sq pinner ++ pinner
    | 0 -> full
    | _ -> empty

  (* Special case for pawns, with en passant capture being an option to escape
     check. *)
  let[@inline] check_mask_pawn capture
      {num_checkers; check_mask; en_passant_check_mask; _} =
    if num_checkers <> 1 then check_mask
    else Bb.(check_mask + (capture & en_passant_check_mask))

  (* Pawn has special case for check mask. *)
  let[@inline] make_pawn sq b capture a =
    Bb.(b & pin_mask sq a & check_mask_pawn capture a)

  (* All other pieces (except the king). *)
  let[@inline] make sq a b = Bb.(b & pin_mask sq a & a.check_mask)

  let[@inline] pawn sq a =
    let open Pawn in
    let open Bb.Syntax in
    let push = push sq a in
    (* Only allow double push if a single push is available. *)
    let push = if Bb.(push <> empty) then
        let rank, file = Square.decomp sq in
        push + push2 rank file a
      else push in
    let capture = capture sq a in
    make_pawn sq (push + capture) capture a

  let[@inline] knight sq a = make sq a @@ Knight.jump  sq a
  let[@inline] bishop sq a = make sq a @@ Bishop.slide sq a
  let[@inline] rook   sq a = make sq a @@ Rook.slide   sq a
  let[@inline] queen  sq a = make sq a @@ Queen.slide  sq a

  let[@inline] king sq a =
    let open King in
    let open Bb.Syntax in
    move sq a + castle a

  let[@inline] castle_side piece src dst =
    if Piece.is_king piece then
      let file = Square.file src in
      if Square.File.(file = e) then
        let file' = Square.file dst in
        if Square.File.(file' = c) then Uopt.some Cr.Queenside
        else if Square.File.(file' = g) then Uopt.some Cr.Kingside
        else Uopt.none
      else Uopt.none
    else Uopt.none

  (* Actually runs the makemove routine and returns relevant info. *)
  let[@inline] make_move_aux pos src dst promote piece en_passant_pawn =
    let new_position = copy pos in
    let direct_capture = piece_at_square_uopt pos dst in
    let is_en_passant = Piece.is_pawn piece && is_en_passant pos dst in
    let en_passant_pawn =
      if is_en_passant then en_passant_pawn else Uopt.none in
    let castle_side = castle_side piece src dst in
    let ctx = Makemove.Fields_of_context.create
        ~en_passant_pawn ~piece ~castle_side ~direct_capture in
    let capture = Makemove.go src dst promote ctx new_position in
    new_position, capture, is_en_passant, castle_side

  (* Accumulator for making more than one move. *)
  let[@inline] accum_move pos en_passant_pawn p acc move =
    let src, dst, promote = Move.decomp move in
    let new_position, capture, is_en_passant, castle_side =
      make_move_aux pos src dst promote p en_passant_pawn in
    Legal.Fields.create
      ~move ~new_position ~capture ~is_en_passant ~castle_side :: acc

  (* If we're promoting, then the back rank should be the only
     available squares. *)
  let[@inline] is_promote_rank b = function
    | Piece.White -> Bb.((b & rank_8) = b)
    | Piece.Black -> Bb.((b & rank_1) = b)

  let[@inline] exec_promote src init b ~f =
    (Bb.fold [@specialised]) b ~init ~f:(fun init dst ->
        Pawn.promote src dst |> List.fold ~init ~f)

  let[@inline] exec_normal src init b ~f =
    (Bb.fold [@specialised]) b ~init
      ~f:(fun acc dst -> Move.create src dst |> f acc)

  (* Get the new positions from the bitboard of squares we can move to. *)
  let[@inline] exec src k init {pos; en_passant_pawn; _} b =
    let active = pos.active in
    let f = accum_move pos en_passant_pawn @@ Piece.create active k in
    match k with
    | Piece.Pawn when is_promote_rank b active -> exec_promote src init b ~f
    | _ -> exec_normal src init b ~f

  let[@inline] any sq = function
    | Piece.Pawn   -> pawn sq
    | Piece.Knight -> knight sq
    | Piece.Bishop -> bishop sq
    | Piece.Rook   -> rook sq
    | Piece.Queen  -> queen sq
    | Piece.King   -> king sq

  let[@inline] go ({pos; king_sq; num_checkers; _} as a) =
    (* If the king has more than one attacker, then it is the only piece
       we can move. *)
    if num_checkers > 1 then king king_sq a |> exec king_sq King [] a
    else collect_active pos |>
         (List.fold [@specialised]) ~init:[] ~f:(fun acc (sq, k) ->
             any sq k a |> exec sq k acc a)
end

let make_move ?(validate = false) pos move =
  let src, dst, promote = Move.decomp move in
  let p = piece_at_square_exn pos src in
  let en_passant_pawn = en_passant_pawn_uopt pos in
  let new_position, _, _, _ =
    Moves.make_move_aux pos src dst promote p en_passant_pawn in
  if validate then match Valid.check new_position with
    | Error e -> invalid_argf "Invalid move: %s" (Valid.Error.to_string e) ()
    | Ok () -> new_position
  else new_position    

let legal_moves pos = Analysis.create pos |> Moves.go

include Comparable.Make(T)
