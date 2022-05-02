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

  let[@inline] bind x ~f = if is_none x then x else f @@ unsafe_value x
  let[@inline] map x ~f = if is_none x then x else some @@ f @@ unsafe_value x

  let[@inline] value_map x ~default ~f =
    if is_none x then default else f @@ unsafe_value x

  let[@inline] iter x ~f = if not @@ is_none x then f @@ unsafe_value x
end

module Pre = Precalculated
module Bb = Bitboard
module Cr = Castling_rights

module T = struct
  (* We'll use mutable fields since, when applying moves, this has a
     performance advantage over a typical state monad pattern (where
     we are making a new copy every time we update a field). *)
  type t = {
    mutable white         : Bb.t;
    mutable black         : Bb.t;
    mutable pawn          : Bb.t;
    mutable knight        : Bb.t;
    mutable bishop        : Bb.t;
    mutable rook          : Bb.t;
    mutable queen         : Bb.t;
    mutable king          : Bb.t;
    mutable active        : Piece.color;
    mutable castle        : Cr.t;
    mutable en_passant    : Square.t Uopt.t;
    mutable halfmove      : int;
    mutable fullmove      : int;
    mutable hash          : int64;
    mutable pawn_hash     : int64;
    mutable material_hash : int64;
  } [@@deriving compare, equal, fields, sexp]

  let[@inline] en_passant pos = Uopt.to_option pos.en_passant

  (* We make an explicit copy because our move generator will return
     a new position (thus adhering to a functional style). *)
  let[@inline] copy pos = {
    white         = pos.white;
    black         = pos.black;
    pawn          = pos.pawn;
    knight        = pos.knight;
    bishop        = pos.bishop;
    rook          = pos.rook;
    queen         = pos.queen;
    king          = pos.king;
    active        = pos.active;
    castle        = pos.castle;
    en_passant    = pos.en_passant;
    halfmove      = pos.halfmove;
    fullmove      = pos.fullmove;
    hash          = pos.hash;
    pawn_hash     = pos.pawn_hash;
    material_hash = pos.material_hash;
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

(* En passant *)

let[@inline] is_en_passant pos sq =
  Uopt.is_some pos.en_passant &&
  Square.(sq = Uopt.unsafe_value pos.en_passant) 

let[@inline] en_passant_pawn_aux active ep = match active with
  | Piece.White -> Square.(with_rank_unsafe ep Rank.five)
  | Piece.Black -> Square.(with_rank_unsafe ep Rank.four)

let[@inline] en_passant_pawn_uopt pos =
  Uopt.map pos.en_passant ~f:(en_passant_pawn_aux pos.active)

let[@inline] en_passant_pawn pos = Uopt.to_option @@ en_passant_pawn_uopt pos

let[@inline] has_pawn_threat pos sq =
  let cap = Pre.pawn_capture sq @@ Piece.Color.opposite pos.active in
  Bb.((cap & pos.pawn & active_board pos) <> empty)

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
      let c = which_color_exn pos sq in
      let k = which_kind_exn pos sq in
      (sq, Piece.create c k) :: acc)

(* Zobrist hashing *)

module Hash = struct
  (* Helpers for updating the hash. *)
  module Update = struct
    let[@inline] flip h x = Int64.(h lxor x)
    let[@inline] piece c k sq h = flip h @@ Zobrist.piece c k sq
    let[@inline] castle c s h = flip h @@ Zobrist.castle c s
    let[@inline] en_passant_sq sq h = flip h @@ Zobrist.en_passant sq
    let[@inline] active_player h = flip h Zobrist.white_to_move

    let[@inline] en_passant pos h =
      Uopt.value_map pos.en_passant ~default:h ~f:(fun ep ->
          if has_pawn_threat pos ep then en_passant_sq ep h else h)

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

  (* Get the hash of only the pawn placement. *)
  let pawn_structure pos = Monad.State.exec begin
      collect_kind pos Pawn |> M.List.iter ~f:(fun (sq, c) ->
          M.update @@ Update.piece c Pawn sq)
    end 0L

  (* Get the hash of only of the material. *)
  let material pos = Monad.State.exec begin
      collect_all pos |> M.List.iter ~f:(fun (sq, p) ->
          let c, k = Piece.decomp p in
          M.update @@ Update.piece c k sq)
    end 0L
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

(* Miscellaneous rules *)

let in_check pos =
  let active_board = active_board pos in
  let attacks = Attacks.all pos (inactive pos) ~ignore_same:true in
  Bb.((active_board & pos.king & attacks) <> empty)

let is_insufficient_material pos = let open Bb in
  let active = active_board pos in
  let inactive = inactive_board pos in
  let kb = pos.king + pos.bishop in
  let kn = pos.king + pos.knight in
  (* Lone king on either side. Check if the other player has two knights.
     This rule is actually a bit fuzzy, because there is no forced mate
     from two knights. Yet, a statemate is possible. The FIDE rules say
     that this is not a draw by insufficient material, so we will play
     by their rules. *)
  if pos.king = active then
    Int.(count (kn & inactive) < 3)
  else if pos.king = inactive then
    Int.(count (kn & active) < 3)
  else
    (* Both sides have king+bishop or king+knight. *)
    let akb = (kb & active) = active && Int.equal 2 @@ count active in
    let akn = (kn & active) = active && Int.equal 2 @@ count active in
    let ikb = (kb & inactive) = inactive && Int.equal 2 @@ count inactive in
    let ikn = (kn & inactive) = inactive && Int.equal 2 @@ count inactive in
    (akb && ikb) || (akb && ikn) || (akn && ikb) || (akn && ikn)

(* Relevant info about the position for generating moves, as well as performing
   sanity checks. *)

module Analysis = struct
  module T = struct
    type t = {
      pos                   : T.t;
      king_sq               : Square.t;
      en_passant_pawn       : Square.t Uopt.t;
      occupied              : Bb.t;
      active_board          : Bb.t;
      inactive_board        : Bb.t;
      inactive_attacks      : Bb.t;
      pinners               : Bb.t array;
      num_checkers          : int;
      check_mask            : Bb.t;
      en_passant_check_mask : Bb.t;
      inactive_sliders      : (Square.t * Piece.kind) list;
    } [@@deriving fields]
  end

  (* Attacks of all piece kinds, starting from the king, intersected with
     the squares occupied by the corresponding inactive pieces. *)
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
    let open Bb in
    let bishop  = Pre.bishop king_sq occupied in
    let rook    = Pre.rook   king_sq occupied in
    let queen   = Pre.queen  king_sq occupied in
    let mask    = active_board -- king_sq in
    let pinners = Array.create ~len:Square.count empty in
    List.iter inactive_sliders ~f:(fun (sq, k) ->
        let checker, king = match k with
          | Piece.Bishop -> Pre.bishop sq occupied, bishop
          | Piece.Rook   -> Pre.rook   sq occupied, rook
          | Piece.Queen  -> Pre.queen  sq occupied, queen
          | _ -> empty, empty in
        let b = checker & king & mask & Pre.between king_sq sq in
        if b <> empty then
          let i = Square.to_int @@ first_set_exn b in
          let b = Array.unsafe_get pinners i ++ sq in
          Array.unsafe_set pinners i b);
    pinners

  (* Generate the masks which may restrict movement in the event of a check. *)
  let[@inline] checks pos ~en_passant_pawn:pw ~num_checkers ~checkers ~king_sq =
    if num_checkers = 1 then
      (* Test if the checker is a sliding piece. If so, then we can try to
         block the attack. Otherwise, they may only be captured. *)
      let open Bb.Syntax in
      let sq = Bb.first_set_exn checkers in
      match which_kind_exn pos sq with
      | Bishop | Rook | Queen -> checkers + Pre.between king_sq sq, Bb.empty
      | Pawn when Uopt.is_some pw ->
        (* Edge case for being able to get out of check via en passant
           capture. *)
        let ep, pw = Uopt.(unsafe_value pos.en_passant, unsafe_value pw) in
        if Square.(sq = pw) then checkers, !!ep else checkers, Bb.empty
      |  _ -> checkers, Bb.empty
    else Bb.(full, empty)

  (* Populate info needed for generating legal moves. *)
  let[@inline] create pos =
    (* First, find our king. *)
    let king_sq = Bb.(first_set_exn (pos.king & active_board pos)) in
    (* Square of the en passant pawn. For purposes of analysis, we only care
       if this pawn is actually threatened with an en passant capture. *)
    let en_passant_pawn = Uopt.bind pos.en_passant ~f:(fun ep ->
        if has_pawn_threat pos ep
        then Uopt.some @@ en_passant_pawn_aux pos.active ep
        else Uopt.none) in
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

    let to_string = function
      | Empty_board -> "Board is empty"
      | Full_board -> "Board is full"
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
          ~active ~castle ~en_passant ~halfmove ~fullmove
          ~hash:0L ~pawn_hash:0L ~material_hash:0L in
      set_hash pos @@ Hash.of_position pos;
      set_pawn_hash pos @@ Hash.pawn_structure pos;
      set_material_hash pos @@ Hash.material pos;
      if validate then validate_and_map pos else E.return pos
    | sections -> E.fail @@ Invalid_number_of_sections (List.length sections)

  let of_string_exn ?(validate = true) s =
    of_string s ~validate |> Result.map_error ~f:Error.to_string |> function
    | Error e -> invalid_argf "Failed to parse FEN string '%s': %s" s e ()
    | Ok pos -> pos
end

let pp ppf pos = Format.fprintf ppf "%s" @@ Fen.to_string pos

let start = Fen.(of_string_exn start)

(* Handling moves.

   This is likely to be the biggest source of bugs, as it is quite easy to
   incorrectly implement the rules of chess.

   NOTE: We originally used a state monad for the Makemove module, and a reader
   monad for the Movegen module. For performance reasons, we then switched to a
   reader monad for Makemove, with mutable fields in the position datatype.
   This pattern acted as a "pseudo" state monad. However, we've gotten rid of
   the monadic code entirely since the compiler had trouble with inlining and
   specialization, as there were a lot of calls to anonymous functions left in
   the compiled code. While the monadic style was much more concise and elegant,
   our priority for this code weighs far more in favor of performance.
*)

module Makemove = struct
  (* `en_passant` is the en passant aquare. This field is only valid if the
     square is threatened with capture.

     `en_passant_pawn` is the pawn that is "in front" of the en passant
     square. This field is only valid if the move we are making is in fact an
     en passant capture.

     `castle` is the side on which castling occurred, if any.

     `piece` is the piece being moved (which belongs to the active color).

     `direct_capture` is the inactive piece, if any, that will be captured
     by the "direct" move (e.g. it is not an en passant capture).
  *)
  type context = {
    en_passant      : Square.t Uopt.t;
    en_passant_pawn : Square.t Uopt.t;
    castle_side     : Cr.side Uopt.t;
    piece           : Piece.t;
    direct_capture  : Piece.t Uopt.t;
  } [@@deriving fields]

  let[@inline] update_hash pos ~f = set_hash pos @@ f pos.hash

  let[@inline] update_pawn_hash sq c pos =
    set_pawn_hash pos @@ Hash.Update.piece c Pawn sq pos.pawn_hash

  let[@inline] update_material_hash sq c k pos =
    set_material_hash pos @@ Hash.Update.piece c k sq pos.material_hash

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
    update_material_hash sq c k pos;
    begin match k with
      | Pawn -> update_pawn_hash sq c pos
      | _ -> ()
    end;
    k

  let set = Fn.flip Bb.set
  let clr = Fn.flip Bb.clear

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
  let[@inline] rook_moved src pos = rook_moved_or_captured src pos.active pos

  (* Rook was captured at a square. Assume that it is the inactive's color. *)
  let[@inline] rook_captured dst direct_capture pos =
    if Uopt.is_some direct_capture then
      let p = Uopt.unsafe_value direct_capture in
      if Piece.is_rook p then rook_moved_or_captured dst (Piece.color p) pos

  (* Handle castling-related details. *)
  let[@inline] update_castle ctx src dst pos =
    begin match Piece.kind ctx.piece with
      | Rook -> rook_moved src pos
      | King -> king_moved_or_castled ctx.castle_side pos
      | _ -> ()
    end;
    rook_captured dst ctx.direct_capture pos

  (* Reset the en passant hash and return the new en passant square if a pawn
     double push occurred. *) 
  let[@inline] update_en_passant ctx src dst pos =
    Uopt.iter ctx.en_passant ~f:(fun ep ->
        update_hash pos ~f:(Hash.Update.en_passant_sq ep));
    let ep = if Piece.is_pawn ctx.piece then
        let src_rank = Square.rank src in
        let dst_rank = Square.rank dst in
        match pos.active with
        | Piece.White when dst_rank - src_rank = 2 ->
          Uopt.some @@ Square.(with_rank_unsafe dst Rank.three)
        | Piece.Black when src_rank - dst_rank = 2 ->
          Uopt.some @@ Square.(with_rank_unsafe dst Rank.six)
        | _ -> Uopt.none
      else Uopt.none in
    set_en_passant pos ep

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
  let[@inline] do_promote p promote pos = match promote with
    | Some k -> Piece.with_kind p @@ Move.Promote.to_piece_kind k
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
    update_en_passant ctx src dst pos;
    update_halfmove ctx pos;
    update_castle ctx src dst pos;
    (* Clear the old placement. *)
    clear_square ctx.piece src pos;
    let capture = clear_square_capture dst ctx.direct_capture pos in
    (* Set the new placement. *)
    let p = do_promote ctx.piece promote pos in
    set_square p dst pos;
    let capture =
      if Uopt.is_some capture then capture
      else en_passant_capture ctx.en_passant_pawn pos in
    (* Prepare for the next move. *)
    update_fullmove pos;
    flip_active pos;
    (* If en passant state changed, then update the hash. *)
    update_hash pos ~f:(Hash.Update.en_passant pos);
    (* Return the capture that was made, if any. *)
    capture 
end

module Legal = struct
  module T = struct
    type t = {
      move          : Move.t;
      parent        : T.t;
      new_position  : T.t;
      capture       : Piece.kind Uopt.t;
      is_en_passant : bool;
      castle_side   : Cr.side Uopt.t;
    } [@@deriving compare, equal, sexp, fields]

  end

  include T
  include Comparable.Make(T)

  let same l1 l2 =
    same_hash l1.parent l2.parent &&
    same_hash l1.new_position l2.new_position

  let is_move legal m = Move.(m = legal.move)
  let capture legal = Uopt.to_option legal.capture
  let castle_side legal = Uopt.to_option legal.castle_side

  let capture_square legal =
    if Uopt.is_none legal.capture then None
    else Option.some @@
      let dst = Move.dst legal.move in
      if not legal.is_en_passant then dst
      else Fn.flip en_passant_pawn_aux dst @@ inactive legal.new_position
end

type legal = Legal.t [@@deriving compare, equal, sexp]

module Movegen = struct
  open Analysis

  module Pieces = struct
    module Pawn = struct
      let[@inline] push sq {pos; occupied; _} =
        Bb.(Pre.pawn_advance sq pos.active - occupied)

      (* Double pawn push. *)
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
        (* Remove our pawn and the captured pawn from the board, but pretend
           that the en passant square is occupied. This covers the case where
           we can capture the pawn, but it may leave our pawn pinned. *)
        let occupied = occupied -- sq -- pw ++ ep in
        let init = diag ++ ep and finish = ident in
        (* Check if an appropriate diagonal attack from the king would reach
           that corresponding piece. *)
        let bishop = Pre.bishop king_sq occupied in
        let rook   = Pre.rook   king_sq occupied in
        let queen  = Pre.queen  king_sq occupied in
        (List.fold_until [@specialised]) inactive_sliders ~init ~finish
          ~f:(fun acc -> function
              | sq, Piece.Bishop when sq @ bishop -> Stop diag
              | sq, Piece.Rook   when sq @ rook   -> Stop diag
              | sq, Piece.Queen  when sq @ queen  -> Stop diag
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

      let promote_kinds = Move.Promote.[Knight; Bishop; Rook; Queen]

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
      (* Note that `inactive_attacks` includes squares occupied by inactive
         pieces. Therefore, the king may not attack those squares. *)
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

    (* Special case for pawns, with en passant capture being an option to
       escape check. *)
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
  end

  (* Calculate the side that we're castling on, if any. *)
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
  let[@inline] run_makemove pos ~src ~dst ~promote ~piece ~en_passant_pawn =
    (* The new position, which we are free to mutate. *)
    let new_position = copy pos in
    (* All captures that are not en passant. *)
    let direct_capture = piece_at_square_uopt pos dst in
    (* Are we capturing on the en passant square? *)
    let is_en_passant = Piece.is_pawn piece && is_en_passant pos dst in
    (* The en passant square, which we only care about if a threat is
       possible. *)
    let en_passant =
      if Uopt.is_some en_passant_pawn then pos.en_passant else Uopt.none in
    (* The pawn "in front" of the en passant square, which we only care
       about if we are capturing it on this move. *)
    let en_passant_pawn =
      if is_en_passant then en_passant_pawn else Uopt.none in
    (* The side we're castling on, if any. *)
    let castle_side = castle_side piece src dst in
    (* Shared info for makemove. *)
    let ctx = Makemove.Fields_of_context.create
        ~en_passant ~en_passant_pawn ~piece ~castle_side ~direct_capture in
    (* Update the position and return the captured piece, if any. *)
    let capture = Makemove.go src dst promote ctx new_position in
    new_position, capture, is_en_passant, castle_side

  (* Accumulate a list of legal moves. *)
  let[@inline] accum_makemove acc move ~pos:parent ~en_passant_pawn ~piece =
    let src, dst, promote = Move.decomp move in
    let new_position, capture, is_en_passant, castle_side =
      run_makemove parent ~src ~dst ~promote ~piece ~en_passant_pawn in
    Legal.Fields.create
      ~move ~parent ~new_position ~capture ~is_en_passant ~castle_side :: acc

  (* If we're promoting, then the back rank should be the only
     available squares. *)
  let[@inline] is_promote_rank b = function
    | Piece.White -> Bb.((b & rank_8) = b)
    | Piece.Black -> Bb.((b & rank_1) = b)

  (* Get the list of moves from the bitboard of squares we can move to. *)
  let[@inline] bb_to_moves src k b ~init ~a:{pos; en_passant_pawn; _} =
    let active = pos.active in
    let piece = Piece.create active k in
    let f = accum_makemove ~pos ~en_passant_pawn ~piece in
    match k with
    | Piece.Pawn when is_promote_rank b active -> 
      (Bb.fold [@specialised]) b ~init ~f:(fun init dst ->
          Pieces.Pawn.promote src dst |> List.fold ~init ~f)
    | _ ->
      (Bb.fold [@specialised]) b ~init
        ~f:(fun acc dst -> Move.create src dst |> f acc)

  (* Piece-specific bitboards for legal moves. *)
  let[@inline] bb_of_kind sq k a = match k with
    | Piece.Pawn   -> Pieces.pawn   sq a
    | Piece.Knight -> Pieces.knight sq a
    | Piece.Bishop -> Pieces.bishop sq a
    | Piece.Rook   -> Pieces.rook   sq a
    | Piece.Queen  -> Pieces.queen  sq a
    | Piece.King   -> Pieces.king   sq a

  let[@inline] go ({pos; king_sq; num_checkers; _} as a) =
    (* If the king has more than one attacker, then it is the only piece
       we can move. *)
    if num_checkers > 1
    then Pieces.king king_sq a |> bb_to_moves king_sq King ~init:[] ~a
    else (List.fold [@specialised]) ~init:[] ~f:(fun init (sq, k) ->
        bb_of_kind sq k a |> bb_to_moves sq k ~init ~a) @@ collect_active pos
end

let legal_moves pos = Movegen.go @@ Analysis.create pos

let make_move pos move =
  match legal_moves pos |> List.find ~f:(Fn.flip Legal.is_move move) with
  | None -> invalid_argf "Move %s is not legal" (Move.to_string move) ()
  | Some legal -> legal

let null_move_unsafe pos =
  let pos = copy pos in
  Makemove.flip_active pos;
  Makemove.update_hash pos ~f:(Hash.Update.en_passant pos);
  set_en_passant pos Uopt.none;
  set_halfmove pos 0;
  pos

let null_move pos =
  if in_check pos
  then invalid_argf "Illegal null move on position %s" (Fen.to_string pos) ()
  else null_move_unsafe pos

(* Standard Algebraic Notation (SAN). *)

module San = struct
  let disambiguate parent k ~addc ~adds ~src ~dst =
    let a = Analysis.create parent in
    (* More than one checker means it's a king move, which is unambiguous. *)
    if a.Analysis.num_checkers <= 1 then
      let rank, file = Square.decomp src in
      (* Find all the other pieces of the same kind and generate their move
         bitboards. *)
      collect_kind parent k |> List.filter ~f:(fun (sq, c) ->
          Square.(sq <> src) && Piece.Color.(c = parent.active)) |>
      List.map ~f:(fun (sq, _) -> sq, Movegen.bb_of_kind sq k a) |>
      List.filter ~f:(fun (_, b) -> Bb.(dst @ b)) |> function
      | [] -> ()
      | moves ->
        let search x f =
          not @@ List.exists moves ~f:(fun (sq, _) -> f sq = x) in
        (* First try to distinguish by file, then by rank, and finally the
           departing square. *)
        if search file Square.file then addc @@ Square.File.to_char file
        else if search rank Square.rank then addc @@ Square.Rank.to_char rank
        else adds @@ Square.to_string src

  let of_legal legal =
    let buf = Buffer.create 16 in
    let adds = Buffer.add_string buf in
    let addc = Buffer.add_char buf in
    let src, dst, promote = Move.decomp @@ Legal.move legal in
    let pos = Legal.new_position legal in
    let num_checkers =
      let king_sq =
        List.hd_exn @@ collect_piece pos @@ Piece.create pos.active King in
      let occupied = all_board pos in
      let inactive_board = inactive_board pos in
      Bb.count @@ Analysis.checkers pos ~king_sq ~inactive_board ~occupied in
    let checkmate = num_checkers <> 0 && List.is_empty @@ legal_moves pos in
    begin match Legal.castle_side legal with
      (* Castling *)
      | Some Cr.Kingside -> adds "O-O"
      | Some Cr.Queenside -> adds "O-O-O"
      | None ->
        let p = piece_at_square_exn pos dst in
        let p = match promote with
          | Some _ -> Piece.with_kind p Pawn
          | None -> p in
        (* Piece being moved *)
        let dis = disambiguate ~addc ~adds ~src ~dst @@ Legal.parent legal in
        begin match Piece.kind p with
          | Piece.Pawn -> if Uopt.is_none legal.capture
            then adds @@ Square.to_string dst
            else addc @@ Square.file_char src
          | Piece.Knight -> addc 'N'; dis Knight
          | Piece.Bishop -> addc 'B'; dis Bishop
          | Piece.Rook   -> addc 'R'; dis Rook
          | Piece.Queen  -> addc 'Q'; dis Queen
          | Piece.King   -> addc 'K'
        end;
        (* Capture *)
        Uopt.to_option legal.capture |> Option.iter ~f:(fun _ -> addc 'x');
        (* Destination *)
        if not (Piece.is_pawn p && Uopt.is_none legal.capture)
        then adds @@ Square.to_string dst;
        (* Promotion *)
        Option.iter promote ~f:(function
            | Move.Promote.Knight -> adds "=N"
            | Move.Promote.Bishop -> adds "=B"
            | Move.Promote.Rook   -> adds "=R"
            | Move.Promote.Queen  -> adds "=Q");
    end;
    (* Checkmate or check *)
    if checkmate then addc '#'
    else if num_checkers = 1 then addc '+'
    else if num_checkers = 2 then adds "++";
    Buffer.contents buf

  let of_string s pos =
    legal_moves pos |> List.find ~f:(fun legal ->
        String.(s = of_legal legal))
end

include Comparable.Make(T)
