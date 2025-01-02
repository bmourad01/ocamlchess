open Core_kernel [@@warning "-D"]
open Monads.Std

module Pre = Precalculated
module Bb = Bitboard
module Cr = Castling_rights

(* Metadata for calculating results related to checks. *)
type checks = {
  wpinned  : Bb.t;
  bpinned  : Bb.t;
  wpinners : Bb.t;
  bpinners : Bb.t;
  wsquares : Bb.t array;
  bsquares : Bb.t array;
}

let empty_checks = {
  wpinned  = Bb.empty;
  bpinned  = Bb.empty;
  wpinners = Bb.empty;
  bpinners = Bb.empty;
  wsquares = Array.create Bb.empty ~len:Piece.Kind.count;
  bsquares = Array.create Bb.empty ~len:Piece.Kind.count;
}

(* The last two entries are the composite board of each color.  *)
let attacks_len =
  Piece.Color.count * Piece.Kind.count +
  Piece.Color.count

let attacks_idx c k =
  let c = Piece.Color.to_int c in
  let k = Piece.Kind.to_int k in
  (k lsl 1) lor c

(* We'll use mutable fields since, when applying moves, this has a
   performance advantage over a typical state monad pattern (where
   we are making a new copy every time we update a field). *)
type t = {
  mutable white          : Bb.t;
  mutable black          : Bb.t;
  mutable pawn           : Bb.t;
  mutable knight         : Bb.t;
  mutable bishop         : Bb.t;
  mutable rook           : Bb.t;
  mutable queen          : Bb.t;
  mutable king           : Bb.t;
  mutable active         : Piece.color;
  mutable castle         : Cr.t;
  mutable en_passant     : Square.t Uopt.t;
  mutable halfmove       : int;
  mutable fullmove       : int;
  mutable hash           : Zobrist.key;
  mutable pawn_king_hash : Zobrist.key;
  mutable checkers       : Bb.t Lazy.t;
  mutable checks         : checks Lazy.t;
  mutable attacks        : Bb.t array Lazy.t;
} [@@deriving fields]

type position = t
type histogram = int Int64.Map.t [@@deriving compare, equal, sexp]

let[@inline] en_passant pos = Uopt.to_option pos.en_passant
let[@inline] checkers pos = Lazy.force pos.checkers
let[@inline] in_check pos = Bb.(checkers pos <> empty)

(* We make an explicit copy because our move generator will return
   a new position (thus adhering to a functional style). *)
let[@inline] copy pos = {
  white          = pos.white;
  black          = pos.black;
  pawn           = pos.pawn;
  knight         = pos.knight;
  bishop         = pos.bishop;
  rook           = pos.rook;
  queen          = pos.queen;
  king           = pos.king;
  active         = pos.active;
  castle         = pos.castle;
  en_passant     = pos.en_passant;
  halfmove       = pos.halfmove;
  fullmove       = pos.fullmove;
  hash           = pos.hash;
  pawn_king_hash = pos.pawn_king_hash;
  checkers       = pos.checkers;
  checks         = pos.checks;
  attacks        = pos.attacks;
}

let[@inline] inactive pos = Piece.Color.opposite pos.active

let[@inline] pinned pos c =
  let checks = Lazy.force pos.checks in
  let a = (Obj.magic checks : Bb.t array) in
  let i = Piece.Color.to_int c in
  Array.unsafe_get a i

let[@inline] pinners pos c =
  let checks = Lazy.force pos.checks in
  let a = (Obj.magic checks : Bb.t array) in
  let i = Piece.Color.to_int c in
  Array.unsafe_get a (i + 2)

(* Bitboard accessors *)

let[@inline] all_board pos = Bb.(pos.white + pos.black)

let[@inline] board_of_color pos c =
  let a = ((Obj.magic (pos : t)) : Bb.t array) in
  let i = Piece.Color.to_int c in
  Array.unsafe_get a i

let[@inline] active_board pos = board_of_color pos pos.active
let[@inline] inactive_board pos = board_of_color pos @@ inactive pos

let[@inline] board_of_kind pos k =
  let a = ((Obj.magic (pos : t)) : Bb.t array) in
  let i = Piece.Kind.to_int k in
  Array.unsafe_get a (i + 2)

let[@inline] board_of_piece pos p =
  let c, k = Piece.decomp p in
  Bb.(board_of_color pos c & board_of_kind pos k)

(* En passant *)

let[@inline] is_en_passant_square pos sq =
  Uopt.is_some pos.en_passant &&
  Square.(sq = Uopt.unsafe_value pos.en_passant) 

let[@inline] en_passant_pawn_aux active ep = match active with
  | Piece.White -> Square.(with_rank_unsafe ep Rank.five)
  | Piece.Black -> Square.(with_rank_unsafe ep Rank.four)

let[@inline] en_passant_pawn_uopt pos =
  if Uopt.is_some pos.en_passant then
    Uopt.unsafe_value pos.en_passant |>
    en_passant_pawn_aux pos.active |>
    Uopt.some
  else Uopt.none

let[@inline] en_passant_pawn pos = Uopt.to_option @@ en_passant_pawn_uopt pos

let[@inline] has_pawn_threat pos sq =
  let cap = Pre.pawn_capture sq @@ Piece.Color.opposite pos.active in
  Bb.((cap & pos.pawn & active_board pos) <> empty)

let[@inline] has_non_pawn_material pos c =
  let open Bb in
  let us = board_of_color pos c in
  ((pos.knight + pos.bishop + pos.rook + pos.queen) & us) <> empty

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

let collect_color pos c = (Bb.fold [@specialised]) ~init:[] ~f:(fun acc sq ->
    (sq, which_kind_exn pos sq) :: acc) @@ board_of_color pos c

let collect_active pos = collect_color pos pos.active
let collect_inactive pos = collect_color pos @@ inactive pos

let collect_kind pos k = (Bb.fold [@specialised]) ~init:[] ~f:(fun acc sq ->
    (sq, which_color_exn pos sq) :: acc) @@ board_of_kind pos k

let collect_piece pos p =
  (Bb.fold [@specialised]) ~init:[] ~f:(fun acc sq -> sq :: acc) @@
  board_of_piece pos p

let piece_at_square_uopt pos sq =
  let c = which_color pos sq in
  if Uopt.is_none c then Uopt.none
  else
    let c = Uopt.unsafe_value c in
    let k = which_kind pos sq in
    if Uopt.is_none k then
      failwithf "Square %s is set for color %s, but no kind is available"
        (Square.to_string sq) (Piece.Color.to_string_hum c) ()
    else
      let k = Uopt.unsafe_value k in
      Uopt.some @@ Piece.create c k

let piece_at_square pos sq = Uopt.to_option @@ piece_at_square_uopt pos sq

let piece_at_square_exn pos sq =
  Piece.create (which_color_exn pos sq) (which_kind_exn pos sq)

let collect_all pos = (Bb.fold [@specialised]) ~init:[] ~f:(fun acc sq ->
    let c = which_color_exn pos sq in
    let k = which_kind_exn pos sq in
    (sq, Piece.create c k) :: acc) @@
  all_board pos

let same_hash pos1 pos2 = Int64.(pos1.hash = pos2.hash)

let calculate_checkers pos = set_checkers pos @@ lazy begin
    let open Bb in
    let checks = Lazy.force pos.checks in
    let squares = match pos.active with
      | Piece.White -> checks.wsquares
      | Piece.Black -> checks.bsquares in
    let q = pos.queen in
    let p = Array.unsafe_get squares Piece.Kind.pawn   & pos.pawn in
    let n = Array.unsafe_get squares Piece.Kind.knight & pos.knight in
    let b = Array.unsafe_get squares Piece.Kind.bishop & (pos.bishop + q) in
    let r = Array.unsafe_get squares Piece.Kind.rook   & (pos.rook + q) in
    (p + n + b + r) & inactive_board pos
  end

let is_insufficient_material pos =
  let open Bb in
  (* If there are any pawns, rooks, or queens on the board then there is
     enough material to deliver mate. *)
  pos.pawn = empty && pos.rook = empty && pos.queen = empty && begin
    let active = active_board pos in
    let inactive = inactive_board pos in
    (* If only kings are left then mate is impossible. *)
    pos.king = active + inactive || begin
      let kb = pos.king + pos.bishop in
      let kn = pos.king + pos.knight in
      let ka = (pos.king & active) = active in
      let ki = (pos.king & inactive) = inactive in
      (* Lone king vs king + knight or bishop. *)
      if ka && (kb & inactive) = inactive then
        Int.(count (kn & inactive) < 3)
      else if ki && (kb & active) = active then
        Int.(count (kn & active) < 3)
      else if ka && (kn & inactive) = inactive then
        Int.(count (kb & inactive) < 2)
      else if ki && (kn & active) = active then
        Int.(count (kb & active) < 2)
      else
        (* Both sides have king+bishop or king+knight. *)
        let akb = (kb & active) = active && Int.equal 2 @@ count active in
        let akn = (kn & active) = active && Int.equal 2 @@ count active in
        let ikb = (kb & inactive) = inactive && Int.equal 2 @@ count inactive in
        let ikn = (kn & inactive) = inactive && Int.equal 2 @@ count inactive in
        (akb && ikb) || (akb && ikn) || (akn && ikb) || (akn && ikn)
    end
  end

let is_castle src dst = function
  | Piece.White -> Square.(src = e1 && (dst = g1 || dst = c1))
  | Piece.Black -> Square.(src = e8 && (dst = g8 || dst = c8))

let gives_check pos m =
  let open Bb in
  let src, dst, promote = Move.decomp m in
  (* Make sure the piece exists. *)
  let p = piece_at_square_uopt pos src in
  Uopt.is_some p && begin
    (* Make sure it is the side to move. *)
    let c, k = Piece.decomp @@ Uopt.unsafe_value p in
    Piece.Color.(c = pos.active) &&
    let checks = Lazy.force pos.checks in
    let squares, pinned = match c with
      | Piece.White -> checks.bsquares, checks.bpinned
      | Piece.Black -> checks.wsquares, checks.wpinned in
    (* Direct check. *)
    dst @ Array.unsafe_get squares @@ Piece.Kind.to_int k || begin
      let ksq = first_set_exn (pos.king & inactive_board pos) in
      (* Discovered check. *)
      (src @ pinned && not (ksq @ Pre.line src dst)) || begin
        let all = all_board pos in
        match promote with
        | Some k ->
          (* Promotion check. *)
          let k = Move.Promote.to_piece_kind k in
          ksq @ Pre.attacks dst (all -- src -- dst) c k
        | None -> match k with
          | Piece.Pawn when is_en_passant_square pos dst ->
            (* En passant check. *)
            let pw = Uopt.unsafe_value @@ en_passant_pawn_uopt pos in
            let occ = (all -- src -- pw) ++ dst in
            let bq = Pre.bishop ksq occ & (pos.queen + pos.bishop) in
            let rq = Pre.rook   ksq occ & (pos.queen + pos.rook) in
            ((bq + rq) & board_of_color pos c) <> empty
          | Piece.King when is_castle src dst c ->
            (* Castling check. *)
            let r = Square.(to_int @@ if dst > src then f1 else d1) in
            let i = Piece.Color.to_int pos.active * 56 in
            let r = Square.of_int_exn Int.(r lxor i) in
            ksq @ Pre.rook r empty &&
            ksq @ Pre.rook r (all -- src -- dst)
          | _ -> false
      end
    end
  end
