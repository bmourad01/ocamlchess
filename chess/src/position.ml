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

module Histogram = struct
  type t = histogram [@@deriving compare, equal, sexp]

  let empty = Int64.Map.empty
  let singleton pos = Int64.Map.singleton pos.hash 1

  let incr h pos = Map.update h pos.hash ~f:(function
      | Some n -> n + 1
      | None -> 1)

  let decr h pos = Map.change h pos.hash ~f:(function
      | None | Some 1 -> None
      | Some n -> Some (n - 1))

  let frequency h pos =
    Map.find h pos.hash |> Option.value ~default:0

  let to_sequence h = Map.to_sequence h
end

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
end

let same_hash pos1 pos2 = Int64.(pos1.hash = pos2.hash)

(* Attack patterns. *)

module Attacks = struct
  let white_idx = attacks_len - 2
  let black_idx = white_idx + 1

  let calculate pos = set_attacks pos @@ lazy begin
      let open Bb.Syntax in
      let occupied = all_board pos in
      let a = Array.init attacks_len ~f:(fun i ->
          if i < white_idx then
            let p = Piece.of_int_unsafe i in
            let c, k = Piece.decomp p in
            collect_piece pos p |>
            List.fold ~init:Bb.empty ~f:(fun acc sq ->
                acc + Pre.attacks sq occupied c k)
          else Bb.empty) in
      let[@inline] get p = Array.unsafe_get a @@ Piece.to_int p in
      let white =
        let open Piece  in
        get white_pawn   +
        get white_knight +
        get white_bishop +
        get white_rook   +
        get white_queen  +
        get white_king  in
      let black =
        let open Piece  in
        get black_pawn   +
        get black_knight +
        get black_bishop +
        get black_rook   +
        get black_queen  +
        get black_king  in
      Array.unsafe_set a white_idx white;
      Array.unsafe_set a black_idx black;
      a
    end

  let[@inline] get pos c k =
    let i = attacks_idx c k in
    let a = Lazy.force pos.attacks in
    Array.unsafe_get a i

  let[@inline] pawn   pos c = get pos c Pawn
  let[@inline] knight pos c = get pos c Knight
  let[@inline] bishop pos c = get pos c Bishop
  let[@inline] rook   pos c = get pos c Rook
  let[@inline] queen  pos c = get pos c Queen
  let[@inline] king   pos c = get pos c King

  let[@inline] piece pos p =
    let a = Lazy.force pos.attacks in
    Array.unsafe_get a @@ Piece.to_int p

  let[@inline] all pos c =
    let a = Lazy.force pos.attacks in
    Array.unsafe_get a (white_idx + Piece.Color.to_int c)
end

module Threats = struct
  module T = struct
    type t = {
      color : Piece.color;
      pawn  : Bb.t;
      minor : Bb.t;
      rook  : Bb.t;
    } [@@deriving compare, equal, sexp, fields]
  end

  include T
  include Comparable.Make(T)

  let get pos c =
    let them = board_of_color pos @@ Piece.Color.opposite c in
    let major = Bb.(pos.rook + pos.queen) in
    let minor = Bb.(pos.knight + pos.bishop) in
    let pawn_att = Attacks.pawn pos c in
    let knight_att = Attacks.knight pos c in
    let bishop_att = Attacks.bishop pos c in
    let rook_att = Attacks.rook pos c in
    Fields.create
      ~color:c
      ~pawn:Bb.(pawn_att & them & (minor + major))
      ~minor:Bb.((knight_att + bishop_att) & them & major)
      ~rook:Bb.(rook_att & them & pos.queen)

  let count t = Bb.count t.pawn + Bb.count t.minor + Bb.count t.rook
end

type threats = Threats.t [@@deriving compare, equal, sexp]

(* Relevant info about the position for generating moves, as well as performing
   sanity checks. *)

module Analysis = struct
  module T = struct
    type t = {
      pos                   : position;
      king_sq               : Square.t;
      en_passant_pawn       : Square.t Uopt.t;
      occupied              : Bb.t;
      active_board          : Bb.t;
      inactive_board        : Bb.t;
      inactive_attacks      : Bb.t;
      pin_masks             : Bb.t Map.M(Square).t;
      num_checkers          : int;
      check_mask            : Bb.t;
      en_passant_check_mask : Bb.t;
    } [@@deriving fields]
  end

  (* Calculate the set of pinned pieces and pinners. *)
  let[@inline] pins pos sliders sq c =
    let open Bb in
    let c = board_of_color pos c in
    let bq = Pre.bishop sq empty & (pos.queen + pos.bishop) in
    let rq = Pre.rook sq empty & (pos.queen + pos.rook) in
    let snipers = (bq + rq) & sliders in
    let occupied = all_board pos ^ snipers in
    let init = empty, empty in
    Bb.fold snipers ~init ~f:(fun ((pinned, pinners) as acc) ssq ->
        let b = Pre.between sq ssq & occupied in
        if Int.equal 1 @@ count b then
          let pinned = pinned + b in
          let pinners = if (b & c) <> empty then pinners ++ ssq else pinners in
          pinned, pinners
        else acc)

  let calculate_checks pos = set_checks pos @@ lazy begin
      let occupied = all_board pos in
      let wksq = Bb.(first_set_exn (pos.king & pos.white)) in
      let bksq = Bb.(first_set_exn (pos.king & pos.black)) in
      let[@specialise] squares c =
        let ksq = match c with
          | Piece.White -> wksq
          | Piece.Black -> bksq in
        let squares = Array.create ~len:Piece.Kind.count Bb.empty in
        let p = Pre.pawn_capture ksq c in
        let n = Pre.knight ksq in
        let b = Pre.bishop ksq occupied in
        let r = Pre.rook ksq occupied in
        Array.unsafe_set squares Piece.Kind.pawn   p;
        Array.unsafe_set squares Piece.Kind.knight n;
        Array.unsafe_set squares Piece.Kind.bishop b;
        Array.unsafe_set squares Piece.Kind.rook   r;
        Array.unsafe_set squares Piece.Kind.queen  Bb.(b + r);
        squares in
      let wpinned, bpinners = pins pos pos.black wksq White in
      let bpinned, wpinners = pins pos pos.white bksq Black in
      let wsquares = squares White in
      let bsquares = squares Black in
      {wpinned; bpinned; wpinners; bpinners; wsquares; bsquares}
    end

  (* For each pinned piece, calculate a pin mask to restrict its movement. *)
  let[@inline] pin_masks pos ~king_sq =
    let init = Map.empty (module Square) in
    pinned pos pos.active |> Bb.fold ~init ~f:(fun m sq ->
        Map.set m ~key:sq ~data:(Pre.line sq king_sq))

  (* Generate the masks which may restrict movement in the event of a check. *)
  let[@inline] checks pos ~en_passant_pawn ~num_checkers ~checkers ~king_sq =
    if num_checkers = 1 then
      (* Test if the checker is a sliding piece. If so, then we can try to
         block the attack. Otherwise, they may only be captured. *)
      let open Bb.Syntax in
      let sq = Bb.first_set_exn checkers in
      match which_kind_exn pos sq with
      | Bishop | Rook | Queen -> checkers + Pre.between king_sq sq, Bb.empty
      | Pawn when Uopt.is_some en_passant_pawn ->
        (* Edge case for being able to get out of check via en passant
           capture. *)
        let ep = Uopt.unsafe_value pos.en_passant in
        let pw = Uopt.unsafe_value en_passant_pawn in
        if Square.(sq = pw) then checkers, !!ep else checkers, Bb.empty
      |  _ -> checkers, Bb.empty
    else Bb.(full, empty)

  (* Populate info needed for generating legal moves. *)
  let[@inline] create pos =
    let open Bb.Syntax in
    (* First, find our king. *)
    let king_sq = Bb.first_set_exn (pos.king & active_board pos) in
    (* Square of the en passant pawn. For purposes of analysis, we only care
       if this pawn is actually threatened with an en passant capture. *)
    let en_passant_pawn =
      if Uopt.is_some pos.en_passant then
        let ep = Uopt.unsafe_value pos.en_passant in
        if has_pawn_threat pos ep
        then Uopt.some @@ en_passant_pawn_aux pos.active ep
        else Uopt.none
      else Uopt.none in
    (* Most general info. *)
    let inactive = inactive pos in
    let occupied = all_board pos in
    let active_board = active_board pos in
    let inactive_board = inactive_board pos in
    let inactive_pieces = collect_color pos inactive in
    (* We're considering attacked squares only for king moves. For sliding
       moves, we pretend that the king is removed from the board. Thus,
       when calculating king moves we will be able to exclude moves where
       the king doesn't escape from the attack ray (if he is in check). *)
    let inactive_attacks =
      let occupied = occupied -- king_sq in
      List.fold inactive_pieces ~init:Bb.empty ~f:(fun acc (sq, k) ->
          acc + Pre.attacks sq occupied inactive k) in
    (* Pinned pieces. *)
    let pin_masks = pin_masks pos ~king_sq in
    (* Pieces checking our king. *)
    let checkers = checkers pos in
    (* Number of checkers is important for how we can decide to get out of
       check. *)
    let num_checkers = Bb.count checkers in
    (* Masks which will may allow us to escape check. *)
    let check_mask, en_passant_check_mask =
      checks pos ~en_passant_pawn ~num_checkers ~checkers ~king_sq in
    (* Construct the analyzed position. *)
    T.Fields.create
      ~pos ~king_sq ~en_passant_pawn ~occupied ~active_board
      ~inactive_board ~inactive_attacks ~pin_masks ~num_checkers
      ~check_mask ~en_passant_check_mask

  include T
end

(* Miscellaneous rules *)

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
      let attacks = Attacks.all pos pos.active in
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
end

(* FEN parsing/unparsing *)

module Fen = struct
  (* This is based on the string:

     pppppppp/pppppppp/pppppppp/pppppppp/pppppppp/pppppppp/pppppppp/pppppppp w KQkq e3 9223372036854775807 9223372036854775807

     where 9223372036854775807 is (2^63)-1, the largest possible
     unsigned value for OCaml integers.
  *)
  let max_len = 121

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
      | String_too_long of int

    let pp ppf = function
      | Invalid_number_of_ranks n ->
        Format.fprintf ppf "Invalid number of ranks %d%!" n
      | Invalid_file_increment (n, sq) ->
        Format.fprintf ppf "Invalid file increment %d on square %a%!"
          n Square.pp sq
      | Rank_full rank ->
        Format.fprintf ppf "Piece placement on full rank %d%!" (rank + 1)
      | Invalid_piece_symbol (sym, sq) ->
        Format.fprintf ppf "Invalid piece symbol '%c' placed at square %a%!"
          sym Square.pp sq
      | Unspecified_squares (rank, n) ->
        Format.fprintf ppf "Rank %d has %d unspecified square(s)%!"
          (rank + 1) n
      | Invalid_active_color s ->
        Format.fprintf ppf "Invalid active color '%s'%!" s
      | Invalid_castling_rights s ->
        Format.fprintf ppf "Invalid castling rights '%s'%!" s
      | Invalid_en_passant s ->
        Format.fprintf ppf "Invalid en passant square '%s'%!" s
      | Invalid_halfmove s ->
        Format.fprintf ppf "Invalid halfmove clock '%s'%!" s
      | Invalid_fullmove s ->
        Format.fprintf ppf "Invalid fullmove clock '%s'%!" s
      | Invalid_position e ->
        Format.fprintf ppf "Invalid position; %a%!" Valid.Error.pp e
      | Invalid_number_of_sections n ->
        Format.fprintf ppf "Invalid number of sections %d%!" n
      | String_too_long n ->
        Format.fprintf ppf "String is too long (%d), maximum is %d" n max_len

    let to_string t = Format.asprintf "%a%!" pp t
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

  let pp_placement ppf pos =
    let rec aux rank file skip =
      if rank < 0 then ()
      else if file > 7 then begin
        if skip > 0 then Format.fprintf ppf "%d%!" skip;
        if rank > 0 then Format.fprintf ppf "/%!";
        aux (rank - 1) 0 0
      end else
        let p = piece_at_square_uopt pos @@ Square.create_exn ~rank ~file in
        if Uopt.is_none p then aux rank (file + 1) (skip + 1)
        else
          let p = Uopt.unsafe_value p in
          if skip > 0 then Format.fprintf ppf "%d%!" skip;
          Format.fprintf ppf "%c%!" @@ Piece.to_fen p;
          aux rank (file + 1) 0 in
    aux 7 0 0

  let pp_active ppf = function
    | Piece.White -> Format.fprintf ppf "w%!"
    | Piece.Black -> Format.fprintf ppf "b%!"

  let pp_castle ppf cr = Format.fprintf ppf "%a%!" Cr.pp cr

  let pp_en_passant ppf = function
    | None -> Format.fprintf ppf "-%!"
    | Some ep -> Format.fprintf ppf "%a%!" Square.pp ep

  let pp ppf pos =
    let sep () = Format.fprintf ppf " %!" in
    pp_placement ppf pos; sep ();
    pp_active ppf @@ active pos; sep ();
    pp_castle ppf @@ castle pos; sep ();
    pp_en_passant ppf @@ Uopt.to_option pos.en_passant; sep ();
    Format.fprintf ppf "%d %d%!" pos.halfmove pos.fullmove

  let to_string pos = Format.asprintf "%a%!" pp pos

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

  let of_string ?(validate = true) s =
    let len = String.length s in
    if len <= max_len then match String.split s ~on:' ' with
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
        let pos =
          let checkers = lazy Bb.empty in
          let checks = lazy empty_checks in
          let attacks = lazy [||] in
          Fields.create ~hash:0L ~pawn_king_hash:0L ~checkers ~checks
            ~attacks ~white ~black ~pawn ~knight ~bishop ~rook ~queen
            ~king ~active ~castle ~en_passant ~halfmove ~fullmove in
        set_hash pos @@ Hash.of_position pos;
        set_pawn_king_hash pos @@ Hash.of_pawn_king pos;
        calculate_checkers pos;
        Analysis.calculate_checks pos;
        Attacks.calculate pos;
        if validate then validate_and_map pos else E.return pos
      | sections -> E.fail @@ Invalid_number_of_sections (List.length sections)
    else E.fail @@ String_too_long len

  let of_string_exn ?(validate = true) s =
    of_string s ~validate |> Result.map_error ~f:Error.to_string |> function
    | Error e -> invalid_argf "Failed to parse FEN string '%s': %s" s e ()
    | Ok pos -> pos
end

let pp = Fen.pp
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
  (* Collection of precomputed info that is useful for generating the new
     position. We have the following fields:

     - en_passant: the en passant aquare. This field is only valid if
       the square is threatened with capture.

     - en_passant_pawn: the pawn that is "in front" of the en passant
       square. This field is only valid if the move we are making is in
       fact an en passant capture.

     - castle: the side on which castling occurred, if any.

     - piece: the piece being moved (which belongs to the active color).

     - direct_capture: the inactive piece, if any, that will be captured
       by the "direct" move (e.g. it is not an en passant capture).
  *)
  type info = {
    en_passant      : Square.t Uopt.t;
    en_passant_pawn : Square.t Uopt.t;
    castle_side     : Cr.side Uopt.t;
    piece           : Piece.t;
    direct_capture  : Piece.t Uopt.t;
  } [@@deriving fields]

  let[@inline] update_hash pos ~f = set_hash pos @@ f pos.hash

  let[@inline] update_pawn_king_hash sq c k pos =
    set_pawn_king_hash pos @@ Hash.Update.piece c k sq pos.pawn_king_hash

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
    begin match k with
      | Piece.Pawn | Piece.King -> update_pawn_king_hash sq c k pos
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
      map_piece p sq pos ~f:(clr sq) |> ignore

  (* The halfmove clock is reset after captures and pawn moves, and incremented
     otherwise. *)
  let[@inline] update_halfmove info pos = set_halfmove pos @@
    if Uopt.is_some info.en_passant_pawn
    || Uopt.is_some info.direct_capture
    || Piece.is_pawn info.piece
    then 0 else succ pos.halfmove

  (* Castling rights change monotonically, so the only time we update the hash
     is when we take away rights. *)
  let[@inline] castle_hash c s pos =
    if Cr.mem pos.castle c s then update_hash pos ~f:(Hash.Update.castle c s)

  let[@inline] clear_white_castling_rights pos =
    castle_hash White Kingside pos;
    castle_hash White Queenside pos;
    set_castle pos Cr.(pos.castle - white)

  let[@inline] clear_black_castling_rights pos =
    castle_hash Black Kingside pos;
    castle_hash Black Queenside pos;
    set_castle pos Cr.(pos.castle - black)

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

  let castles = [|
    white_kingside_castle;
    white_queenside_castle;
    black_kingside_castle;
    black_queenside_castle;
  |]

  let[@inline] select_castle pos s =
    (Array.unsafe_get castles
       (Ocaml_intrinsics.Int.count_trailing_zeros
          Cr.(to_int (pos.active --> s)))) pos

  (* If we're castling our king on this move, then we need to move the rook as
     well as clear our rights. *)
  let[@inline] king_moved_or_castled castle pos =
    if Uopt.is_some castle then
      select_castle pos @@ Uopt.unsafe_value castle
    else match pos.active with
      | Piece.White -> clear_white_castling_rights pos
      | Piece.Black -> clear_black_castling_rights pos

  (* If we're moving or capturing a rook, then clear the castling rights for
     that particular side. *)
  let[@inline] rook_moved_or_was_captured sq c pos = match c with
    | Piece.White when Square.(sq = h1) ->
      castle_hash White Kingside pos;
      set_castle pos Cr.(pos.castle - white_kingside)
    | Piece.White when Square.(sq = a1) ->
      castle_hash White Queenside pos;
      set_castle pos Cr.(pos.castle - white_queenside)
    | Piece.Black when Square.(sq = h8) ->
      castle_hash Black Kingside pos;
      set_castle pos Cr.(pos.castle - black_kingside)
    | Piece.Black when Square.(sq = a8) ->
      castle_hash Black Queenside pos;
      set_castle pos Cr.(pos.castle - black_queenside)
    | _ -> ()

  let[@inline] update_castle info src dst pos =
    (* Check if our king or rook moved. *)
    begin match Piece.kind info.piece with
      | Rook -> rook_moved_or_was_captured src pos.active pos
      | King -> king_moved_or_castled info.castle_side pos
      | _ -> ()
    end;
    (* Check if an enemy rook was captured. *)
    if Uopt.is_some info.direct_capture then
      let p = Uopt.unsafe_value info.direct_capture in
      if Piece.is_rook p then
        rook_moved_or_was_captured dst (Piece.color p) pos

  (* Reset the en passant hash and return the new en passant square if a pawn
     double push occurred. *) 
  let[@inline] update_en_passant info src dst pos =
    if Uopt.is_some info.en_passant then begin
      let ep = Uopt.unsafe_value info.en_passant in
      update_hash pos ~f:(Hash.Update.en_passant_sq ep)
    end;
    set_en_passant pos @@ if Piece.is_pawn info.piece then
      let src_rank = Square.rank src in
      let dst_rank = Square.rank dst in
      match pos.active with
      | Piece.White when dst_rank - src_rank = 2 ->
        Uopt.some @@ Square.(with_rank_unsafe dst Rank.three)
      | Piece.Black when src_rank - dst_rank = 2 ->
        Uopt.some @@ Square.(with_rank_unsafe dst Rank.six)
      | _ -> Uopt.none
    else Uopt.none

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
      clear_square p sq pos

  let[@inline] go src dst promote pos info =
    (* Do the stuff that relies on the initial state. *)
    update_en_passant info src dst pos;
    update_halfmove info pos;
    update_castle info src dst pos;
    (* Clear the old placement. *)
    clear_square info.piece src pos;
    clear_square_capture dst info.direct_capture pos;
    (* Set the new placement. *)
    let p = do_promote info.piece promote pos in
    set_square p dst pos;
    en_passant_capture info.en_passant_pawn pos;
    (* Prepare for the next move. *)
    update_fullmove pos;
    flip_active pos;
    (* If en passant state changed, then update the hash. *)
    update_hash pos ~f:(Hash.Update.en_passant pos)
end

module Child = struct
  type t = {
    move          : Move.t;
    parent        : position;
    self          : position Lazy.t;
    capture       : Piece.kind Uopt.t;
    is_en_passant : bool;
    castle_side   : Cr.side Uopt.t;
  } [@@deriving fields]

  let self child = Lazy.force child.self

  let same x y =
    same_hash  x.parent y.parent &&
    Move.equal x.move   y.move

  let is_move child m = Move.(m = child.move)
  let is_capture child = Uopt.is_some child.capture
  let is_castle child = Uopt.is_some child.castle_side
  let capture child = Uopt.to_option child.capture
  let castle_side child = Uopt.to_option child.castle_side

  let gives_check child =
    if Lazy.is_val child.self then in_check @@ self child
    else gives_check child.parent child.move

  let capture_square child =
    if Uopt.is_none child.capture then None
    else Option.some @@
      let dst = Move.dst child.move in
      if not child.is_en_passant then dst
      else Fn.flip en_passant_pawn_aux dst @@ inactive @@ self child

  let new_threats {move; parent = pos; _} =
    let c = active pos in
    let all = all_board pos in
    let them = Bb.(all - board_of_color pos c) in
    let major = Bb.(pos.rook + pos.queen) in
    let minor = Bb.(pos.knight + pos.bishop) in
    let src, dst, promote = Move.decomp move in
    match promote with
    | None -> begin
        match Piece.kind @@ piece_at_square_exn pos src with
        | Pawn   -> Bb.(Pre.pawn_capture dst c & them & (minor + major))
        | Knight -> Bb.(Pre.knight dst & them & major)
        | Bishop -> Bb.(Pre.(bishop dst all - bishop src all) & them & major)
        | Rook   -> Bb.(Pre.(rook dst all - rook src all) & them & pos.queen)
        | Queen  -> Bb.empty
        | King   -> Bb.empty
      end
    | Some k ->
      let p = Pre.pawn_capture src c in
      match k with
      | Knight -> Bb.((Pre.knight dst - p) & them & major)
      | Bishop -> Bb.((Pre.bishop dst all - p) & them & major)
      | Rook   -> Bb.((Pre.rook dst all - p) & them & pos.queen)
      | Queen  -> Bb.empty

  let is_passed_pawn {move; parent; _} =
    let open Bb in
    let src = Move.src move in
    let dst = Move.dst move in
    let us = board_of_color parent parent.active in
    let mask = Pre.passed_pawns dst parent.active in
    let our_pawns = parent.pawn & us in
    let their_pawns = parent.pawn - our_pawns in
    src @ our_pawns && (their_pawns & mask) = empty
end

type child = Child.t

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
      let[@inline] en_passant sq ep pw diag a =
        let open Bb in
        (* Remove our pawn and the captured pawn from the board, but pretend
           that the en passant square is occupied. This covers the case where
           we can capture the pawn, but it may leave our pawn pinned. *)
        let occupied = a.occupied -- sq -- pw ++ ep in
        (* Check if an appropriate diagonal attack from the king would reach
           that corresponding piece. *)
        let {bishop; rook; queen; _} = a.pos in
        let bq = Pre.bishop a.king_sq occupied & (bishop + queen) in
        let rq = Pre.rook   a.king_sq occupied & (rook   + queen) in
        if ((bq + rq) & a.inactive_board) <> empty then diag else diag ++ ep

      let[@inline] capture sq a =
        let open Bb.Syntax in
        let capture = Pre.pawn_capture sq a.pos.active in
        let diag = capture & a.inactive_board in
        if Uopt.is_some a.en_passant_pawn then
          let ep = Uopt.unsafe_value a.pos.en_passant  in
          let pw = Uopt.unsafe_value a.en_passant_pawn in
          if ep @ capture then en_passant sq ep pw diag a else diag
        else diag

      let[@inline] promote src dst =
        let f = Move.create_with_promote src dst in
        [f Knight; f Bishop; f Rook; f Queen]
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
    let[@inline] pin_mask sq a =
      Option.value ~default:Bb.full @@ Map.find a.pin_masks sq

    (* Special case for pawns, with en passant capture being an option to
       escape check. *)
    let[@inline] pawn_check_mask capture a =
      if a.num_checkers <> 1 then a.check_mask
      else Bb.(a.check_mask + (capture & a.en_passant_check_mask))

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
      (push + capture) & pin_mask sq a & pawn_check_mask capture a

    let[@inline] knight sq a = make sq a @@ Knight.jump  sq a
    let[@inline] bishop sq a = make sq a @@ Bishop.slide sq a
    let[@inline] rook   sq a = make sq a @@ Rook.slide   sq a
    let[@inline] queen  sq a = make sq a @@ Queen.slide  sq a
    let[@inline] king   sq a = Bb.(King.(move sq a + castle a))
  end

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

  let[@inline] run_makemove pos ~src ~dst ~promote ~piece ~en_passant_pawn =
    let is_en_passant = Piece.is_pawn piece && is_en_passant_square pos dst in
    let castle_side = castle_side piece src dst in
    let direct_capture =
      if is_en_passant then Uopt.none
      else piece_at_square_uopt pos dst in
    let capture =
      if is_en_passant then Uopt.some Piece.Pawn
      else if Uopt.is_some direct_capture then
        Uopt.some @@ Piece.kind @@ Uopt.unsafe_value direct_capture
      else Uopt.none in
    (* We will defer running makemove until the new position is actually
       needed. Unsurprisingly, this gives a significant speedup during
       alpha-beta search. *)
    let self = lazy begin
      let self = copy pos in
      let en_passant =
        if Uopt.is_some en_passant_pawn then pos.en_passant else Uopt.none in
      let en_passant_pawn =
        if is_en_passant then en_passant_pawn else Uopt.none in
      Makemove.go src dst promote self @@
      Makemove.Fields_of_info.create
        ~en_passant ~en_passant_pawn ~piece
        ~castle_side ~direct_capture;
      calculate_checkers self;
      calculate_checks self;
      Attacks.calculate self;
      self
    end in
    self, capture, is_en_passant, castle_side

  let[@inline] accum_makemove acc move ~parent ~en_passant_pawn ~piece =
    let src, dst, promote = Move.decomp move in
    let self, capture, is_en_passant, castle_side =
      run_makemove parent ~src ~dst ~promote ~piece ~en_passant_pawn in
    Child.Fields.create
      ~move ~parent ~self ~capture ~is_en_passant ~castle_side :: acc

  let[@inline] is_promote_rank b = function
    | Piece.White -> Bb.((b & rank_8) = b)
    | Piece.Black -> Bb.((b & rank_1) = b)

  let[@inline] bb_to_moves src k ~init ~a b = match k with
    | Piece.Pawn when is_promote_rank b a.pos.active ->
      (Bb.fold [@specialised]) b ~init
        ~f:(fun acc dst -> List.rev_append (Pieces.Pawn.promote src dst) acc)
    | _ ->
      (Bb.fold [@specialised]) b ~init
        ~f:(fun acc dst -> Move.create src dst :: acc)

  let[@inline] bb_to_children src k ~init ~a:{pos; en_passant_pawn; _} b =
    let active = pos.active in
    let piece = Piece.create active k in
    let f = accum_makemove ~parent:pos ~en_passant_pawn ~piece in
    match k with
    | Piece.Pawn when is_promote_rank b active -> 
      (Bb.fold [@specialised]) b ~init ~f:(fun init dst ->
          Pieces.Pawn.promote src dst |>
          (List.fold [@specialised]) ~init ~f)
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

  let[@inline][@specialise] go f a =
    if a.num_checkers <= 1 then
      (List.fold [@speciailised]) ~init:[] ~f:(fun init (sq, k) ->
          bb_of_kind sq k a |> f sq k ~init ~a) @@ collect_active a.pos
    else Pieces.king a.king_sq a |> f a.king_sq Piece.King ~init:[] ~a

  let[@inline][@specialise] go_captures f a =
    if a.num_checkers <= 1 then
      (List.fold [@speciailised]) ~init:[] ~f:(fun init (sq, k) ->
          let b = bb_of_kind sq k a in
          let b = match k with
            | Piece.Pawn when Uopt.is_some a.en_passant_pawn ->
              let ep = Uopt.unsafe_value a.pos.en_passant in
              Bb.(b & (a.inactive_board ++ ep))
            | _ -> Bb.(b & a.inactive_board) in
          f sq k ~init ~a b) @@ collect_active a.pos
    else
      Bb.(Pieces.king a.king_sq a & a.inactive_board) |>
      f a.king_sq Piece.King ~init:[] ~a

  let[@inline][@specialise] go_promotions f a =
    if a.num_checkers <= 1 then
      let mask = match a.pos.active with
        | Piece.White -> Bb.rank_8
        | Piece.Black -> Bb.rank_1 in
      Bb.(a.pos.pawn & a.active_board) |>
      (Bb.fold [@speciailised]) ~init:[] ~f:(fun init sq ->
          Bb.(Pieces.pawn sq a & mask) |> f sq Piece.Pawn ~init ~a)
    else []

  let[@inline][@specialise] go_quiet f a =
    if a.num_checkers <= 1 then
      (List.fold [@speciailised]) ~init:[] ~f:(fun init (sq, k) ->
          let b = bb_of_kind sq k a in
          let b = match k with
            | Piece.Pawn when Uopt.is_some a.en_passant_pawn ->
              let ep = Uopt.unsafe_value a.pos.en_passant in
              Bb.(b - (a.inactive_board ++ ep))
            | _ -> Bb.(b - a.inactive_board) in
          f sq k ~init ~a b) @@ collect_active a.pos
    else
      Bb.(Pieces.king a.king_sq a - a.inactive_board) |>
      f a.king_sq Piece.King ~init:[] ~a
end

let legal_moves pos =
  Movegen.(go bb_to_moves) @@ Analysis.create pos

let capture_moves pos =
  Movegen.(go_captures bb_to_moves) @@ Analysis.create pos

let promotion_moves pos =
  Movegen.(go_promotions bb_to_moves) @@ Analysis.create pos

let quiet_moves pos =
  Movegen.(go_quiet bb_to_moves) @@ Analysis.create pos

let children pos = Movegen.(go bb_to_children) @@ Analysis.create pos

let capture_children pos =
  Movegen.(go_captures bb_to_children) @@ Analysis.create pos

let promotion_children pos =
  Movegen.(go_promotions bb_to_children) @@ Analysis.create pos

let quiet_children pos =
  Movegen.(go_quiet bb_to_children) @@ Analysis.create pos

let make_move pos m =
  children pos |> List.find ~f:(Fn.flip Child.is_move m)

let make_move_exn pos move = match make_move pos move with
  | None -> invalid_argf "Move %s is not legal" (Move.to_string move) ()
  | Some child -> child

module Unsafe = struct
  let make_move parent move =
    let src, dst, promote = Move.decomp move in
    let self, capture, is_en_passant, castle_side =
      let piece = piece_at_square_uopt parent src in
      if Uopt.is_none piece then
        lazy (copy parent), Uopt.none, false, Uopt.none
      else
        let piece = Uopt.unsafe_value piece in
        let en_passant_pawn =
          if Uopt.is_some parent.en_passant then
            let ep = Uopt.unsafe_value parent.en_passant in
            if has_pawn_threat parent ep
            then Uopt.some @@ en_passant_pawn_aux parent.active ep
            else Uopt.none
          else Uopt.none in
        Movegen.run_makemove parent
          ~src ~dst ~promote ~piece ~en_passant_pawn in
    Child.Fields.create
      ~move ~parent ~self ~capture ~is_en_passant ~castle_side

  let[@inline] null_move pos =
    let pos = copy pos in
    Makemove.flip_active pos;
    Makemove.update_hash pos ~f:(Hash.Update.en_passant pos);
    set_en_passant pos Uopt.none;
    set_halfmove pos 0;
    pos

  let[@inline] is_en_passant pos m =
    is_en_passant_square pos (Move.dst m) &&
    let p = piece_at_square_uopt pos (Move.src m) in
    Uopt.is_some p &&
    let p = Uopt.unsafe_value p in
    Piece.is_pawn p &&
    Piece.(Color.equal (color p) pos.active)

  let[@inline] is_capture pos m =
    let open Bb.Syntax in
    (Move.src m @ active_board pos &&
     Move.dst m @ inactive_board pos) ||
    is_en_passant pos m

  let is_castle pos m =
    let src = Move.src m in
    let p = piece_at_square_uopt pos src in
    Uopt.is_some p &&
    let c, k = Piece.decomp @@ Uopt.unsafe_value p in
    Piece.Color.(c = pos.active) &&
    Piece.Kind.(k = King) &&
    is_castle src (Move.dst m) c
end

let null_move_exn pos =
  if in_check pos
  then invalid_argf "Illegal null move on position %s" (Fen.to_string pos) ()
  else Unsafe.null_move pos

(* Standard Algebraic Notation (SAN). *)

module San = struct
  let disambiguate ppf parent k ~src ~dst =
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
        if search file Square.file then
          Format.fprintf ppf "%c%!" @@ Square.File.to_char file
        else if search rank Square.rank then
          Format.fprintf ppf "%c%!" @@ Square.Rank.to_char rank
        else Format.fprintf ppf "%a%!" Square.pp src

  let pp ppf child =
    let src, dst, promote = Move.decomp @@ Child.move child in
    let pos = Child.self child in
    let num_checkers = Bb.count @@ checkers pos in
    let checkmate = num_checkers <> 0 && List.is_empty @@ children pos in
    begin match Child.castle_side child with
      (* Castling *)
      | Some Cr.Kingside -> Format.fprintf ppf "O-O%!"
      | Some Cr.Queenside -> Format.fprintf ppf "O-O-O%!"
      | None ->
        let p = piece_at_square_exn pos dst in
        let p = match promote with
          | Some _ -> Piece.with_kind p Pawn
          | None -> p in
        (* Piece being moved *)
        let dis = disambiguate ppf ~src ~dst @@ Child.parent child in
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
        (Move.to_string m) (Fen.to_string pos) ()

  let of_string s pos =
    children pos |> List.find ~f:(fun m -> String.equal s @@ of_child m)
end

(* Static Exchange Evaluation. *)

module See = struct
  (* Get the set of squares from sliding pieces that are attacking the
     destination square. *)
  let[@inline] sliders pos dst occupied =
    let open Bb.Syntax in
    let diag = Pre.bishop dst occupied in
    let orth = Pre.rook dst occupied in
    let b = diag & (pos.bishop + pos.queen) in
    let r = orth & (pos.rook + pos.queen) in
    b + r

  (* Get the set of squares that are attacking the destination square. *)
  let[@inline] attackers pos dst occupied =
    let open Bb.Syntax in
    let wp = Pre.pawn_capture dst Black & pos.white & pos.pawn in
    let bp = Pre.pawn_capture dst White & pos.black & pos.pawn in
    let n = Pre.knight dst & pos.knight in
    let k = Pre.king dst & pos.king in
    wp + bp + n + k + sliders pos dst occupied

  type state = {
    mutable from       : Square.t;
    mutable attackers  : Bb.t;
    mutable occupation : Bb.t;
    mutable target_val : int;
    mutable depth      : int;
    mutable side       : Piece.color;
  }

  let[@inline] init is_en_passant from dst pos victim =
    let side = pos.active in
    let attackers = attackers pos dst @@ all_board pos in
    let occupation =
      let sq =
        if is_en_passant then
          en_passant_pawn_aux pos.active @@
          Uopt.unsafe_value pos.en_passant
        else dst in
      Bb.(full -- sq) in
    let p = piece_at_square_exn pos from in
    let target_val = Piece.(Kind.value @@ kind p) in
    {from; attackers; occupation; target_val; depth = 0; side}

  (* Simple negamax search with a branching factor of 1. *)
  let[@inline] rec evaluate swap depth =
    let depth = depth - 1 in
    if depth > 0 then
      let s = Array.unsafe_get swap depth in
      let s1 = Array.unsafe_get swap (depth - 1) in
      Array.unsafe_set swap (depth - 1) (-(max (-s1) s));
      evaluate swap depth
    else Array.unsafe_get swap 0

  (* We're using `Base.Array.exists`, which starts from the end
     of the list. *)
  let[@inline] lva_order pos = [|
    pos.king;
    pos.queen;
    pos.rook;
    pos.bishop;
    pos.knight;
    pos.pawn;
  |]

  (* Calculate the mask for attackers on this turn. Pinned pieces shouldn't
     be able to attack if the pinners haven't moved yet. *)
  let[@inline] attacker_mask pos all us st =
    let mask = Bb.(st.attackers & us) in
    let pinners = pinners pos @@ Piece.Color.opposite st.side in
    if Bb.((all & st.occupation & pinners) <> empty)
    then Bb.(mask - pinned pos st.side)
    else mask

  (* Get the least valuable piece that is currently able to attack the
     square.

     Note that if we try to attack with the king, it can only be when the
     opposite side to move has no attackers left.
  *)
  let[@inline] lva pos all order st =
    let us = board_of_color pos st.side in
    let mask = attacker_mask pos all us st in
    Bb.(mask <> empty) &&
    let them = Bb.(all - us) in
    (Array.existsi [@specialised]) order ~f:(fun i b ->
        match Piece.Kind.(of_int_exn (count - 1 - i)) with
        | King when Bb.((st.attackers & them) <> empty) -> false
        | k -> match Bb.(first_set (mask & b)) with
          | None -> false
          | Some sq ->
            st.target_val <- Piece.Kind.value k;
            st.from <- sq;
            true)

  let see m pos is_en_passant victim =
    let src = Move.src m in
    let dst = Move.dst m in
    let st = init is_en_passant src dst pos victim in
    let value = Option.value_map victim ~default:0 ~f:Piece.Kind.value in
    let swap = Array.create value ~len:Square.count in
    let all = all_board pos in
    let order = lva_order pos in
    let continue = ref true in
    while !continue do
      (* Remove the attacker from the board. *)
      st.attackers <- Bb.(st.attackers -- st.from);
      st.occupation <- Bb.(st.occupation -- st.from);
      (* Now that the attacker is removed, compute sliding attacks that may have
         been previously blocked by him. *)
      let sliders = sliders pos dst Bb.(all & st.occupation) in
      let mask = board_of_color pos st.side in
      st.attackers <- Bb.((st.attackers + (sliders & mask)) & st.occupation);
      (* Next iteration. *)
      st.side <- Piece.Color.opposite st.side;
      st.depth <- st.depth + 1;
      let v = st.target_val - Array.unsafe_get swap (st.depth - 1) in
      Array.unsafe_set swap st.depth v;
      continue := lva pos all order st
    done;
    (* Evaluate the material gains/losses. *)
    evaluate swap st.depth

  let go (child : child) =
    let victim = Uopt.to_option child.capture in
    let m = Child.move child in
    let pos = Child.parent child in
    let is_en_passant = Child.is_en_passant child in
    see m pos is_en_passant victim

  let go_unsafe pos m =
    let dst = Move.dst m in
    let them = inactive_board pos in
    let is_en_passant = Unsafe.is_en_passant pos m in
    let victim =
      if is_en_passant || Bb.(dst @ them) then
        if is_en_passant then Some Piece.Pawn
        else Some (Piece.kind @@ piece_at_square_exn pos dst)
      else None in
    see m pos is_en_passant victim
end

module Comparable = struct
  (* This should work, with questionable efficiency. We can't use the
     derivers since the record contains lazy fields.

     We could instead just compare the Zobrist hashes, but we run the
     chance of a collision.
  *)
  let compare x y = String.compare (Fen.to_string x) (Fen.to_string y)
  let equal x y = compare x y = 0

  let t_of_sexp = function
    | Sexp.Atom fen -> Fen.of_string_exn fen
    | sexp -> invalid_argf "Invalid position sexp %s" (Sexp.to_string sexp) ()

  let sexp_of_t pos = Sexp.Atom (Fen.to_string pos)
end

include Comparable

include Base.Comparable.Make(struct
    include Comparable
    type t = position
  end)
