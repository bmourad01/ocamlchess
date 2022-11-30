(* Adapted from:

   https://github.com/AndyGrant/Ethereal/blob/master/src/evaluate.c
*)

open Core_kernel [@@warning "-D"]

module Bb = Bitboard
module Pre = Precalculated

(* To avoid excessive allocations, we'll represent the scores as a
   single integer, with the middle and endgame scores taking up 16
   bits each.

   Since the scores may be negative, we need to explicitly perform a
   sign extension when we try to extract them. Such operations aren't
   as common as simple arithmetic on the evaluations, which can be done
   with the normal integer operations.
*)

module Score = struct
  type t = int

  (* XXX: use Int63? *)
  let () =
    let s = Caml.Sys.int_size in
    if s < 32 then
      failwithf "Integer size must be at least 32 bits (got %d)" s ()

  let bits = 16
  let mask = (1 lsl bits) - 1
  let smask = 1 lsl (bits - 1)

  let[@inline] sext x = (x lxor smask) - smask
  let[@inline] make mg eg = (eg lsl bits) + mg
  let[@inline] mg s = sext (s land mask)
  let[@inline] eg s = sext (((s + smask) lsr bits) land mask)

  module Syntax = struct
    (* Convenience. *)
    let ($) = make

    (* Avoid conflicts with the bitboard operators. *)
    let (+$) = Int.(+)
  end
end

type score = Score.t

open Score.Syntax

(* Helpers. *)

let pawn_advance = function
  | Piece.White -> fun b -> Bb.(b << 8)
  | Piece.Black -> fun b -> Bb.(b >> 8)

let pawn_captures p = function
  | Piece.White -> Bb.((p << 7) - file_h, (p << 9) - file_a)
  | Piece.Black -> Bb.((p >> 7) - file_a, (p >> 9) - file_h)

let rammed ours theirs = function
  | Piece.White -> Bb.((theirs >> 8) & ours)
  | Piece.Black -> Bb.((theirs << 8) & ours)

let relative_rank = function
  | Piece.White -> Square.rank
  | Piece.Black -> fun sq -> 7 - Square.rank sq

let relative_square sq c =
  let r = relative_rank c sq in
  let sq = Square.with_rank_exn sq r in
  Square.to_int sq

let backmost = function
  | Piece.White -> Bb.first_set_exn
  | Piece.Black -> Bb.first_set_rev_exn

let square_fwd = function
  | Piece.White -> fun sq -> Square.(of_int_exn (to_int sq + 8))
  | Piece.Black -> fun sq -> Square.(of_int_exn (to_int sq - 8))

let third_rank = function
  | Piece.White -> Bb.rank_3
  | Piece.Black -> Bb.rank_6

let uget = Array.unsafe_get
let uset = Array.unsafe_set

(* Pawn and king placements change less often than other pieces, so we
   can cache the related information.

   See: https://www.chessprogramming.org/Pawn_Hash_Table
*)

module Pawn_king_cache = struct
  type entry = {
    key     : Zobrist.key;
    weval   : score;
    beval   : score;
    wsafety : score;
    bsafety : score;
    wpassed : Bb.t;
    bpassed : Bb.t;
  }

  let len = 1 lsl 16
  let mask = Int64.of_int (len - 1)
  let table = Option_array.create ~len
  let same e k = Zobrist.equal_key e.key k
  let slot k = Int64.(to_int_trunc (k land mask))

  let find pos =
    let k = Position.pawn_king_hash pos in
    slot k |> Option_array.unsafe_get table |>
    Option.bind ~f:(fun e -> Option.some_if (same e k) e)
end

type pkentry = Pawn_king_cache.entry

(* Common info used by the evaluation features. *)

module Info = struct
  type info = {
    pawn_attacks               : Bb.t;
    pawn_attacks2              : Bb.t;
    rammed_pawns               : Bb.t;
    blocked_pawns              : Bb.t;
    king_square                : Square.t;
    king_area                  : Bb.t;
    mobility_area              : Bb.t;
    no_bishop                  : Bb.t;
    no_rook                    : Bb.t;
    attacked_by                : Bb.t array;
    mutable passed_pawns       : Bb.t;
    mutable attacks            : Bb.t;
    mutable attacks2           : Bb.t;
    mutable num_king_attacks   : int;
    mutable num_king_attackers : int;
    mutable king_attackers     : score;
    mutable pawn_king_eval     : score;
    mutable pawn_king_safety   : score;
  }

  let extract c = function
    | None -> (0 $ 0), (0 $ 0), Bb.empty
    | Some (e : pkentry) -> match c with
      | Piece.White -> e.weval, e.wsafety, e.wpassed
      | Piece.Black -> e.beval, e.bsafety, e.bpassed

  let create_info pos c pke =
    let open Bb in
    let c' = Piece.Color.opposite c in
    let all = Position.all_board pos in
    let us = Position.board_of_color pos c in
    let them = all - us in
    let pawn = Position.pawn pos in
    let our_king = Position.king pos & us in
    let our_queens = Position.queen pos & us in
    let our_pawns = pawn & us in
    let their_pawns = pawn & them in
    let pawn_left, pawn_right = pawn_captures our_pawns c in
    let pawn_left', pawn_right' = pawn_captures their_pawns c' in
    let blocked_pawns = rammed our_pawns all c in
    let their_pawn_attacks = pawn_left' + pawn_right' in
    let king_square = Bb.first_set_exn our_king in
    let king_attacks = Pre.king king_square in
    let pawn_king_eval, pawn_king_safety, passed_pawns = extract c pke in
    let attacked_by = Array.create Bb.empty ~len:Piece.Kind.count in
    let pawn_attacks = pawn_left + pawn_right in
    let pawn_attacks2 = pawn_left & pawn_right in
    attacked_by.(Piece.Kind.pawn) <- pawn_attacks;
    attacked_by.(Piece.Kind.king) <- king_attacks; {
      pawn_attacks;
      pawn_attacks2;
      rammed_pawns = rammed our_pawns their_pawns c;
      blocked_pawns;
      king_square;
      king_area = Pre.king_area king_square c;
      mobility_area = ~~(their_pawn_attacks + our_king + blocked_pawns);
      no_bishop = all ^ ((Position.bishop pos & us) + our_queens);
      no_rook = all ^ ((Position.rook pos & us) + our_queens);
      attacked_by;
      passed_pawns;
      attacks = pawn_attacks + king_attacks;
      attacks2 = pawn_attacks2 + (pawn_attacks & king_attacks);
      num_king_attacks = 0;
      num_king_attackers = 0;
      king_attackers = 0 $ 0;
      pawn_king_eval;
      pawn_king_safety;
    }

  let attacked_by info k =
    uget info.attacked_by @@ Piece.Kind.to_int k

  let update_attacks info a k =
    let open Bb.Syntax in
    let k = Piece.Kind.to_int k in
    let ak = uget info.attacked_by k in
    uset info.attacked_by k (a + ak);
    info.attacks2 <- info.attacks2 + (a & info.attacks);
    info.attacks <- info.attacks + a

  let update_king_safety info a w =
    let a = Bb.(a & (info.king_area - info.pawn_attacks2)) in
    if Bb.(a <> empty) then begin
      info.num_king_attacks <- info.num_king_attacks + Bb.count a;
      info.num_king_attackers <- info.num_king_attackers + 1;
      info.king_attackers <- info.king_attackers + w;
    end

  type t = {
    pos     : Position.t;
    white   : info;
    black   : info;
    pkentry : pkentry option;
  }

  let create pos =
    let pke = Pawn_king_cache.find pos in {
      pos;
      white = create_info pos White pke;
      black = create_info pos Black pke;
      pkentry = pke;
    }

  let color t = function
    | Piece.White -> t.white
    | Piece.Black -> t.black

  let pawn_king_eval t =
    t.white.pawn_king_eval - t.black.pawn_king_eval

  let save_pkentry t = match t.pkentry with
    | Some _ -> ()
    | None ->
      let open Pawn_king_cache in
      let key = Position.pawn_king_hash t.pos in
      Option_array.unsafe_set_some table (slot key) {
        key;
        weval = t.white.pawn_king_eval;
        beval = t.black.pawn_king_eval;
        wsafety = t.white.pawn_king_safety;
        bsafety = t.black.pawn_king_safety;
        wpassed = t.white.passed_pawns;
        bpassed = t.black.passed_pawns;
      }
end

type info = Info.t

(* Evaluation features. *)

module Safety = struct
  (* These are for piece-specific evaluations, to be later used in the
     king safety evaluation. *)
  let knight = 48 $ 41
  let bishop = 24 $ 35
  let rook = 37 $ 8
  let queen = 30 $ 6

  (* King safety. *)
  let attack_value = 45 $ 34
  let weak_squares = 42 $ 41
  let no_enemy_queens = -237 $ -259
  let safe_queen_check = 93 $ 83
  let safe_rook_check = 90 $ 98
  let safe_bishop_check = 59 $ 59
  let safe_knight_check = 112 $ 117
  let adjustment = -74 $ -26

  module Shelter = struct
    let t = [|
      (* Different file. *)
      -2 $ 7;
      -1 $ 13;
      0 $ 8;
      4 $ 7;
      6 $ 2;
      -1 $ 0;
      2 $ 0;
      0 $ -13;
      (* Same file. *)
      0 $ 0;
      -2 $ 13;
      -2 $ 9;
      4 $ 5;
      3 $ 1;
      -3 $ 0;
      -2 $ 0;
      -1 $ -9;
    |]

    let get same_file file =
      let i = file + (Bool.to_int same_file * Square.File.count) in
      uget t i
  end

  module Storm = struct
    let t = [|
      (* Not blocked. *)
      -4 $ -1;
      -8 $ 3;
      0 $ 5;
      1 $ -1;
      3 $ 6;
      -2 $ 20;
      -2 $ 18;
      2 $ -12;
      (* Blocked. *)
      0 $ 0;
      1 $ 0;
      -1 $ 4;
      0 $ 0;
      0 $ 5;
      -1 $ 1;
      1 $ 0;
      1 $ 0;
    |]

    let get blocked rank =
      let i = rank + (Bool.to_int blocked * Square.Rank.count) in
      uget t i
  end
end

module Pawn = struct
  module Passers = struct
    let t = [|
      (* No backup. *)
      0 $ 0;
      -11 $ -18;
      -16 $ 18;
      -18 $ 29;
      -22 $ 61;
      21 $ 59;
      0 $ 0;
      0 $ 0;
      (* Backup. *)
      0 $ 0;
      -12 $ 21;
      -7 $ 27;
      2 $ 53;
      22 $ 116;
      49 $ 78;
      0 $ 0;
      0 $ 0;
    |]

    let get backup rank =
      let i = rank + (Bool.to_int backup * Square.Rank.count) in
      uget t i
  end

  (* By file. *)
  let isolated = [|
    -13 $ -12;
    -1 $ -16;
    1 $ -16;
    3 $ -18;
    7 $ -19;
    3 $ -15;
    -4 $ -14;
    -4 $ -17;
  |]

  module Stacked = struct
    let t = [|
      (* Not unstackable. *)
      10 $ -29;
      -2 $ -26;
      0 $ -23;
      0 $ -20;
      3 $ -20;
      5 $ -26;
      4 $ -30;
      8 $ -31;
      (* Unstackable. *)
      3 $ -14;
      0 $ -15;
      -6 $ -9;
      -7 $ -10;
      -4 $ -9;
      -2 $ -10;
      0 $ -13;
      0 $ -17;
    |]

    let get unstack file =
      let i = file + (Bool.to_int unstack * Square.File.count) in
      uget t i
  end

  module Backwards = struct
    let t = [|
      (* Blocking. *)
      0 $ 0;
      0 $ -7;
      7 $ -7;
      6 $ -18;
      -4 $ -29;
      0 $ 0;
      0 $ 0;
      0 $ 0;
      (* Not blocking. *)
      0 $ 0;
      -9 $ -32;
      -5 $ -30;
      3 $ -31;
      29 $ -41;
      0 $ 0;
      0 $ 0;
      0 $ 0;
    |]

    let get not_blocking rank =
      let i = rank + (Bool.to_int not_blocking * Square.Rank.count) in
      uget t i
  end

  module Connected = struct
    let t = [|
      (* 1 *)
      0 $ 0;
      0 $ 0;
      0 $ 0;
      0 $ 0;
      (* 2 *)
      -1 $ -11;
      12 $ -4;
      0 $ -2;
      6 $ 8;
      (* 3 *)
      14 $ 0;
      20 $ -6;
      19 $ 3;
      17 $ 8;
      (* 4 *)
      6 $ -1;
      20 $ 1;
      6 $ 3;
      14 $ 10;
      (* 5 *)
      8 $ 14;
      21 $ 17;
      31 $ 23;
      25 $ 18;
      (* 6 *)
      45 $ 40;
      36 $ 64;
      58 $ 74;
      64 $ 88;
      (* 7 *)
      108 $ 35;
      214 $ 45;
      216 $ 70;
      233 $ 61;
      (* 8 *)
      0 $ 0;
      0 $ 0;
      0 $ 0;
      0 $ 0;
    |]

    let get sq c =
      let r = relative_rank c sq in
      let fc = Square.File.count / 2 in
      let f = Square.(File.mirror_exn @@ file sq) in
      uget t (fc * r + f)
  end

  let[@inline] go (info : info) c =
    let open Bb in
    let our_info = Info.color info c in
    let c' = Piece.Color.opposite c in
    let all = Position.all_board info.pos in
    let us = Position.board_of_color info.pos c in
    let them = all - us in
    let pawn = Position.pawn info.pos in
    let our_pawns = pawn & us in
    let their_pawns = pawn & them in
    let fwd = square_fwd c in
    let relr = relative_rank c in
    let eval = Bb.fold our_pawns ~init:(0 $ 0) ~f:(fun acc sq ->
        let fsq = fwd sq and file = Square.file sq in
        (* Features. *)
        let neighbors    = our_pawns   & neighbor_files_exn file in
        let backup       = our_pawns   & Pre.passed_pawns sq c'  in
        let stoppers     = their_pawns & Pre.passed_pawns sq c   in
        let threats      = their_pawns & Pre.pawn_capture sq c   in
        let support      = our_pawns   & Pre.pawn_capture sq c'  in
        let push_threats = their_pawns & Pre.pawn_capture fsq c  in
        let push_support = our_pawns   & Pre.pawn_capture fsq c' in
        let leftovers    = stoppers ^ threats ^ push_threats     in
        (* Passed pawns. *)
        let acc =
          if stoppers <> empty then
            let ns, nt = count push_support, count push_threats in
            if leftovers = empty && Int.(ns >= nt) then
              let ns, nt = count support, count threats in
              acc +$ Passers.get Int.(ns >= nt) (relr sq)
            else acc
          else begin
            our_info.passed_pawns <- our_info.passed_pawns ++ sq;
            acc
          end in
        (* Isolated pawns. *)
        let not_isolated = threats <> empty || neighbors <> empty in
        let acc = if not_isolated then acc else acc +$ uget isolated file in
        (* Stacked (doubled) pawns. *)
        let fb = file_exn file in
        let acc =
          if several (our_pawns & fb) then
            let unstack =
              (stoppers <> empty && not_isolated) ||
              (stoppers - Pre.forward_files sq c) <> empty in
            acc +$ Stacked.get unstack file
          else acc in
        (* Backward or connected pawns. *)
        let acc =
          if neighbors <> empty
          && push_threats <> empty
          && backup = empty then
            let not_blocking = (fb & their_pawns) = empty in
            acc +$ Backwards.get not_blocking (relr sq)
          else if (our_pawns & Pre.connected_pawns sq c) <> empty then
            acc +$ Connected.get sq c
          else acc in
        acc) in
    our_info.pawn_king_eval <- eval
end

module Knight = struct
  module Outpost = struct
    let t = [|
      12 $ -32;
      40 $ 0;
      7 $ -24;
      21 $ -3;
    |]

    let get outside defended =
      let o = Bool.to_int outside in
      let d = Bool.to_int defended in
      uget t (d + o * 2)
  end

  let behind_pawn = 3 $ 28

  (* By distance from either king (using the Chebyshev method). *)
  let king_distance = [|
    -9 $ -6;
    -12 $ -20;
    -27 $ -20;
    -47 $ -19;
  |]

  (* By number of attacked squares in our mobility area *)
  let mobility = [|
    -104 $ -139;
    -45 $ -114;
    -22 $ -37;
    -8 $ 3;
    6 $ 15;
    11 $ 34;
    19 $ 38;
    30 $ 37;
    43 $ 17;
  |]

  let[@inline] go (info : info) c =
    let open Bb in
    let c' = Piece.Color.opposite c in
    let our_info = Info.color info c in
    let their_info = Info.color info c' in
    let all = Position.all_board info.pos in
    let us = Position.board_of_color info.pos c in
    let them = all - us in
    let pawn = Position.pawn info.pos in
    let knight = Position.knight info.pos in
    let our_knights = knight & us in
    let their_pawns = pawn & them in
    let outpost = Pre.outpost_ranks c in
    let adv = pawn_advance c' pawn in
    Bb.fold our_knights ~init:(0 $ 0) ~f:(fun acc sq ->
        let att = Pre.knight sq in
        Info.update_attacks our_info att Knight;
        (* Knight outpost. *)
        let acc =
          let os = Pre.outpost_squares sq c in
          if sq @ outpost && (os & their_pawns) = empty then
            let outside = sq @ (file_a + file_h) in
            let defended = sq @ our_info.pawn_attacks in
            acc +$ Outpost.get outside defended
          else acc in
        (* Behind pawn. *)
        let acc = if sq @ adv then acc +$ behind_pawn else acc in
        (* Distance from king. *)
        let acc =
          let ours = Square.chebyshev sq our_info.king_square in
          let theirs = Square.chebyshev sq their_info.king_square in
          let d = Int.(min ours theirs - 4) in
          if Int.(d < 0) then acc else acc +$ uget king_distance d in
        (* Mobility. *)
        let acc =
          let c = count (our_info.mobility_area & att) in
          acc +$ uget mobility c in
        (* King safety. *)
        Info.update_king_safety their_info att Safety.knight;
        acc)
end

module Bishop = struct
  module Outpost = struct
    let t = [|
      16 $ -16;
      50 $ -3;
      9 $ -9;
      -4 $ -4;
    |]

    let get outside defended =
      let o = Bool.to_int outside in
      let d = Bool.to_int defended in
      uget t (d + o * 2)
  end

  let pair = 22 $ 88
  let rammed_pawn = -8 $ -17
  let behind_pawn = 4 $ 24
  let long_diagonal = 26 $ 20

  (* By number of attacked squares in our mobility area *)
  let mobility = [|
    -99 $ -186;
    -46 $ -124;
    -16 $ -54;
    -4 $ -14;
    6 $ 1;
    14 $ 20;
    17 $ 35;
    19 $ 39;
    19 $ 49;
    27 $ 48;
    26 $ 48;
    52 $ 32;
    55 $ 47;
    83 $ 2;
  |]

  let[@inline] go (info : info) c =
    let open Bb in
    let c' = Piece.Color.opposite c in
    let our_info = Info.color info c in
    let their_info = Info.color info c' in
    let all = Position.all_board info.pos in
    let us = Position.board_of_color info.pos c in
    let them = all - us in
    let pawn = Position.pawn info.pos in
    let bishop = Position.bishop info.pos in
    let our_bishops = bishop & us in
    let their_pawns = pawn & them in
    let orank = Pre.outpost_ranks c in
    let adv = pawn_advance c' pawn in
    (* Bishop pair. *)
    let init =
      if (our_bishops & white) <> empty
      && (our_bishops & black) <> empty
      then pair else 0 $ 0 in
    Bb.fold our_bishops ~init ~f:(fun acc sq ->
        let att = Pre.bishop sq our_info.no_bishop in
        Info.update_attacks our_info att Bishop;
        (* Rammed pawns. *)
        let acc =
          let m = if sq @ white then white else black in
          let c = count (our_info.rammed_pawns & m) in
          acc +$ (rammed_pawn * c) in
        (* Bishop outpost. *)
        let acc =
          let osq = Pre.outpost_squares sq c in
          if sq @ orank && (osq & their_pawns) = empty then
            let outside = sq @ (file_a + file_h) in
            let defended = sq @ our_info.pawn_attacks in
            acc +$ Outpost.get outside defended
          else acc in
        (* Behind pawn. *)
        let acc = if sq @ adv then acc +$ behind_pawn else acc in
        (* Control center on long diagonal. *)
        let acc =
          let c = Pre.bishop sq pawn & center in
          if sq @ (longdiag - center) && several c
          then acc +$ long_diagonal else acc in
        (* Mobility. *)
        let acc =
          let c = count (our_info.mobility_area & att) in
          acc +$ uget mobility c in
        (* King safety. *)
        Info.update_king_safety their_info att Safety.bishop;
        acc)
end

module Rook = struct
  let open_file = 10 $ 9
  let semi_open_file = 34 $ 8
  let seventh_rank = -1 $ 42

  (* By number of attacked squares in our mobility area *)
  let mobility = [|
    -127 $ -148;
    -56 $ -127;
    -25 $ -85;
    -12 $ -28;
    -10 $ 2;
    -12 $ 27;
    -11 $ 42;
    -4 $ 46;
    4 $ 52;
    9 $ 55;
    11 $ 64;
    19 $ 68;
    19 $ 73;
    37 $ 60;
    97 $ 15;
  |]

  let[@inline] go (info : info) c =
    let open Bb in
    let c' = Piece.Color.opposite c in
    let our_info = Info.color info c in
    let their_info = Info.color info c' in
    let all = Position.all_board info.pos in
    let us = Position.board_of_color info.pos c in
    let them = all - us in
    let pawn = Position.pawn info.pos in
    let rook = Position.rook info.pos in
    let our_pawns = pawn & us in
    let our_rooks = rook & us in
    let their_pawns = pawn & them in
    let relr = relative_rank c in
    let kr6 = Int.(relr their_info.king_square >= Square.Rank.seven) in
    Bb.fold our_rooks ~init:(0 $ 0) ~f:(fun acc sq ->
        let att = Pre.rook sq our_info.no_rook in
        Info.update_attacks our_info att Rook;
        (* Open or semi-open file. *)
        let acc =
          let f = file_exn @@ Square.file sq in
          if (our_pawns & f) = empty then
            if (their_pawns & f) <> empty
            then acc +$ semi_open_file
            else acc +$ open_file
          else acc in
        (* Seventh rank. *)
        let acc =
          if kr6 && Int.((relr sq) = Square.Rank.seven)
          then acc +$ seventh_rank else acc in
        (* Mobility. *)
        let acc =
          let c = count (our_info.mobility_area & att) in
          acc +$ uget mobility c in
        (* King safety. *)
        Info.update_king_safety their_info att Safety.rook;
        acc)
end

module Queen = struct
  let discovery = -22 $ -13

  (* By number of attacked squares in our mobility area *)
  let mobility = [|
    -111 $ -273;
    -253 $ -401;
    -127 $ -228;
    -46 $ -236;
    -20 $ -173;
    -9 $ -86;
    -1 $ -35;
    2 $ -1;
    8 $ 8;
    10 $ 31;
    15 $ 37;
    17 $ 55;
    20 $ 46;
    23 $ 57;
    22 $ 58;
    21 $ 64;
    24 $ 62;
    16 $ 65;
    13 $ 63;
    18 $ 48;
    25 $ 30;
    38 $ 8;
    34 $ -12;
    28 $ -29;
    10 $ -44;
    7 $ -79;
    -42 $ -30;
    -23 $ -50;
  |]

  let[@inline] go (info : info) c =
    let open Bb in
    let c' = Piece.Color.opposite c in
    let our_info = Info.color info c in
    let their_info = Info.color info c' in
    let all = Position.all_board info.pos in
    let us = Position.board_of_color info.pos c in
    let them = all - us in
    let bishop = Position.bishop info.pos in
    let rook = Position.rook info.pos in
    let queen = Position.queen info.pos in
    let our_queens = queen & us in
    let their_bishops = bishop & them in
    let their_rooks = rook & them in
    Bb.fold our_queens ~init:(0 $ 0) ~f:(fun acc sq ->
        let att = Pre.queen sq all in
        Info.update_attacks our_info att Queen;
        (* Discovered attacks. *)
        let acc =
          (* Remove bishops and rooks that can be reached by the queen. *)
          let batt = Pre.bishop sq all in
          let ratt = Pre.rook sq all in
          let b = their_bishops - batt in
          let r = their_rooks - ratt in
          (* See if there is a second bishop or rook in the attack. *)
          if (b & Pre.bishop sq (all - batt)) <> empty
          || (r & Pre.rook   sq (all - ratt)) <> empty
          then acc +$ discovery else acc in
        (* Mobility. *)
        let acc =
          let c = count (our_info.mobility_area & att) in
          acc +$ uget mobility c in
        (* King safety. *)
        Info.update_king_safety their_info att Safety.queen;
        acc)
end

module Pawn_king = struct
  (* By king-pawn file distance. *)
  let proximity = [|
    36 $ 46;
    22 $ 31;
    13 $ 15;
    -8 $ -22;
    -5 $ -62;
    -3 $ -75;
    -15 $ -81;
    -12 $ -75;
  |]

  module Shelter = struct
    let t = [|
      (* Different file. *)
      (* A *)
      -5 $ -5;
      17 $ -31;
      26 $ -3;
      24 $ 8;
      4 $ 1;
      -12 $ 4;
      -16 $ -33;
      -59 $ 24;
      (* B *)
      11 $ -6;
      3 $ -15;
      -5 $ -2;
      5 $ -4;
      -11 $ 7;
      -53 $ 70;
      81 $ 82;
      -19 $ 1;
      (* C *)
      38 $ -3;
      5 $ -6;
      -34 $ 5;
      -17 $ -15;
      -9 $ -5;
      -26 $ 12;
      11 $ 73;
      -16 $ -1;
      (* D *)
      18 $ 11;
      25 $ -18;
      0 $ -14;
      10 $ -21;
      22 $ -34;
      -48 $ 9;
      -140 $ 49;
      -5 $ -5;
      (* E *)
      -11 $ 15;
      1 $ -3;
      -44 $ 6;
      -28 $ 10;
      -24 $ -2;
      -35 $ -5;
      40 $ -24;
      -13 $ 3;
      (* F *)
      51 $ -14;
      15 $ -14;
      -24 $ 5;
      -10 $ -20;
      10 $ -34;
      34 $ -20;
      48 $ -38;
      -21 $ 1;
      (* G *)
      40 $ -17;
      2 $ -24;
      -31 $ -1;
      -24 $ -8;
      -31 $ 2;
      -20 $ 29;
      4 $ 49;
      -16 $ 3;
      (* H *)
      10 $ -20;
      4 $ -24;
      10 $ 2;
      2 $ 16;
      -10 $ 24;
      -10 $ 44;
      -184 $ 81;
      -17 $ 17;
      (* Same file. *)
      (* A *)
      0 $ 0;
      -15 $ -39;
      9 $ -29;
      -49 $ 14;
      -36 $ 6;
      -8 $ 50;
      -168 $ -3;
      -59 $ 19;
      (* B *)
      0 $ 0;
      17 $ -18;
      9 $ -11;
      -11 $ -5;
      -1 $ -24;
      26 $ 73;
      -186 $ 4;
      -32 $ 11;
      (* C *)
      0 $ 0;
      19 $ -9;
      1 $ -11;
      9 $ -26;
      28 $ -5;
      -92 $ 56;
      -88 $ -74;
      -8 $ 1;
      (* D *)
      0 $ 0;
      0 $ 3;
      -6 $ -6;
      -35 $ 10;
      -46 $ 13;
      -98 $ 33;
      -7 $ -45;
      -35 $ -5;
      (* E *)
      0 $ 0;
      12 $ -3;
      17 $ -15;
      17 $ -15;
      -5 $ -14;
      -36 $ 5;
      -101 $ -52;
      -18 $ -1;
      (* F *)
      0 $ 0;
      -8 $ -5;
      -22 $ 1;
      -16 $ -6;
      25 $ -22;
      -27 $ 10;
      52 $ 39;
      -14 $ -2;
      (* G *)
      0 $ 0;
      32 $ -22;
      19 $ -15;
      -9 $ -6;
      -29 $ 13;
      -7 $ 23;
      -50 $ -39;
      -27 $ 18;
      (* H *)
      0 $ 0;
      16 $ -57;
      17 $ -32;
      -18 $ -7;
      -31 $ 24;
      -11 $ 24;
      -225 $ -49;
      -30 $ 5;
    |]

    let get same_file f r =
      let rc = Square.Rank.count in
      let fc = Square.File.count in
      let i = r + (f * rc) + (Bool.to_int same_file * fc * rc) in
      uget t i
  end

  module Storm = struct
    let t = [|
      (* Not blocked *)
      (* +0 *)
      -6 $ 36;
      144 $ -4;
      -13 $ 26;
      -7 $ 1;
      -12 $ -3;
      -8 $ -7;
      -19 $ 8;
      -28 $ -2;
      (* +1 *)
      -17 $ 60;
      64 $ 17;
      -9 $ 21;
      8 $ 12;
      3 $ 9;
      6 $ -2;
      -5 $ 2;
      -16 $ 8;
      (* +2 *)
      2 $ 48;
      15 $ 30;
      -17 $ 20;
      -13 $ 10;
      -1 $ 6;
      7 $ 3;
      8 $ -7;
      7 $ 8;
      (* +3 *)
      -1 $ 25;
      15 $ 22;
      -31 $ 10;
      -22 $ 1;
      -15 $ 4;
      13 $ -10;
      3 $ 5;
      -20 $ 8;
      (* Blocked *)
      (* +0 *)
      0 $ 0;
      -18 $ 16;
      -18 $ -2;
      27 $ -24;
      10 $ -6;
      15 $ -24;
      -6 $ 9;
      9 $ 30;
      (* +1 *)
      0 $ 0;
      -15 $ -24;
      -3 $ -15;
      53 $ -17;
      15 $ -5;
      20 $ -28;
      -12 $ -17;
      -34 $ 5;
      (* +2 *)
      0 $ 0;
      -34 $ -62;
      -15 $ -13;
      9 $ -6;
      6 $ -2;
      -2 $ -17;
      -5 $ -21;
      -3 $ 3;
      (* +3 *)
      0 $ 0;
      -1 $ -26;
      -27 $ -18;
      -21 $ 4;
      -10 $ -6;
      7 $ -35;
      66 $ -29;
      11 $ 25;
    |]

    let get blocked file rank =
      let rc = Square.Rank.count in
      let mf = Square.File.mirror_exn file in
      let i = rank + (mf * rc) + (Bool.to_int blocked * 4 * rc) in
      uget t i
  end

  let surrounding_files f =
    let fa = Int.(max 0 (f - 1)) in
    let fb = Int.(min (Square.File.count - 1) (f + 1)) in
    Sequence.range fa fb ~stop:`inclusive

  let[@inline] go (info : info) c =
    let open Bb in
    let our_info = Info.color info c in
    let all = Position.all_board info.pos in
    let us = Position.board_of_color info.pos c in
    let them = all - us in
    let pawn = Position.pawn info.pos in
    let our_pawns = pawn & us in
    let their_pawns = pawn & them in
    let kr, kf = Square.decomp our_info.king_square in
    let fwd = Pre.forward_ranks our_info.king_square c in
    let bmost = Fn.compose Square.rank @@ backmost c in
    (* Proximity of king to nearest file-wise pawn. *)
    let dist = Pre.king_pawn_file_distance our_info.king_square pawn in
    our_info.pawn_king_eval <- our_info.pawn_king_eval +$ uget proximity dist;
    (* Look at the files of and surrounding our king. *)
    surrounding_files kf |> Sequence.iter ~f:(fun f ->
        let eval = our_info.pawn_king_eval in
        let safety = our_info.pawn_king_safety in
        (* Closest firendly and enemy pawns at or above the king
           on this file. *)
        let fb = file_exn f in
        let ours = our_pawns & fb & fwd in
        let theirs = their_pawns & fb & fwd in
        (* Rank-wise distance between these pawns and our king. If the pawns
           are missing then a distance of 7 is used, since if there were a
           pawn it would be strictly less than 7. *)
        let od = if ours   <> empty then abs Int.(kr - bmost ours)   else 7 in
        let td = if theirs <> empty then abs Int.(kr - bmost theirs) else 7 in
        (* Shelter. *)
        let same_file = Int.(f = kf) in
        let eval = eval +$ Shelter.get same_file f od in
        let safety = safety +$ Safety.Shelter.get same_file od in
        (* Storm. *)
        let blocked = Int.(od <> 7 && (od = td - 1)) in
        let eval = eval +$ Storm.get blocked f td in
        let safety = safety +$ Safety.Storm.get blocked td in
        (* Update. *)
        our_info.pawn_king_eval <- eval;
        our_info.pawn_king_safety <- safety)
end

module King = struct
  (* By number of defenders (pawn + minor) in our king's area. *)
  let defenders = [|
    -37 $ -3;
    -17 $ 2;
    0 $ 6;
    11 $ 8;
    21 $ 8;
    32 $ 0;
    38 $ -14;
    10 $ -5;
    12 $ 6;
    12 $ 6;
    12 $ 6;
    12 $ 6;
  |]

  let finalize s =
    let mg = Score.mg s and eg = Score.eg s in
    (-mg * max 0 mg / 720) $ (-(max 0 eg) / 20)

  let[@inline] go (info : info) c =
    let open Bb in
    let c' = Piece.Color.opposite c in
    let our_info = Info.color info c in
    let their_info = Info.color info c' in
    let all = Position.all_board info.pos in
    let us = Position.board_of_color info.pos c in
    let them = all - us in
    let pawn = Position.pawn info.pos in
    let knight = Position.knight info.pos in
    let bishop = Position.bishop info.pos in
    let queen = Position.queen info.pos in
    let their_queens = queen & them in
    (* Defenders. *)
    let eval =
      let b = (pawn + knight + bishop) & us in
      let d = count (b & our_info.king_area) in
      uget defenders d in
    (* Ignore when we have less than two attackers and no potential for
       a queen attacker. *)
    if Int.(our_info.num_king_attackers > 1 - count their_queens) then
      let na, na2 = ~~(our_info.attacks), ~~(our_info.attacks2) in
      (* Weak squares. *)
      let weak =
        let aq = Info.attacked_by our_info Queen in
        let ak = Info.attacked_by our_info King in
        their_info.attacks & na2 & (na + aq + ak) in
      (* Scale the number of attacks on the king by the size of the king
         area, which is typically 9 squares. *)
      let scaled_attacks =
        let n = 9.0 *. Float.of_int our_info.num_king_attacks in
        let d = Float.of_int @@ count our_info.king_area in
        Float.to_int (n /. d) in
      (* Squares that are safe for the enemy pieces. *)
      let safe = ~~them & (na + (weak & their_info.attacks2)) in
      (* Potential checking squares. *)
      let nthreats = Pre.knight our_info.king_square in
      let bthreats = Pre.bishop our_info.king_square all in
      let rthreats = Pre.rook   our_info.king_square all in
      let qthreats = bthreats + rthreats in
      let nchecks  = nthreats & safe & Info.attacked_by their_info Knight in
      let bchecks  = bthreats & safe & Info.attacked_by their_info Bishop in
      let rchecks  = rthreats & safe & Info.attacked_by their_info Rook   in
      let qchecks  = qthreats & safe & Info.attacked_by their_info Queen  in
      (* Safety. *)
      let base = our_info.king_attackers +$ our_info.pawn_king_safety in
      let scale =
        (Safety.attack_value      * scaled_attacks) +$
        (Safety.weak_squares      * count (weak & our_info.king_area)) +$
        (Safety.no_enemy_queens   * Bool.to_int (their_queens = empty)) +$
        (Safety.safe_queen_check  * count qchecks) +$
        (Safety.safe_rook_check   * count rchecks) +$
        (Safety.safe_bishop_check * count bchecks) +$
        (Safety.safe_knight_check * count nchecks) in
      eval +$ finalize (base +$ scale +$ Safety.adjustment)
    else eval
end

module Passed = struct
  module Rank = struct
    let t = [|
      (* Cannot advance, unsafe. *)
      0 $ 0;
      -39 $ -4;
      -43 $ 25;
      -62 $ 28;
      8 $ 19;
      97 $ -4;
      162 $ 46;
      0 $ 0;
      (* Cannot advance, safe. *)
      0 $ 0;
      -28 $ 13;
      -40 $ 42;
      -56 $ 44;
      -2 $ 56;
      114 $ 54;
      193 $ 94;
      0 $ 0;
      (* Can advance, unsafe. *)
      0 $ 0;
      -28 $ 29;
      -47 $ 36;
      -60 $ 54;
      8 $ 65;
      106 $ 76;
      258 $ 124;
      0 $ 0;
      (* Can advance, safe. *)
      0 $ 0;
      -28 $ 23;
      -40 $ 35;
      -55 $ 60;
      8 $ 89;
      95 $ 166;
      124 $ 293;
      0 $ 0;
    |]

    let get can_advance safe_advance rank =
      let rc = Square.Rank.count in
      let c = Bool.to_int can_advance in
      let s = Bool.to_int safe_advance in
      let i = rank + (rc * s) + (rc * 2 * c) in
      uget t i
  end

  module Distance = struct
    (* By distance from king (using the Chebyshev method). *)
    let ours = [|
      0 $ 0;
      -3 $ 1;
      0 $ -4;
      5 $ -13;
      6 $ -19;
      -9 $ -19;
      -9 $ -7;
      0 $ 0;
    |]

    (* By distance from king (using the Chebyshev method). *)
    let theirs = [|
      0 $ 0;
      5 $ -1;
      7 $ 0;
      9 $ 11;
      0 $ 25;
      1 $ 37;
      16 $ 37;
      0 $ 0;
    |]
  end

  let promotion = -49 $ 56

  let[@inline] go (info : info) c =
    let open Bb in
    let c' = Piece.Color.opposite c in
    let our_info = Info.color info c in
    let their_info = Info.color info c' in
    let all = Position.all_board info.pos in
    let us = Position.board_of_color info.pos c in
    let them = all - us in
    let pawn = Position.pawn info.pos in
    let our_pawns = pawn & us in
    let relr = relative_rank c in
    let fwd = square_fwd c in
    Bb.fold our_pawns ~init:(0 $ 0) ~f:(fun acc sq ->
        let r = relr sq and b = !!(fwd sq) in
        (* Rank. *)
        let acc =
          let can_advance = (b & all) = empty in
          let safe_advance = (b & their_info.attacks) = empty in
          Rank.get can_advance safe_advance r in
        (* Ignore additional passers. *)
        if not (several (Pre.forward_files sq c & our_info.passed_pawns)) then
          (* Distance from our king. *)
          let acc =
            let d = Square.chebyshev sq our_info.king_square in
            acc +$ uget Distance.ours d in
          (* Distance from their king. *)
          let acc =
            let d = Square.chebyshev sq their_info.king_square in
            acc +$ uget Distance.theirs d in
          (* Promotion. *)
          let acc =
            let f = Square.file sq in
            let b = Pre.forward_ranks sq c & file_exn f in
            if (b & (them + their_info.attacks)) = empty
            then acc +$ promotion else acc in
          acc
        else acc)
end

module Threat = struct
  let weak_pawn = -11 $ -38
  let pawn_minor = -55 $ -83
  let minor_minor = -25 $ -45
  let major_minor = -30 $ -55
  let pawn_minor_rook = -48 $ -28
  let king_minor = -43 $ -21
  let king_rook = -33 $ -18
  let any_queen = -50 $ -7
  let overload = -7 $ -16
  let pawn_push = 15 $ 32

  let[@inline] go (info : info) c =
    let open Bb in
    let c' = Piece.Color.opposite c in
    let our_info = Info.color info c in
    let their_info = Info.color info c' in
    let na = ~~(our_info.attacks) in
    let na2 = ~~(our_info.attacks2) in
    let na' = ~~(their_info.attacks) in
    let na2' = ~~(their_info.attacks2) in
    let p = Info.attacked_by our_info Pawn in
    let all = Position.all_board info.pos in
    let us = Position.board_of_color info.pos c in
    let them = all - us in
    let pawn = Position.pawn info.pos in
    let knight = Position.knight info.pos in
    let bishop = Position.bishop info.pos in
    let rook = Position.rook info.pos in
    let queen = Position.queen info.pos in
    let our_minors = (knight + bishop) & us in
    (* Enemy attacks. *)
    let pawn_att   = Info.attacked_by their_info Pawn   in
    let knight_att = Info.attacked_by their_info Knight in
    let bishop_att = Info.attacked_by their_info Bishop in
    let rook_att   = Info.attacked_by their_info Rook   in
    let queen_att  = Info.attacked_by their_info Queen  in
    let king_att   = Info.attacked_by their_info King   in
    let minor_att  = knight_att + bishop_att in
    let major_att  = rook_att   + queen_att  in
    (* More attackers, few defenders, and no pawn support. *)
    let poorly_defended =
      (their_info.attacks & na) +
      (their_info.attacks2 & na2 & ~~p) in
    let weak_minors = our_minors & poorly_defended in
    (* Attacked and defended by exactly one. *)
    let overloaded =
      ((knight + bishop + rook + queen) & us) &
      our_info.attacks & na2 & their_info.attacks & na2' in
    (* New threats from a pawn advance. *)
    let np' = ~~pawn_att in
    let push_threat =
      let adv = pawn_advance c in
      (* Discard squares that are already theatened by our pawns. *)
      let m = ~~(them - p) in
      (* Single pawn advance. *)
      let b = adv (pawn & us) - all in
      (* Double pawn advance if we're at the third rank and we're
         not countered by an enemy pawn. *)
      let b = b + (adv (b & np' & third_rank c) - all) in
      (* Remove pawn counter-threats. *)
      let b = b & np' in
      (* Focus on squares that are not attacked or can be defended. *)
      let b = b & (na' + our_info.attacks) in
      (* Get the pieces threatened by the pawn push. *)
      let l, r = pawn_captures b c in
      (l + r) - m in
    (* Combine the features. *)
    (weak_pawn       * count (pawn & us & np' & poorly_defended))      +$
    (pawn_minor      * count (our_minors & pawn_att))                  +$
    (minor_minor     * count (our_minors & minor_att))                 +$
    (major_minor     * count (weak_minors & major_att))                +$
    (pawn_minor_rook * count (rook & us & (pawn_att + minor_att)))     +$
    (king_minor      * count (weak_minors & king_att))                 +$
    (king_rook       * count (rook & us & poorly_defended & king_att)) +$
    (any_queen       * count (queen & us & their_info.attacks))        +$
    (overload        * count overloaded)                               +$
    (pawn_push       * count push_threat)
end

module Space = struct
  let restrict_occupied = -4 $ -1
  let restrict_empty = -4 $ -2
  let center_control = 3 $ 0

  let[@inline] go (info : info) c =
    let open Bb in
    let acc = 0 $ 0 in
    let c' = Piece.Color.opposite c in
    let our_info = Info.color info c in
    let their_info = Info.color info c' in
    let all = Position.all_board info.pos in
    let us = Position.board_of_color info.pos c in
    let uncontrolled =
      their_info.attacks2 &
      our_info.attacks -
      our_info.attacks2 -
      Info.attacked_by our_info Pawn in
    (* Occupied squares. *)
    let acc = acc +$ (restrict_occupied * count (uncontrolled & all)) in
    (* Empty squares. *)
    let acc = acc +$ (restrict_empty * count (uncontrolled - all)) in
    (* Uncontested central squares. *)
    let acc =
      let knight = Position.knight info.pos in
      let bishop = Position.bishop info.pos in
      let rook = Position.rook info.pos in
      let queen = Position.queen info.pos in
      let minor = count (knight + bishop) in
      let major = count (rook + queen) in
      if Int.((minor + 2 * major) > 12) then
        let b = ~~(their_info.attacks) & (our_info.attacks + us) & bigcenter in
        acc +$ (center_control * count b)
      else acc in
    acc
end

module Closedness = struct
  (* By closedness factor. *)
  let knights = [|
    -7 $ 10;
    -7 $ 29;
    -9 $ 37;
    -5 $ 37;
    -3 $ 44;
    -1 $ 36;
    1 $ 33;
    -10 $ 51;
    -7 $ 30;
  |]

  (* By closedness factor. *)
  let rooks = [|
    42 $ 43;
    -6 $ 80;
    3 $ 59;
    -5 $ 47;
    -7 $ 41;
    -3 $ 23;
    -6 $ 11;
    -17 $ 11;
    -34 $ -12;
  |]

  let open_files b =
    let open Bb in
    let b = b + (b >> 8) in
    let b = b + (b >> 16) in
    let b = b + (b >> 32) in
    count (of_int64 0xFFL - b)

  let[@inline] go (info : info) =
    let open Bb in
    let acc = 0 $ 0 in
    let w = Position.white info.pos in
    let b = Position.white info.pos in
    let pawn = Position.pawn info.pos in
    let closedness =
      let open Int in
      let n =
        1 * count pawn +
        3 * count info.white.rammed_pawns -
        4 * open_files pawn in
      max 0 (min 8 (n / 3)) in
    (* Knights. *)
    let acc =
      let knight = Position.knight info.pos in
      let wn = count (knight & w) in
      let bn = count (knight & b) in
      let weight = uget knights closedness in
      acc +$ (weight * Int.(wn - bn)) in
    (* Rooks. *)
    let acc =
      let rook = Position.rook info.pos in
      let wr = count (rook & w) in
      let br = count (rook & b) in
      let weight = uget rooks closedness in
      acc +$ (weight * Int.(wr - br)) in
    acc
end

module Complexity = struct
  let total_pawns = 0 $ 8
  let pawn_flanks = 0 $ 82
  let pawn_endgame = 0 $ 76
  let adjustment = 0 $ -157

  let[@inline] go pos eval =
    let open Bb in
    let pawn = Position.pawn pos in
    let both_flanks =
      (pawn & (file_a + file_b + file_c + file_d)) <> empty &&
      (pawn & (file_e + file_f + file_g + file_h)) <> empty in
    let npw = Position.has_non_pawn_material pos White in
    let npb = Position.has_non_pawn_material pos Black in
    let complexity =
      (total_pawns * count pawn) +$
      (pawn_flanks * Bool.to_int both_flanks) +$
      (pawn_endgame * Bool.to_int (not (npw || npb))) +$
      adjustment in
    let eg = Score.eg eval in
    let s = Sign.to_int @@ Int.sign eg in
    0 $ Int.(s * max (Score.eg complexity) (-(abs eg)))
end

module Scale = struct
  let (>>?) x f = match x with
    | None -> f ()
    | Some x -> x

  module Opposite_bishops = struct
    let one_knight = 106
    let one_rook = 96
    let only_bishops = 64

    let[@inline] go w b knight bishop rook queen =
      let open Bb.Syntax in
      if Bb.count (w & bishop) = 1
      && Bb.count (b & bishop) = 1
      && Bb.count (bishop & Bb.white) = 1 then
        if Bb.((rook + queen) = empty)
        && Bb.count (w & knight) = 1
        && Bb.count (b & knight) = 1 then
          Some one_knight
        else if Bb.((knight + queen) = empty)
             && Bb.count (w & rook) = 1
             && Bb.count (b & rook) = 1 then
          Some one_rook
        else if Bb.((knight + rook + queen) = empty) then
          Some only_bishops
        else None
      else None
  end

  module Lone_queen = struct
    let scale = 88

    let[@inline] go knight bishop rook queen weak =
      let open Bb.Syntax in
      let pieces = knight + bishop + rook in
      if Bb.count queen = 1
      && Bb.several pieces
      && Bb.(pieces = (weak & pieces))
      then Some scale else None
  end

  module Lone_minor = struct
    let scale = 0

    let[@inline] go knight bishop strong =
      if Bb.((strong & (knight + bishop)) <> empty)
      && Bb.count strong = 2 then Some scale else None
  end

  module Lone_vs_pawns = struct
    let scale = 144

    let[@inline] go w b pawn knight bishop rook queen weak strong =
      let open Bb in
      let pieces = knight + bishop + rook in
      if queen = empty
      && not (several (pieces & w))
      && not (several (pieces & b))
      && Int.(count (strong & pawn) - count (weak & pawn) > 2)
      then Some scale else None
  end

  let normal = 128

  let[@inline] go pos eval =
    let pawn = Position.pawn pos in
    let knight = Position.knight pos in
    let bishop = Position.bishop pos in
    let rook = Position.rook pos in
    let queen = Position.queen pos in
    let w = Position.white pos in
    let b = Position.black pos in
    let weak = if Score.eg eval < 0 then w else b in
    let strong = if Score.eg eval < 0 then b else w in
    Opposite_bishops.go w b knight bishop rook queen >>? fun () ->
    Lone_queen.go knight bishop rook queen weak >>? fun () ->
    Lone_minor.go knight bishop strong >>? fun () ->
    Lone_vs_pawns.go w b pawn knight bishop rook queen weak strong >>? fun () ->
    min normal (96 + Bb.(count (pawn & strong)) * 8)
end

module Material = struct
  let pawn = 82 $ 114
  let knight = 427 $ 475
  let bishop = 441 $ 510
  let rook = 627 $ 803
  let queen = 1292 $ 1623
  let king = 0 $ 0
end

module Psqt = struct
  (* By square. *)
  let pawn = [|
    (* 1 *)
    0 $ 0;
    0 $ 0;
    0 $ 0;
    0 $ 0;
    0 $ 0;
    0 $ 0;
    0 $ 0;
    0 $ 0;
    (* 2 *)
    -13 $ 7;
    -4 $ 0;
    1 $ 4;
    6 $ 1;
    3 $ 10;
    -9 $ 4;
    -9 $ 3;
    -16 $ 7;
    (* 3 *)
    -21 $ 5;
    -17 $ 6;
    -1 $ -6;
    12 $ -14;
    8 $ -10;
    -4 $ -5;
    -15 $ 7;
    -24 $ 11;
    (* 4 *)
    -14 $ 16;
    -21 $ 17;
    9 $ -10;
    10 $ -24;
    4 $ -22;
    4 $ -10;
    -20 $ 17;
    -17 $ 18;
    (* 5 *)
    -15 $ 18;
    -18 $ 11;
    -16 $ -8;
    4 $ -30;
    -2 $ -24;
    -18 $ -9;
    -23 $ 13;
    -17 $ 21;
    (* 6 *)
    -20 $ 48;
    -9 $ 44;
    1 $ 31;
    17 $ -9;
    36 $ -6;
    -9 $ 31;
    -6 $ 45;
    -23 $ 49;
    (* 7 *)
    -33 $ -70;
    -66 $ -9;
    -16 $ -22;
    65 $ -23;
    41 $ -18;
    39 $ -14;
    -47 $ 4;
    -62 $ -51;
    (* 8 *)
    0 $ 0;
    0 $ 0;
    0 $ 0;
    0 $ 0;
    0 $ 0;
    0 $ 0;
    0 $ 0;
    0 $ 0;
  |]

  (* By square. *)
  let knight = [|
    (* 1 *)
    -31 $ -38;
    -6 $ -24;
    -20 $ -22;
    -16 $ -1;
    -11 $ -1;
    -22 $ -19;
    -8 $ -20;
    -41 $ -30;
    (* 2 *)
    1 $ -5;
    -11 $ 3;
    -6 $ -19;
    -1 $ -2;
    0 $ 0;
    -9 $ -16;
    -8 $ -3;
    -6 $ 1;
    (* 3 *)
    7 $ -21;
    8 $ -5;
    7 $ 2;
    10 $ 19;
    10 $ 19;
    4 $ 2;
    8 $ -4;
    3 $ -19;
    (* 4 *)
    16 $ 21;
    17 $ 30;
    23 $ 41;
    27 $ 50;
    24 $ 53;
    23 $ 41;
    19 $ 28;
    13 $ 26;
    (* 5 *)
    13 $ 30;
    23 $ 30;
    37 $ 51;
    30 $ 70;
    26 $ 67;
    38 $ 50;
    22 $ 33;
    14 $ 28;
    (* 6 *)
    -24 $ 25;
    -5 $ 37;
    25 $ 56;
    22 $ 60;
    27 $ 55;
    29 $ 55;
    -1 $ 32;
    -19 $ 25;
    (* 7 *)
    13 $ -2;
    -11 $ 18;
    27 $ -2;
    37 $ 24;
    41 $ 24;
    40 $ -7;
    -13 $ 16;
    2 $ -2;
    (* 8 *)
    -167 $ -5;
    -91 $ 12;
    -117 $ 41;
    -38 $ 17;
    -18 $ 19;
    -105 $ 48;
    -119 $ 24;
    -165 $ -17;
  |]

  (* By square. *)
  let bishop = [|
    (* 1 *)
    5 $ -21;
    1 $ 1;
    -1 $ 5;
    1 $ 5;
    2 $ 8;
    -6 $ -2;
    0 $ 1;
    4 $ -25;
    (* 2 *)
    26 $ -17;
    2 $ -31;
    15 $ -2;
    8 $ 8;
    8 $ 8;
    13 $ -3;
    9 $ -31;
    26 $ -29;
    (* 3 *)
    9 $ 3;
    22 $ 9;
    -5 $ -3;
    18 $ 19;
    17 $ 20;
    -5 $ -6;
    20 $ 4;
    15 $ 8;
    (* 4 *)
    0 $ 12;
    10 $ 17;
    17 $ 32;
    20 $ 32;
    24 $ 34;
    12 $ 30;
    15 $ 17;
    0 $ 14;
    (* 5 *)
    -20 $ 34;
    13 $ 31;
    1 $ 38;
    21 $ 45;
    12 $ 46;
    6 $ 38;
    13 $ 33;
    -14 $ 37;
    (* 6 *)
    -13 $ 31;
    -11 $ 45;
    -7 $ 23;
    2 $ 40;
    8 $ 38;
    -21 $ 34;
    -5 $ 46;
    -9 $ 35;
    (* 7 *)
    -59 $ 38;
    -49 $ 22;
    -13 $ 30;
    -35 $ 36;
    -33 $ 36;
    -13 $ 33;
    -68 $ 21;
    -55 $ 35;
    (* 8 *)
    -66 $ 18;
    -65 $ 36;
    -123 $ 48;
    -107 $ 56;
    -112 $ 53;
    -97 $ 43;
    -33 $ 22;
    -74 $ 15;
  |]

  (* By square. *)
  let rook = [|
    (* 1 *)
    -26 $ -1;
    -21 $ 3;
    -14 $ 4;
    -6 $ -4;
    -5 $ -4;
    -10 $ 3;
    -13 $ -2;
    -22 $ -14;
    (* 2 *)
    -70 $ 5;
    -25 $ -10;
    -18 $ -7;
    -11 $ -11;
    -9 $ -13;
    -15 $ -15;
    -15 $ -17;
    -77 $ 3;
    (* 3 *)
    -39 $ 3;
    -16 $ 14;
    -25 $ 9;
    -14 $ 2;
    -12 $ 3;
    -25 $ 8;
    -4 $ 9;
    -39 $ 1;
    (* 4 *)
    -32 $ 24;
    -21 $ 36;
    -21 $ 36;
    -5 $ 26;
    -8 $ 27;
    -19 $ 34;
    -13 $ 33;
    -30 $ 24;
    (* 5 *)
    -22 $ 46;
    4 $ 38;
    16 $ 38;
    35 $ 30;
    33 $ 32;
    10 $ 36;
    17 $ 31;
    -14 $ 43;
    (* 6 *)
    -33 $ 60;
    17 $ 41;
    0 $ 54;
    33 $ 36;
    29 $ 35;
    3 $ 52;
    33 $ 32;
    -26 $ 56;
    (* 7 *)
    -18 $ 41;
    -24 $ 47;
    -1 $ 38;
    15 $ 38;
    14 $ 37;
    -2 $ 36;
    -24 $ 49;
    -12 $ 38;
    (* 8 *)
    33 $ 55;
    24 $ 63;
    -1 $ 73;
    9 $ 66;
    10 $ 67;
    0 $ 69;
    34 $ 59;
    37 $ 56;
  |]

  (* By square. *)
  let queen = [|
    (* 1 *)
    20 $ -34;
    4 $ -26;
    9 $ -34;
    17 $ -16;
    18 $ -18;
    14 $ -46;
    9 $ -28;
    22 $ -44;
    (* 2 *)
    6 $ -15;
    15 $ -22;
    22 $ -42;
    13 $ 2;
    17 $ 0;
    22 $ -49;
    18 $ -29;
    3 $ -18;
    (* 3 *)
    6 $ -1;
    21 $ 7;
    5 $ 35;
    0 $ 34;
    2 $ 34;
    5 $ 37;
    24 $ 9;
    13 $ -15;
    (* 4 *)
    9 $ 17;
    12 $ 46;
    -6 $ 59;
    -19 $ 109;
    -17 $ 106;
    -4 $ 57;
    18 $ 48;
    8 $ 33;
    (* 5 *)
    -10 $ 42;
    -8 $ 79;
    -19 $ 66;
    -32 $ 121;
    -32 $ 127;
    -23 $ 80;
    -8 $ 95;
    -10 $ 68;
    (* 6 *)
    -28 $ 56;
    -23 $ 50;
    -33 $ 66;
    -18 $ 70;
    -17 $ 71;
    -19 $ 63;
    -18 $ 65;
    -28 $ 76;
    (* 7 *)
    -16 $ 61;
    -72 $ 108;
    -19 $ 65;
    -52 $ 114;
    -54 $ 120;
    -14 $ 59;
    -69 $ 116;
    -11 $ 73;
    (* 8 *)
    8 $ 43;
    19 $ 47;
    0 $ 79;
    3 $ 78;
    -3 $ 89;
    13 $ 65;
    18 $ 79;
    21 $ 56;
  |]

  (* By square. *)
  let king = [|
    (* 1 *)
    87 $ -77;
    67 $ -49;
    4 $ -7;
    -9 $ -26;
    -10 $ -27;
    -8 $ -1;
    57 $ -50;
    79 $ -82;
    (* 2 *)
    35 $ 3;
    -27 $ -3;
    -41 $ 16;
    -89 $ 29;
    -64 $ 26;
    -64 $ 28;
    -25 $ -3;
    30 $ -4;
    (* 3 *)
    -44 $ -19;
    -16 $ -19;
    28 $ 7;
    0 $ 35;
    18 $ 32;
    31 $ 9;
    -13 $ -18;
    -36 $ -13;
    (* 4 *)
    -48 $ -44;
    98 $ -39;
    71 $ 12;
    -22 $ 45;
    12 $ 41;
    79 $ 10;
    115 $ -34;
    -59 $ -38;
    (* 5 *)
    -6 $ -10;
    95 $ -39;
    39 $ 14;
    -49 $ 18;
    -27 $ 19;
    35 $ 14;
    81 $ -34;
    -50 $ -13;
    (* 6 *)
    24 $ -39;
    123 $ -22;
    105 $ -1;
    -22 $ -21;
    -39 $ -20;
    74 $ -15;
    100 $ -23;
    -17 $ -49;
    (* 7 *)
    0 $ -98;
    28 $ -21;
    7 $ -18;
    -3 $ -41;
    -57 $ -39;
    12 $ -26;
    22 $ -24;
    -15 $ -119;
    (* 8 *)
    -16 $ -153;
    49 $ -94;
    -21 $ -73;
    -19 $ -32;
    -51 $ -55;
    -42 $ -62;
    53 $ -93;
    -58 $ -133;
  |]

  let idx p sq =
    Square.to_int sq + (Piece.to_int p * Square.count)

  let tbl =
    let open Piece in
    let len = Color.count * Kind.count * Square.count in
    let t = Array.create ~len (0 $ 0) in
    for i = 0 to Square.last do
      let sq = Square.of_int_exn i in
      let wsq = relative_square sq White in
      t.(idx white_pawn   sq) <- +Material.pawn   + pawn.(wsq);
      t.(idx white_knight sq) <- +Material.knight + knight.(wsq);
      t.(idx white_bishop sq) <- +Material.bishop + bishop.(wsq);
      t.(idx white_rook   sq) <- +Material.rook   + rook.(wsq);
      t.(idx white_queen  sq) <- +Material.queen  + queen.(wsq);
      t.(idx white_king   sq) <- +Material.king   + king.(wsq);
      let bsq = relative_square sq Black in
      t.(idx black_pawn   sq) <- -Material.pawn   - pawn.(bsq);
      t.(idx black_knight sq) <- -Material.knight - knight.(bsq);
      t.(idx black_bishop sq) <- -Material.bishop - bishop.(bsq);
      t.(idx black_rook   sq) <- -Material.rook   - rook.(bsq);
      t.(idx black_queen  sq) <- -Material.queen  - queen.(bsq);
      t.(idx black_king   sq) <- -Material.king   - king.(bsq);
    done;
    t

  let go pos =
    Position.collect_all pos |>
    List.fold ~init:(0 $ 0) ~f:(fun acc (sq, p) ->
        acc +$ uget tbl (idx p sq))
end

(* Game phase (i.e. middle or endgame). *)

module Phase = struct
  let knight_phase = 1
  let bishop_phase = 1
  let rook_phase   = 2
  let queen_phase  = 4

  let total_phase  =
    knight_phase * 4 +
    bishop_phase * 4 +
    rook_phase   * 4 +
    queen_phase  * 2

  let[@inline] weighted_count pos k p =
    p * (Bb.count @@ Position.board_of_kind pos k)

  (* Determine the current phase weight of the game based on the
     available material. *)
  let[@inline] weight pos =
    weighted_count pos Knight knight_phase +
    weighted_count pos Bishop bishop_phase +
    weighted_count pos Rook   rook_phase   +
    weighted_count pos Queen  queen_phase

  (* Interpolate between middle and endgame scores. *)
  let[@inline] interpolate pos eval =
    let scale = Scale.go pos eval in
    let w = weight pos in
    let mg = Score.mg eval * w in
    let eg = Score.eg eval * (total_phase - w) * scale / Scale.normal in
    (mg + eg) / total_phase
end

(* The evaluation function. *)

let tempo = 20

let go pos =
  let eval = 0 $ 0 in
  let info = Info.create pos in
  if Option.is_none info.pkentry then begin
    Pawn.go info White;
    Pawn.go info Black;
    Pawn_king.go info White;
    Pawn_king.go info Black;
  end;
  let eval = eval + (Knight.go info White - Knight.go info Black) in
  let eval = eval + (Bishop.go info White - Bishop.go info Black) in
  let eval = eval + (Rook.go   info White - Rook.go   info Black) in
  let eval = eval + (Queen.go  info White - Queen.go  info Black) in
  let eval = eval + (King.go   info White - King.go   info Black) in
  let eval = eval + (Passed.go info White - Passed.go info Black) in
  let eval = eval + (Threat.go info White - Threat.go info Black) in
  let eval = eval + (Space.go  info White - Space.go  info Black) in
  let eval = eval + Info.pawn_king_eval info in
  let eval = eval + Psqt.go pos in
  let eval = eval + Closedness.go info in
  let eval = eval + Complexity.go pos eval in
  Info.save_pkentry info;
  let score = Phase.interpolate pos eval in
  tempo + match Position.active pos with
  | Piece.White -> score
  | Piece.Black -> -score
