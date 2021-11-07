open Core_kernel

(* Magic shift constants. *)

module Shift = struct
  let diagonal =
    [| 6; 5; 5; 5; 5; 5; 5; 6; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 7; 7; 7; 7; 5; 5
     ; 5; 5; 7; 9; 9; 7; 5; 5; 5; 5; 7; 9; 9; 7; 5; 5; 5; 5; 7; 7; 7; 7; 5; 5
     ; 5; 5; 5; 5; 5; 5; 5; 5; 6; 5; 5; 5; 5; 5; 5; 6 |]

  let straight =
    [| 12; 11; 11; 11; 11; 11; 11; 12; 11; 10; 10; 10; 10; 10; 10; 11; 11; 10
     ; 10; 10; 10; 10; 10; 11; 11; 10; 10; 10; 10; 10; 10; 11; 11; 10; 10; 10
     ; 10; 10; 10; 11; 11; 10; 10; 10; 10; 10; 10; 11; 11; 10; 10; 10; 10; 10
     ; 10; 11; 12; 11; 11; 11; 11; 11; 11; 12 |]
end

(* Generated by:

   https://www.chessprogramming.org/index.php?title=Looking_for_Magics#Feeding_in_Randoms
   with USE_32_BIT_MULTIPLICATIONS undefined

   Credit: Tord Romstad *)

module Magic = struct
  let bishop =
    [| 0x89A1121896040240L; 0x2004844802002010L; 0x2068080051921000L
     ; 0x62880A0220200808L; 0x0004042004000000L; 0x0100822020200011L
     ; 0xC00444222012000AL; 0x0028808801216001L; 0x0400492088408100L
     ; 0x0201C401040C0084L; 0x00840800910A0010L; 0x0000082080240060L
     ; 0x2000840504006000L; 0x30010C4108405004L; 0x1008005410080802L
     ; 0x8144042209100900L; 0x0208081020014400L; 0x004800201208CA00L
     ; 0x0F18140408012008L; 0x1004002802102001L; 0x0841000820080811L
     ; 0x0040200200A42008L; 0x0000800054042000L; 0x88010400410C9000L
     ; 0x0520040470104290L; 0x1004040051500081L; 0x2002081833080021L
     ; 0x000400C00C010142L; 0x941408200C002000L; 0x0658810000806011L
     ; 0x0188071040440A00L; 0x4800404002011C00L; 0x0104442040404200L
     ; 0x0511080202091021L; 0x0004022401120400L; 0x80C0040400080120L
     ; 0x8040010040820802L; 0x0480810700020090L; 0x0102008E00040242L
     ; 0x0809005202050100L; 0x8002024220104080L; 0x0431008804142000L
     ; 0x0019001802081400L; 0x0200014208040080L; 0x3308082008200100L
     ; 0x041010500040C020L; 0x4012020C04210308L; 0x208220A202004080L
     ; 0x0111040120082000L; 0x6803040141280A00L; 0x2101004202410000L
     ; 0x8200000041108022L; 0x0000021082088000L; 0x0002410204010040L
     ; 0x0040100400809000L; 0x0822088220820214L; 0x0040808090012004L
     ; 0x00910224040218C9L; 0x0402814422015008L; 0x0090014004842410L
     ; 0x0001000042304105L; 0x0010008830412A00L; 0x2520081090008908L
     ; 0x40102000A0A60140L |]

  let rook =
    [| 0x0A8002C000108020L; 0x06C00049B0002001L; 0x0100200010090040L
     ; 0x2480041000800801L; 0x0280028004000800L; 0x0900410008040022L
     ; 0x0280020001001080L; 0x2880002041000080L; 0xA000800080400034L
     ; 0x0004808020004000L; 0x2290802004801000L; 0x0411000D00100020L
     ; 0x0402800800040080L; 0x000B000401004208L; 0x2409000100040200L
     ; 0x0001002100004082L; 0x0022878001E24000L; 0x1090810021004010L
     ; 0x0801030040200012L; 0x0500808008001000L; 0x0A08018014000880L
     ; 0x8000808004000200L; 0x0201008080010200L; 0x0801020000441091L
     ; 0x0000800080204005L; 0x1040200040100048L; 0x0000120200402082L
     ; 0x0D14880480100080L; 0x0012040280080080L; 0x0100040080020080L
     ; 0x9020010080800200L; 0x0813241200148449L; 0x0491604001800080L
     ; 0x0100401000402001L; 0x4820010021001040L; 0x0400402202000812L
     ; 0x0209009005000802L; 0x0810800601800400L; 0x4301083214000150L
     ; 0x204026458E001401L; 0x0040204000808000L; 0x8001008040010020L
     ; 0x8410820820420010L; 0x1003001000090020L; 0x0804040008008080L
     ; 0x0012000810020004L; 0x1000100200040208L; 0x430000A044020001L
     ; 0x0280009023410300L; 0x00E0100040002240L; 0x0000200100401700L
     ; 0x2244100408008080L; 0x0008000400801980L; 0x0002000810040200L
     ; 0x8010100228810400L; 0x2000009044210200L; 0x4080008040102101L
     ; 0x0040002080411D01L; 0x2005524060000901L; 0x0502001008400422L
     ; 0x489A000810200402L; 0x0001004400080A13L; 0x4000011008020084L
     ; 0x0026002114058042L |]
end

(* Total number of possible squares. *)
let ncoord = 1 lsl Square.bits

(* Construct a simple table which maps squares to bitboards. *)
let make_simple () =
  let tbl = Array.create ~len:ncoord Bitboard.empty in
  let add i rank file =
    Square.of_rank_and_file ~rank ~file
    |> Option.iter ~f:(fun sq -> tbl.(i) <- Bitboard.(tbl.(i) <-- sq))
  in
  (tbl, add)

(* Pawns, knights, and kings have simple movement patterns, which we can
   store the entirety of. *)

let white_pawn_moves =
  let tbl, add = make_simple () in
  for rank = 0 to 7 do
    for file = 0 to 7 do
      let i = Square.of_rank_and_file_exn ~rank ~file |> Square.to_int in
      add i (rank + 1) (file + 1);
      add i (rank + 1) (file + 1)
    done
  done;
  tbl

let black_pawn_moves =
  let tbl, add = make_simple () in
  for rank = 0 to 7 do
    for file = 0 to 7 do
      let i = Square.of_rank_and_file_exn ~rank ~file |> Square.to_int in
      add i (rank - 1) (file + 1);
      add i (rank - 1) (file - 1)
    done
  done;
  tbl

let knight_moves =
  let tbl, add = make_simple () in
  for rank = 0 to 7 do
    for file = 0 to 7 do
      let i = Square.of_rank_and_file_exn ~rank ~file |> Square.to_int in
      add i (rank + 2) (file + 1);
      add i (rank - 2) (file + 1);
      add i (rank + 2) (file - 1);
      add i (rank - 2) (file - 1);
      add i (rank + 1) (file + 2);
      add i (rank - 1) (file + 2);
      add i (rank + 1) (file - 2);
      add i (rank - 1) (file - 2)
    done
  done;
  tbl

let king_moves =
  let tbl, add = make_simple () in
  for rank = 0 to 7 do
    for file = 0 to 7 do
      let i = Square.of_rank_and_file_exn ~rank ~file |> Square.to_int in
      add i rank (file + 1);
      add i rank (file - 1);
      add i (rank + 1) (file + 1);
      add i (rank + 1) (file - 1);
      add i (rank - 1) (file + 1);
      add i (rank - 1) (file - 1);
      add i (rank + 1) file;
      add i (rank - 1) file
    done
  done;
  tbl

(* Masks for various movement directions. *)

module Mask = struct
  (* The edges of the board. *)
  module Edge = struct
    let rank_1 =
      Array.init 8 ~f:ident
      |> Array.fold ~init:Bitboard.empty ~f:(fun b i ->
             Bitboard.(b <-- Square.of_rank_and_file_exn ~rank:0 ~file:i) )

    let rank_8 =
      Array.init 8 ~f:ident
      |> Array.fold ~init:Bitboard.empty ~f:(fun b i ->
             Bitboard.(b <-- Square.of_rank_and_file_exn ~rank:7 ~file:i) )

    let file_a =
      Array.init 8 ~f:ident
      |> Array.fold ~init:Bitboard.empty ~f:(fun b i ->
             Bitboard.(b <-- Square.of_rank_and_file_exn ~rank:i ~file:0) )

    let file_h =
      Array.init 8 ~f:ident
      |> Array.fold ~init:Bitboard.empty ~f:(fun b i ->
             Bitboard.(b <-- Square.of_rank_and_file_exn ~rank:i ~file:7) )

    let edges = Bitboard.(rank_1 + rank_8 + file_a + file_h)
  end

  (* Direction to move in when starting from a particular square. *)
  let dir r f =
    let tbl, add = make_simple () in
    for rank = 0 to 7 do
      for file = 0 to 7 do
        let i = Square.of_rank_and_file_exn ~rank ~file |> Square.to_int in
        for j = 1 to 7 do
          add i (r rank j) (f file j)
        done
      done
    done;
    tbl

  (* All 8 directions. *)

  let east = dir const ( + )

  let west = dir const ( - )

  let north = dir ( + ) const

  let south = dir ( - ) const

  let northeast = dir ( + ) ( + )

  let northwest = dir ( + ) ( - )

  let southeast = dir ( - ) ( + )

  let southwest = dir ( - ) ( - )

  (* Combine all diagonal directions, minus the edges. *)
  let diagonal =
    let tbl = Array.create ~len:ncoord Bitboard.empty in
    for rank = 0 to 7 do
      for file = 0 to 7 do
        let i = Square.of_rank_and_file_exn ~rank ~file |> Square.to_int in
        let ne = northeast.(i) in
        let nw = northwest.(i) in
        let se = southeast.(i) in
        let sw = southwest.(i) in
        tbl.(i) <- Bitboard.(ne + nw + se + sw - Edge.edges)
      done
    done;
    tbl

  (* Combine all straight directions, minus the edges. *)
  let straight =
    let tbl = Array.create ~len:ncoord Bitboard.empty in
    for rank = 0 to 7 do
      for file = 0 to 7 do
        let i = Square.of_rank_and_file_exn ~rank ~file |> Square.to_int in
        let e = east.(i) in
        let w = west.(i) in
        let n = north.(i) in
        let s = south.(i) in
        tbl.(i) <-
          Bitboard.(
            e - Edge.file_h + (w - Edge.file_a) + (n - Edge.rank_8)
            + (s - Edge.rank_1))
      done
    done;
    tbl
end

(* Compute the bitboard of attacking squares for a diagonal move, given the
   set of occupied squares. *)
let diagonal_attacks i occupied =
  let open Mask in
  let occupied = Bitboard.of_int64 occupied in
  let ne = northeast.(i) in
  let nw = northwest.(i) in
  let se = southeast.(i) in
  let sw = southwest.(i) in
  let result = ne in
  let result =
    match Bitboard.((ne & occupied) |> to_int64) with
    | 0L -> result
    | b ->
        let j = Int64.ctz b in
        Bitboard.(result - northeast.(j))
  in
  let result = Bitboard.(result + nw) in
  let result =
    match Bitboard.((nw & occupied) |> to_int64) with
    | 0L -> result
    | b ->
        let j = Int64.ctz b in
        Bitboard.(result - northwest.(j))
  in
  let result = Bitboard.(result + se) in
  let result =
    match Bitboard.((se & occupied) |> to_int64) with
    | 0L -> result
    | b ->
        let j = 63 - Int64.clz b in
        Bitboard.(result - southeast.(j))
  in
  let result = Bitboard.(result + sw) in
  let result =
    match Bitboard.((sw & occupied) |> to_int64) with
    | 0L -> result
    | b ->
        let j = 63 - Int64.clz b in
        Bitboard.(result - southwest.(j))
  in
  result

(* Compute the bitboard of attacking squares for a straight move, given the
   set of occupied squares. *)
let straight_attacks i occupied =
  let open Mask in
  let occupied = Bitboard.of_int64 occupied in
  let e = east.(i) in
  let w = west.(i) in
  let n = north.(i) in
  let s = south.(i) in
  let result = e in
  let result =
    match Bitboard.((e & occupied) |> to_int64) with
    | 0L -> result
    | b ->
        let j = Int64.ctz b in
        Bitboard.(result - east.(j))
  in
  let result = Bitboard.(result + w) in
  let result =
    match Bitboard.((w & occupied) |> to_int64) with
    | 0L -> result
    | b ->
        let j = 63 - Int64.clz b in
        Bitboard.(result - west.(j))
  in
  let result = Bitboard.(result + n) in
  let result =
    match Bitboard.((n & occupied) |> to_int64) with
    | 0L -> result
    | b ->
        let j = Int64.ctz b in
        Bitboard.(result - north.(j))
  in
  let result = Bitboard.(result + s) in
  let result =
    match Bitboard.((s & occupied) |> to_int64) with
    | 0L -> result
    | b ->
        let j = 63 - Int64.clz b in
        Bitboard.(result - south.(j))
  in
  result

(* Generate the occupied squares for a particular mask and index. *)
let blockers idx mask =
  let mask = Bitboard.to_int64 mask in
  Int64.popcount mask |> Array.init ~f:ident
  |> Array.fold ~init:(0L, mask) ~f:(fun (blockers, mask) i ->
         let j = Int64.ctz mask in
         let mask = Int64.(mask land pred mask) in
         let blockers =
           if idx land (1 lsl i) = 0 then blockers
           else Int64.(blockers lor (1L lsl j))
         in
         (blockers, mask) )
  |> fst

(* Compute the index into the magic hash table. *)
let hash_key occupied magic shift =
  let shift = 64 - shift in
  Int64.((occupied * magic) lsr shift |> to_int_exn)

(* Generate the magic hash table for bishop moves. *)
let bishop_moves =
  let tbl =
    Array.init ncoord ~f:(fun _ -> Array.create ~len:1024 Bitboard.empty)
  in
  for rank = 0 to 7 do
    for file = 0 to 7 do
      let i = Square.of_rank_and_file_exn ~rank ~file |> Square.to_int in
      let shift = Shift.diagonal.(i) in
      let mask = Mask.diagonal.(i) in
      let magic = Magic.bishop.(i) in
      let t = tbl.(i) in
      for idx = 0 to 1 lsl shift do
        let occupied = blockers idx mask in
        t.(hash_key occupied magic shift) <- diagonal_attacks i occupied
      done
    done
  done;
  tbl

(* Generate the magic hash table for rook moves. *)
let rook_moves =
  let tbl =
    Array.init ncoord ~f:(fun _ -> Array.create ~len:4096 Bitboard.empty)
  in
  for rank = 0 to 7 do
    for file = 0 to 7 do
      let i = Square.of_rank_and_file_exn ~rank ~file |> Square.to_int in
      let shift = Shift.straight.(i) in
      let mask = Mask.straight.(i) in
      let magic = Magic.rook.(i) in
      let t = tbl.(i) in
      for idx = 0 to 1 lsl shift do
        let occupied = blockers idx mask in
        t.(hash_key occupied magic shift) <- straight_attacks i occupied
      done
    done
  done;
  tbl

let pawn sq (color : Piece.color) =
  match color with
  | White -> white_pawn_moves.(Square.to_int sq)
  | Black -> black_pawn_moves.(Square.to_int sq)

let knight sq = knight_moves.(Square.to_int sq)

let bishop sq occupied =
  let occupied = Bitboard.to_int64 occupied in
  let i = Square.to_int sq in
  bishop_moves.(i).(hash_key occupied Magic.bishop.(i) Shift.diagonal.(i))

let rook sq occupied =
  let occupied = Bitboard.to_int64 occupied in
  let i = Square.to_int sq in
  rook_moves.(i).(hash_key occupied Magic.rook.(i) Shift.straight.(i))

let queen sq occupied = Bitboard.(bishop sq occupied + rook sq occupied)

let king sq = king_moves.(Square.to_int sq)

let white_queenside_castle = Bitboard.(empty <-- Square.c1 <-- Square.d1)

let white_kingside_castle = Bitboard.(empty <-- Square.f1 <-- Square.g1)

let black_queenside_castle = Bitboard.(empty <-- Square.c8 <-- Square.d8)

let black_kingside_castle = Bitboard.(empty <-- Square.f8 <-- Square.g8)

let castle (color : Piece.color) side =
  match (color, side) with
  | White, `queen -> white_queenside_castle
  | White, `king -> white_kingside_castle
  | Black, `queen -> black_queenside_castle
  | Black, `king -> black_kingside_castle
