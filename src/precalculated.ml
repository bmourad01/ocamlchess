open Core_kernel

(* Magic shift constants. *)
module Shift = struct
  let diagonal = [|
    6; 5; 5; 5; 5; 5; 5; 6; 5; 5; 5; 5; 5; 5; 5; 5;
    5; 5; 7; 7; 7; 7; 5; 5; 5; 5; 7; 9; 9; 7; 5; 5;
    5; 5; 7; 9; 9; 7; 5; 5; 5; 5; 7; 7; 7; 7; 5; 5;
    5; 5; 5; 5; 5; 5; 5; 5; 6; 5; 5; 5; 5; 5; 5; 6;
  |]

  let straight = [|
    12; 11; 11; 11; 11; 11; 11; 12; 11; 10; 10; 10; 10; 10; 10; 11;
    11; 10; 10; 10; 10; 10; 10; 11; 11; 10; 10; 10; 10; 10; 10; 11;
    11; 10; 10; 10; 10; 10; 10; 11; 11; 10; 10; 10; 10; 10; 10; 11;
    11; 10; 10; 10; 10; 10; 10; 11; 12; 11; 11; 11; 11; 11; 11; 12;
  |]
end

(* Generated by:

   https://www.chessprogramming.org/index.php?title=Looking_for_Magics#Feeding_in_Randoms
   with USE_32_BIT_MULTIPLICATIONS undefined

   Credit: Tord Romstad *)
module Magic = struct
  let bishop = [|
    0x89A1121896040240L; 0x2004844802002010L; 0x2068080051921000L;
    0x62880A0220200808L; 0x0004042004000000L; 0x0100822020200011L;
    0xC00444222012000AL; 0x0028808801216001L; 0x0400492088408100L;
    0x0201C401040C0084L; 0x00840800910A0010L; 0x0000082080240060L;
    0x2000840504006000L; 0x30010C4108405004L; 0x1008005410080802L;
    0x8144042209100900L; 0x0208081020014400L; 0x004800201208CA00L;
    0x0F18140408012008L; 0x1004002802102001L; 0x0841000820080811L;
    0x0040200200A42008L; 0x0000800054042000L; 0x88010400410C9000L;
    0x0520040470104290L; 0x1004040051500081L; 0x2002081833080021L;
    0x000400C00C010142L; 0x941408200C002000L; 0x0658810000806011L;
    0x0188071040440A00L; 0x4800404002011C00L; 0x0104442040404200L;
    0x0511080202091021L; 0x0004022401120400L; 0x80C0040400080120L;
    0x8040010040820802L; 0x0480810700020090L; 0x0102008E00040242L;
    0x0809005202050100L; 0x8002024220104080L; 0x0431008804142000L;
    0x0019001802081400L; 0x0200014208040080L; 0x3308082008200100L;
    0x041010500040C020L; 0x4012020C04210308L; 0x208220A202004080L;
    0x0111040120082000L; 0x6803040141280A00L; 0x2101004202410000L;
    0x8200000041108022L; 0x0000021082088000L; 0x0002410204010040L;
    0x0040100400809000L; 0x0822088220820214L; 0x0040808090012004L;
    0x00910224040218C9L; 0x0402814422015008L; 0x0090014004842410L;
    0x0001000042304105L; 0x0010008830412A00L; 0x2520081090008908L;
    0x40102000A0A60140L;
  |]

  let rook = [|
    0x0A8002C000108020L; 0x06C00049B0002001L; 0x0100200010090040L;
    0x2480041000800801L; 0x0280028004000800L; 0x0900410008040022L;
    0x0280020001001080L; 0x2880002041000080L; 0xA000800080400034L;
    0x0004808020004000L; 0x2290802004801000L; 0x0411000D00100020L;
    0x0402800800040080L; 0x000B000401004208L; 0x2409000100040200L;
    0x0001002100004082L; 0x0022878001E24000L; 0x1090810021004010L;
    0x0801030040200012L; 0x0500808008001000L; 0x0A08018014000880L;
    0x8000808004000200L; 0x0201008080010200L; 0x0801020000441091L;
    0x0000800080204005L; 0x1040200040100048L; 0x0000120200402082L;
    0x0D14880480100080L; 0x0012040280080080L; 0x0100040080020080L;
    0x9020010080800200L; 0x0813241200148449L; 0x0491604001800080L;
    0x0100401000402001L; 0x4820010021001040L; 0x0400402202000812L;
    0x0209009005000802L; 0x0810800601800400L; 0x4301083214000150L;
    0x204026458E001401L; 0x0040204000808000L; 0x8001008040010020L;
    0x8410820820420010L; 0x1003001000090020L; 0x0804040008008080L;
    0x0012000810020004L; 0x1000100200040208L; 0x430000A044020001L;
    0x0280009023410300L; 0x00E0100040002240L; 0x0000200100401700L;
    0x2244100408008080L; 0x0008000400801980L; 0x0002000810040200L;
    0x8010100228810400L; 0x2000009044210200L; 0x4080008040102101L;
    0x0040002080411D01L; 0x2005524060000901L; 0x0502001008400422L;
    0x489A000810200402L; 0x0001004400080A13L; 0x4000011008020084L;
    0x0026002114058042L;
  |]                                       
end

(* Simple movement patterns which can be wholly precalculated with no
   parameters. *)
module Simple = struct
  (* Construct a simple table which maps squares to bitboards. *)
  let make f = Array.init Square.count ~f:(fun i ->
      let open Square in
      let sq = of_int_exn i in
      f (rank sq) (file sq) |>
      List.filter_map ~f:(fun (rank, file) -> create ~rank ~file) |>
      List.fold ~init:Bitboard.empty ~f:Bitboard.set)

  let white_pawn_advance = make @@ fun rank file -> [rank + 1, file]
  let black_pawn_advance = make @@ fun rank file -> [rank - 1, file]

  let white_pawn_capture = make @@ fun rank file -> [
      rank + 1, file + 1;
      rank + 1, file - 1;
    ]

  let black_pawn_capture = make @@ fun rank file -> [
      rank - 1, file + 1;
      rank - 1, file - 1
    ]

  let knight = make @@ fun rank file -> [
      rank + 2, file + 1;
      rank - 2, file + 1;
      rank + 2, file - 1;
      rank - 2, file - 1;
      rank + 1, file + 2;
      rank - 1, file + 2;
      rank + 1, file - 2;
      rank - 1, file - 2;
    ]

  let king = make @@ fun rank file -> [
      rank,     file + 1;
      rank,     file - 1;
      rank + 1, file + 1;
      rank + 1, file - 1;
      rank - 1, file + 1;
      rank - 1, file - 1;
      rank + 1, file;
      rank - 1, file;
    ]
end

(* Masks for various movement directions. *)
module Mask = struct
  (* Direction to move in when starting from a particular square. *)
  let dir r f =
    Simple.make @@ fun rank file ->
    List.init ((Square.count lsr 3) - 1) ~f:(fun i -> r rank (i + 1), f file (i + 1))

  (* All 8 directions. *)
  let east = dir const (+)
  and west = dir const (-)
  and north = dir (+) const
  and south = dir (-) const
  and neast = dir (+) (+)
  and nwest = dir (+) (-)
  and seast = dir (-) (+)
  and swest = dir (-) (-)

  (* Combine all diagonal directions, minus the edges. *)
  let diagonal = Array.init Square.count ~f:(fun i -> Bitboard.(
      neast.(i) + nwest.(i) + seast.(i) + swest.(i) - edges))

  (* Combine all straight directions, minus the edges. *)
  let straight = Array.init Square.count ~f:(fun i -> Bitboard.(
      (east.(i)  - file_h) +
      (west.(i)  - file_a) +
      (north.(i) - rank_8) +
      (south.(i) - rank_1)))
end

(* Generation of sliding attack patterns. *)
module Sliding = struct
  (* Computes the bitboards for diagonal and straight attacks, given a
     starting square and the set of occupied squares. *)
  let diagonal, straight =
    let gen arr i occupied =
      let open Bitboard in
      let occupied = of_int64 occupied in
      Array.map arr ~f:(fun (tbl, f) ->
          let b = tbl.(i) in
          let j = match to_int64 (b & occupied) with
            | 0L -> None
            | b' -> Some (f b') in
          b, j) |>
      Array.foldi ~init:empty ~f:(fun i acc (b, j) ->
          let acc = acc + b in
          Option.value_map j ~default:acc
            ~f:(fun j -> acc - (fst arr.(i)).(j))) in
    let l b = Square.last - Int64.clz b in
    let r = Int64.ctz in
    Mask.(gen [|(neast, r); (nwest, r); (seast, l); (swest, l)|],
          gen [|(east,  r); (west,  l); (north, r); (south, l)|])

  (* Generate the occupied squares for a particular mask and index. *)
  let blockers idx mask =
    let mask = Bitboard.to_int64 mask in
    Int64.popcount mask |> Array.init ~f:ident |> Array.fold ~init:(0L, mask)
      ~f:(fun (blockers, mask) i ->
          let blockers =
            if idx land (1 lsl i) = 0 then blockers
            else Int64.(blockers lor (one lsl ctz mask)) in
          blockers, Int64.(mask land pred mask)) |> fst

  (* Compute the index into the magic hash table. *)
  let hash occupied magic shift =
    Int64.((occupied * magic) lsr Int.(64 - shift) |> to_int_exn)

  (* Generate the magic hash table for bishop and rook moves. *)
  let bishop, rook =
    let go len shift mask magic gen =
      let tbl = Array.init Square.count
          ~f:(fun _ -> Array.create ~len Bitboard.empty) in
      for i = 0 to Square.count - 1 do
        let shift = shift.(i) and mask = mask.(i)
        and magic = magic.(i) and t = tbl.(i) in
        for j = 0 to 1 lsl shift do
          let occupied = blockers j mask in
          t.(hash occupied magic shift) <- gen i occupied
        done
      done;
      tbl in
    go 1024 Shift.diagonal Mask.diagonal Magic.bishop diagonal,
    go 4096 Shift.straight Mask.straight Magic.rook straight
end

(* The actual API for accessing precalculated move patterns. *)

let pawn_advance sq color = match color with
  | Piece.White -> Simple.white_pawn_advance.(Square.to_int sq)
  | Piece.Black -> Simple.black_pawn_advance.(Square.to_int sq)

let pawn_capture sq color = match color with
  | Piece.White -> Simple.white_pawn_capture.(Square.to_int sq)
  | Piece.Black -> Simple.black_pawn_capture.(Square.to_int sq)

let knight sq = Simple.knight.(Square.to_int sq)

let bishop sq occupied =
  let i = Square.to_int sq in
  let occupied = Bitboard.(to_int64 (occupied & Mask.diagonal.(i))) in
  Sliding.(bishop.(i).(hash occupied Magic.bishop.(i) Shift.diagonal.(i)))

let rook sq occupied =
  let i = Square.to_int sq in
  let occupied = Bitboard.(to_int64 (occupied & Mask.straight.(i))) in
  Sliding.(rook.(i).(hash occupied Magic.rook.(i) Shift.straight.(i)))

let queen sq occupied = Bitboard.(bishop sq occupied + rook sq occupied)
let king sq = Simple.king.(Square.to_int sq)

let castle =
  let open Bitboard in
  let open Castling_rights in
  let valid = [
    Piece.White, `king,  Square.(!!f1 + !!g1);
    Piece.White, `queen, Square.(!!c1 + !!d1);
    Piece.Black, `king,  Square.(!!f8 + !!g8);
    Piece.Black, `queen, Square.(!!c8 + !!d8);
  ] in
  let tbl = Array.init (1 lsl bits) ~f:(fun i ->
      let x = of_int_exn i in
      List.fold valid ~init:empty ~f:(fun b (c, s, b') ->
          if mem x c s then b + b' else b)) in
  fun rights c s -> tbl.(to_int @@ inter rights @@ singleton c s)

module Between = struct
  (* XXX: clean this up *)
  let tbl =
    Array.init Square.count ~f:(fun sq ->
        let sq = Square.of_int_exn sq in
        let rank, file = Square.decomp sq in
        Array.init Square.count ~f:(fun sq' ->
            let sq' = Square.of_int_exn sq' in
            let rank', file' = Square.decomp sq' in
            let rec east i acc =
              let file = file + i in
              if file > 7 then Bitboard.empty
              else if file = file' then acc
              else east (i + 1) @@
                Bitboard.set acc (Square.create_exn ~rank ~file) in
            let rec west i acc =
              let file = file - i in
              if file < 0 then Bitboard.empty
              else if file = file' then acc
              else west (i + 1) @@
                Bitboard.set acc (Square.create_exn ~rank ~file) in
            let rec north i acc =
              let rank = rank + i in
              if rank > 7 then Bitboard.empty
              else if rank = rank' then acc
              else north (i + 1) @@
                Bitboard.set acc (Square.create_exn ~rank ~file) in
            let rec south i acc =
              let rank = rank - i in
              if rank < 0 then Bitboard.empty
              else if rank = rank' then acc
              else south (i + 1) @@
                Bitboard.set acc (Square.create_exn ~rank ~file) in
            let rec northeast i acc =
              let rank = rank + i and file = file + i in
              if rank > 7 || file > 7 then Bitboard.empty
              else if rank = rank' && file = file' then acc
              else northeast (i + 1) @@
                Bitboard.set acc (Square.create_exn ~rank ~file) in
            let rec southeast i acc =
              let rank = rank - i and file = file + i in
              if rank < 0 || file > 7 then Bitboard.empty
              else if rank = rank' && file = file' then acc
              else southeast (i + 1) @@
                Bitboard.set acc (Square.create_exn ~rank ~file) in
            let rec northwest i acc =
              let rank = rank + i and file = file - i in
              if rank > 7 || file < 0 then Bitboard.empty
              else if rank = rank' && file = file' then acc
              else northwest (i + 1) @@
                Bitboard.set acc (Square.create_exn ~rank ~file) in
            let rec southwest i acc =
              let rank = rank - i and file = file - i in
              if rank < 0 || file < 0 then Bitboard.empty
              else if rank = rank' && file = file' then acc
              else southwest (i + 1) @@
                Bitboard.set acc (Square.create_exn ~rank ~file) in
            Bitboard.(east 1 empty +
                      west 1 empty +
                      north 1 empty +
                      south 1 empty +
                      northeast 1 empty +
                      southeast 1 empty +
                      northwest 1 empty +
                      southwest 1 empty)))
end

let between sq sq' =
  let i = Square.to_int sq and i' = Square.to_int sq' in
  Between.tbl.(i).(i')
