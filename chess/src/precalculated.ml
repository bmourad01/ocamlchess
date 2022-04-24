open Core_kernel

module Bb = Bitboard
module Cr = Castling_rights

(* Magic shift constants. *)

module Shift = struct
  let diagonal = [|
    6; 5; 5; 5; 5; 5; 5; 6;
    5; 5; 5; 5; 5; 5; 5; 5;
    5; 5; 7; 7; 7; 7; 5; 5;
    5; 5; 7; 9; 9; 7; 5; 5;
    5; 5; 7; 9; 9; 7; 5; 5;
    5; 5; 7; 7; 7; 7; 5; 5;
    5; 5; 5; 5; 5; 5; 5; 5;
    6; 5; 5; 5; 5; 5; 5; 6;
  |]

  let orthogonal = [|
    12; 11; 11; 11; 11; 11; 11; 12;
    11; 10; 10; 10; 10; 10; 10; 11;
    11; 10; 10; 10; 10; 10; 10; 11;
    11; 10; 10; 10; 10; 10; 10; 11;
    11; 10; 10; 10; 10; 10; 10; 11;
    11; 10; 10; 10; 10; 10; 10; 11;
    11; 10; 10; 10; 10; 10; 10; 11;
    12; 11; 11; 11; 11; 11; 11; 12;
  |]
end

(* Generated by:

   https://www.chessprogramming.org/index.php?title=Looking_for_Magics#Feeding_in_Randoms
   with USE_32_BIT_MULTIPLICATIONS undefined

   Credit: Tord Romstad
*)

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
      let sq = of_int_unsafe i in
      f (rank sq) (file sq) |>
      List.filter_map ~f:(fun (rank, file) -> create ~rank ~file) |>
      List.fold ~init:Bb.empty ~f:Bb.set)

  let white_pawn_advance = make @@ fun rank file -> [rank + 1, file]
  let black_pawn_advance = make @@ fun rank file -> [rank - 1, file]

  let white_pawn_capture = make @@ fun rank file -> [
      rank + 1, file + 1;
      rank + 1, file - 1;
    ]

  let black_pawn_capture = make @@ fun rank file -> [
      rank - 1, file + 1;
      rank - 1, file - 1;
    ]

  let pawn_advance = Array.append white_pawn_advance black_pawn_advance
  let pawn_capture = Array.append white_pawn_capture black_pawn_capture

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
    Simple.make @@ fun rank file -> List.init ((Square.count lsr 3) - 1)
      ~f:(fun i -> r rank (i + 1), f file (i + 1))

  module Tbl = struct
    (* All 8 directions. *)
    let east  = dir const (+)
    and west  = dir const (-)
    and north = dir (+) const
    and south = dir (-) const
    and neast = dir (+) (+)
    and nwest = dir (+) (-)
    and seast = dir (-) (+)
    and swest = dir (-) (-)

    (* Combine all diagonal directions, minus the edges. *)
    let diagonal = Array.init Square.count ~f:(fun i -> Bb.(
        neast.(i) + nwest.(i) + seast.(i) + swest.(i) - edges))

    (* Combine all orthogonal directions, minus the edges. *)
    let orthogonal = Array.init Square.count ~f:(fun i -> Bb.(
        (east.(i)  - file_h) +
        (west.(i)  - file_a) +
        (north.(i) - rank_8) +
        (south.(i) - rank_1)))
  end

  let east  sq = Array.unsafe_get Tbl.east  @@ Square.to_int sq
  let west  sq = Array.unsafe_get Tbl.west  @@ Square.to_int sq
  let north sq = Array.unsafe_get Tbl.north @@ Square.to_int sq
  let south sq = Array.unsafe_get Tbl.south @@ Square.to_int sq
  let neast sq = Array.unsafe_get Tbl.neast @@ Square.to_int sq
  let nwest sq = Array.unsafe_get Tbl.nwest @@ Square.to_int sq
  let seast sq = Array.unsafe_get Tbl.seast @@ Square.to_int sq
  let swest sq = Array.unsafe_get Tbl.swest @@ Square.to_int sq
end

(* Generation of sliding attack patterns. *)

module Sliding = struct
  let diagonal, orthogonal =
    (* Computes the bitboards for diagonal and orthogonal attacks, given a
       starting square and the set of occupied squares. *)
    let gen dirs i occupied =
      let open Bb in
      let occupied = of_int64 occupied in
      List.fold dirs ~init:empty ~f:(fun acc (tbl, f) ->
          let ray = tbl.(i) in
          let acc = acc + ray in
          match to_int64 (ray & occupied) with
          | 0L -> acc
          | ray' -> acc - tbl.(f ray')) in
    let l b = Square.last - Int64.clz b in
    let r = Int64.ctz in
    Mask.Tbl.(gen [(neast, r); (nwest, r); (seast, l); (swest, l)],
              gen [(east,  r); (west,  l); (north, r); (south, l)])

  (* Generate the occupied squares for a particular mask and index. *)
  let blockers idx mask =
    let n = Bb.count mask in
    let blockers = ref 0L in
    let mask = ref @@ Bb.to_int64 mask in
    for i = 0 to n - 1 do
      let m = !mask in
      if idx land (1 lsl i) <> 0 then
        blockers := Int64.(!blockers lor (one lsl ctz m));
      mask := Int64.(m land pred m);
    done;
    !blockers

  (* Compute the index into the hash table. *)
  let[@inline] hash occupied magic shift =
    Int64.((occupied * magic) lsr Int.(64 - shift) |> to_int_trunc)

  (* Populate the hash tables for bishop and rook moves. *)
  let bishop, rook =
    let go len shift mask magic gen =
      let tbl = Array.create Bb.empty ~len:(Square.count * len) in
      for i = 0 to Square.count - 1 do
        let shift = shift.(i) in
        let mask  = mask.(i)  in
        let magic = magic.(i) in
        for j = 0 to 1 lsl shift do
          let occupied = blockers j mask in
          let hash = hash occupied magic shift in
          tbl.(i + hash * Square.count) <- gen i occupied
        done
      done;
      tbl in
    go 1024 Shift.diagonal Mask.Tbl.diagonal Magic.bishop diagonal,
    go 4096 Shift.orthogonal Mask.Tbl.orthogonal Magic.rook orthogonal
end

(* The actual API for accessing precalculated move patterns. *)

let[@inline] pawn_advance sq c =
  let i = Square.to_int sq + Piece.Color.to_int c * Square.count in
  Array.unsafe_get Simple.pawn_advance i

let[@inline] pawn_capture sq c =
  let i = Square.to_int sq + Piece.Color.to_int c * Square.count in
  Array.unsafe_get Simple.pawn_capture i

let[@inline] knight sq = Array.unsafe_get Simple.knight @@ Square.to_int sq

let[@inline] bishop sq occupied =
  let i = Square.to_int sq in
  let mask = Array.unsafe_get Mask.Tbl.diagonal i in
  let shift = Array.unsafe_get Shift.diagonal i in
  let magic = Array.unsafe_get Magic.bishop i in
  let occupied = Bb.(to_int64 (occupied & mask)) in
  let hash = Sliding.hash occupied magic shift in
  Array.unsafe_get Sliding.bishop (i + hash * Square.count)

let[@inline] rook sq occupied =
  let i = Square.to_int sq in
  let mask = Array.unsafe_get Mask.Tbl.orthogonal i in
  let shift = Array.unsafe_get Shift.orthogonal i in
  let magic = Array.unsafe_get Magic.rook i in
  let occupied = Bb.(to_int64 (occupied & mask)) in
  let hash = Sliding.hash occupied magic shift in
  Array.unsafe_get Sliding.rook (i + hash * Square.count)

let[@inline] queen sq occupied = Bb.(bishop sq occupied + rook sq occupied)
let[@inline] king sq = Array.unsafe_get Simple.king @@ Square.to_int sq

let castle_tbl =
  let open Bb in
  let open Cr in
  let wk = Square.(!!f1 + !!g1) in
  let wq = Square.(!!c1 + !!d1) in
  let bk = Square.(!!f8 + !!g8) in
  let bq = Square.(!!c8 + !!d8) in
  let valid = [
    Piece.White, Kingside,  wk;
    Piece.White, Queenside, wq;
    Piece.Black, Kingside,  bk;
    Piece.Black, Queenside, bq;
  ] in
  Array.init (1 lsl bits) ~f:(fun i ->
      let x = of_int_unsafe i in
      List.fold valid ~init:empty ~f:(fun b (c, s, b') ->
          if mem x c s then b + b' else b))

let[@inline] castle cr c s =
  Cr.(to_int @@ inter cr @@ singleton_unsafe c s) |> Array.unsafe_get castle_tbl

let between_tbl =
  let tbl = Array.create Bb.empty ~len:Square.(count * count) in
  for i = 0 to Square.count - 1 do
    let sq = Square.of_int_unsafe i in
    for j = 0 to Square.count - 1 do
      let open Bb in
      (* Use the singleton bitboard of the target square as the
         blocker mask. *)
      let s = !!Square.(of_int_unsafe j) in
      let orthogonal m =
        let b = rook sq s & m in
        if (b & s) <> empty then b - s else empty in
      let diagonal m =
        let b = bishop sq s & m in
        if (b & s) <> empty then b - s else empty in
      (* Use the pre-generated directional masks. *)
      let east  = orthogonal Mask.Tbl.east.(i)  in
      let west  = orthogonal Mask.Tbl.west.(i)  in
      let north = orthogonal Mask.Tbl.north.(i) in
      let south = orthogonal Mask.Tbl.south.(i) in
      let neast = diagonal   Mask.Tbl.neast.(i) in
      let nwest = diagonal   Mask.Tbl.nwest.(i) in
      let seast = diagonal   Mask.Tbl.seast.(i) in
      let swest = diagonal   Mask.Tbl.swest.(i) in
      (* We're getting the union of all directions, even though only
         one of them will be valid. *)
      tbl.(Int.(i + j * Square.count)) <-
        east + west + north + south + neast + nwest + seast + swest
    done
  done;
  tbl

let[@inline] between sq1 sq2 =
  let i = Square.to_int sq1 in
  let j = Square.to_int sq2 in
  Array.unsafe_get between_tbl (i + j * Square.count)

let mvv_lva =
  let victims = Piece.[Pawn; Knight; Bishop; Rook; Queen] in
  let attackers = Piece.King :: List.rev victims in
  let num_attackers = List.length attackers in
  let num_victims = List.length victims in
  let tbl = Array.create ~len:(num_victims * num_attackers) 0 in
  List.fold victims ~init:0 ~f:(fun acc victim ->
      let i = Piece.Kind.to_int victim in
      List.fold attackers ~init:acc ~f:(fun acc attacker ->
          let j = Piece.Kind.to_int attacker in
          tbl.(i + j * num_victims) <- acc;
          acc + 1)) |> ignore;
  fun victim attacker ->
    let i = Piece.Kind.to_int victim in
    let j = Piece.Kind.to_int attacker in
    Array.unsafe_get tbl (i + j * num_victims)
