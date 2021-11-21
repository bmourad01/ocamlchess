open Core_kernel
open OUnit2
open Chess

let cmp_bitboard = Bitboard.equal

let test_bitboard expected result =
  assert_equal result expected ~cmp:cmp_bitboard
    ~msg:
      (sprintf "Got 0x%LX, expected 0x%LX"
         (Bitboard.to_int64 result)
         (Bitboard.to_int64 expected) )

let test_queen_empty_a1 () =
  let occupied = Bitboard.of_int64 0L in
  let expected = Bitboard.of_int64 0x81412111090503FEL in
  let result = Precalculated.queen Square.a1 occupied in
  test_bitboard expected result

let test_queen_empty_a8 () =
  let occupied = Bitboard.of_int64 0L in
  let expected = Bitboard.of_int64 0xFE03050911214181L in
  let result = Precalculated.queen Square.a8 occupied in
  test_bitboard expected result

let test_queen_empty_h1 () =
  let occupied = Bitboard.of_int64 0L in
  let expected = Bitboard.of_int64 0x8182848890A0C07FL in
  let result = Precalculated.queen Square.h1 occupied in
  test_bitboard expected result

let test_queen_empty_h8 () =
  let occupied = Bitboard.of_int64 0L in
  let expected = Bitboard.of_int64 0x7FC0A09088848281L in
  let result = Precalculated.queen Square.h8 occupied in
  test_bitboard expected result

let test_bishop_start_c1 () =
  let occupied = Bitboard.of_int64 0xFFFF00000000FFFFL in
  let expected = Bitboard.of_int64 0xA00L in
  let result = Precalculated.bishop Square.c1 occupied in
  test_bitboard expected result

let test_bishop_start_f1 () =
  let occupied = Bitboard.of_int64 0xFFFF00000000FFFFL in
  let expected = Bitboard.of_int64 0x5000L in
  let result = Precalculated.bishop Square.f1 occupied in
  test_bitboard expected result

let test_bishop_start_c8 () =
  let occupied = Bitboard.of_int64 0xFFFF00000000FFFFL in
  let expected = Bitboard.of_int64 0xA000000000000L in
  let result = Precalculated.bishop Square.c8 occupied in
  test_bitboard expected result

let test_bishop_start_f8 () =
  let occupied = Bitboard.of_int64 0xFFFF00000000FFFFL in
  let expected = Bitboard.of_int64 0x50000000000000L in
  let result = Precalculated.bishop Square.f8 occupied in
  test_bitboard expected result

let test_rook_start_a1 () =
  let occupied = Bitboard.of_int64 0xFFFF00000000FFFFL in
  let expected = Bitboard.of_int64 0x102L in
  let result = Precalculated.rook Square.a1 occupied in
  test_bitboard expected result

let test_rook_start_h1 () =
  let occupied = Bitboard.of_int64 0xFFFF00000000FFFFL in
  let expected = Bitboard.of_int64 0x8040L in
  let result = Precalculated.rook Square.h1 occupied in
  test_bitboard expected result

let test_rook_start_a8 () =
  let occupied = Bitboard.of_int64 0xFFFF00000000FFFFL in
  let expected = Bitboard.of_int64 0x201000000000000L in
  let result = Precalculated.rook Square.a8 occupied in
  test_bitboard expected result

let test_rook_start_h8 () =
  let occupied = Bitboard.of_int64 0xFFFF00000000FFFFL in
  let expected = Bitboard.of_int64 0x4080000000000000L in
  let result = Precalculated.rook Square.h8 occupied in
  test_bitboard expected result

let suite =
  "Test precalculated"
  >::: [ ("Queen empty a1" >:: fun _ -> test_queen_empty_a1 ())
       ; ("Queen empty a8" >:: fun _ -> test_queen_empty_a8 ())
       ; ("Queen empty h1" >:: fun _ -> test_queen_empty_h1 ())
       ; ("Queen empty h8" >:: fun _ -> test_queen_empty_h8 ())
       ; ("Bishop start c1" >:: fun _ -> test_bishop_start_c1 ())
       ; ("Bishop start f1" >:: fun _ -> test_bishop_start_f1 ())
       ; ("Bishop start c8" >:: fun _ -> test_bishop_start_c8 ())
       ; ("Bishop start f8" >:: fun _ -> test_bishop_start_f8 ())
       ; ("Rook start a1" >:: fun _ -> test_rook_start_a1 ())
       ; ("Rook start h1" >:: fun _ -> test_rook_start_h1 ())
       ; ("Rook start a8" >:: fun _ -> test_rook_start_a8 ())
       ; ("Rook start a8" >:: fun _ -> test_rook_start_h8 ()) ]

let () = run_test_tt_main suite
