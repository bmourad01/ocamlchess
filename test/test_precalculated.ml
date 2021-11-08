open Core_kernel
open OUnit2
open Chess

let cmp_bitboard = Bitboard.equal

let test_queen_empty_a1 () =
  let occupied = Bitboard.of_int64 0L in
  let expected = Bitboard.of_int64 0x81412111090503FEL in
  let result = Precalculated.queen Square.a1 occupied in
  assert_equal result expected ~cmp:cmp_bitboard
    ~msg:
      (sprintf "Got 0x%LX, expected 0x%LX"
         (Bitboard.to_int64 result)
         (Bitboard.to_int64 expected) )

let test_queen_empty_a8 () =
  let occupied = Bitboard.of_int64 0L in
  let expected = Bitboard.of_int64 0xFE03050911214181L in
  let result = Precalculated.queen Square.a8 occupied in
  assert_equal result expected ~cmp:cmp_bitboard
    ~msg:
      (sprintf "Got 0x%LX, expected 0x%LX"
         (Bitboard.to_int64 result)
         (Bitboard.to_int64 expected) )

let test_queen_empty_h1 () =
  let occupied = Bitboard.of_int64 0L in
  let expected = Bitboard.of_int64 0x8182848890A0C07FL in
  let result = Precalculated.queen Square.h1 occupied in
  assert_equal result expected ~cmp:cmp_bitboard
    ~msg:
      (sprintf "Got 0x%LX, expected 0x%LX"
         (Bitboard.to_int64 result)
         (Bitboard.to_int64 expected) )

let test_queen_empty_h8 () =
  let occupied = Bitboard.of_int64 0L in
  let expected = Bitboard.of_int64 0x7FC0A09088848281L in
  let result = Precalculated.queen Square.h8 occupied in
  assert_equal result expected ~cmp:cmp_bitboard
    ~msg:
      (sprintf "Got 0x%LX, expected 0x%LX"
         (Bitboard.to_int64 result)
         (Bitboard.to_int64 expected) )

let suite =
  "Test precalculated"
  >::: [ ("Queen empty a1" >:: fun _ -> test_queen_empty_a1 ())
       ; ("Queen empty a8" >:: fun _ -> test_queen_empty_a8 ())
       ; ("Queen empty h1" >:: fun _ -> test_queen_empty_h1 ())
       ; ("Queen empty h8" >:: fun _ -> test_queen_empty_h8 ()) ]

let () = run_test_tt_main suite
