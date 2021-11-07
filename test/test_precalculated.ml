open Core_kernel
open OUnit2
open Chess

let cmp_bitboard = Bitboard.equal

let test_queen_empty () =
  let occupied = Bitboard.of_int64 0L in
  let expected = Bitboard.of_int64 0x81412111090503FEL in
  let result = Precalculated.queen Square.a1 occupied in
  assert_equal result expected ~cmp:cmp_bitboard
    ~msg:
      (sprintf "Got 0x%LX, expected 0x%LX"
         (Bitboard.to_int64 result)
         (Bitboard.to_int64 expected) )

let suite =
  "Test precalculated" >::: [("Queen empty" >:: fun _ -> test_queen_empty ())]

let () = run_test_tt_main suite
