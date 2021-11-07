open Core_kernel
open OUnit2
open Chess

let test_fold_full () =
  let count =
    Bitboard.fold Bitboard.full ~init:0 ~f:(fun count _ -> count + 1)
  in
  assert_equal count 64 ~cmp:Int.equal

let test_fold_full_rev () =
  let count =
    Bitboard.fold Bitboard.full ~init:0
      ~f:(fun count _ -> count + 1)
      ~rev:true
  in
  assert_equal count 64 ~cmp:Int.equal

let test_fold_empty () =
  let count =
    Bitboard.fold Bitboard.empty ~init:0 ~f:(fun count _ -> count + 1)
  in
  assert_equal count 0 ~cmp:Int.equal

let test_fold_empty_rev () =
  let count =
    Bitboard.fold Bitboard.empty ~init:0
      ~f:(fun count _ -> count + 1)
      ~rev:true
  in
  assert_equal count 0 ~cmp:Int.equal

let suite =
  "Test bitboard"
  >::: [ ("Fold full" >:: fun _ -> test_fold_full ())
       ; ("Fold full (rev)" >:: fun _ -> test_fold_full_rev ())
       ; ("Fold empty" >:: fun _ -> test_fold_empty ())
       ; ("Fold empty (rev)" >:: fun _ -> test_fold_empty_rev ()) ]

let () = run_test_tt_main suite
