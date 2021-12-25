open Core_kernel
open OUnit2
open Chess

let test_fold_full () =
  let count, l =
    Bitboard.fold Bitboard.full ~init:(0, [])
      ~f:(fun (count, l) _ -> count + 1, count :: l) in
  assert_equal count Square.count ~cmp:Int.equal;
  assert_equal l (List.init Square.count ~f:ident |> List.rev)
    ~cmp:(List.equal Int.equal)

let test_fold_full_rev () =
  let count, l =
    Bitboard.fold Bitboard.full ~init:(0, [])
      ~f:(fun (count, l) _ -> count + 1, count :: l)
      ~rev:true in
  assert_equal count Square.count ~cmp:Int.equal;
  assert_equal l (List.init Square.count ~f:ident |> List.rev)
    ~cmp:(List.equal Int.equal)

let test_fold_empty () =
  let count =
    Bitboard.fold Bitboard.empty ~init:0 ~f:(fun count _ -> count + 1) in
  assert_equal count 0 ~cmp:Int.equal

let test_fold_empty_rev () =
  let count =
    Bitboard.fold Bitboard.empty ~init:0
      ~f:(fun count _ -> count + 1)
      ~rev:true in
  assert_equal count 0 ~cmp:Int.equal

let test_rank_accessor () =
  let cmp = Option.equal Bitboard.equal in
  assert_equal (Bitboard.rank 0) (Some Bitboard.rank_1) ~cmp;
  assert_equal (Bitboard.rank 1) (Some Bitboard.rank_2) ~cmp;
  assert_equal (Bitboard.rank 2) (Some Bitboard.rank_3) ~cmp;
  assert_equal (Bitboard.rank 3) (Some Bitboard.rank_4) ~cmp;
  assert_equal (Bitboard.rank 4) (Some Bitboard.rank_5) ~cmp;
  assert_equal (Bitboard.rank 5) (Some Bitboard.rank_6) ~cmp;
  assert_equal (Bitboard.rank 6) (Some Bitboard.rank_7) ~cmp;
  assert_equal (Bitboard.rank 7) (Some Bitboard.rank_8) ~cmp;
  assert_equal (Bitboard.rank (-1)) None ~cmp;
  assert_equal (Bitboard.rank 8) None ~cmp

let test_file_accessor () =
  let cmp = Option.equal Bitboard.equal in
  assert_equal (Bitboard.file 0) (Some Bitboard.file_a) ~cmp;
  assert_equal (Bitboard.file 1) (Some Bitboard.file_b) ~cmp;
  assert_equal (Bitboard.file 2) (Some Bitboard.file_c) ~cmp;
  assert_equal (Bitboard.file 3) (Some Bitboard.file_d) ~cmp;
  assert_equal (Bitboard.file 4) (Some Bitboard.file_e) ~cmp;
  assert_equal (Bitboard.file 5) (Some Bitboard.file_f) ~cmp;
  assert_equal (Bitboard.file 6) (Some Bitboard.file_g) ~cmp;
  assert_equal (Bitboard.file 7) (Some Bitboard.file_h) ~cmp;
  assert_equal (Bitboard.file (-1)) None ~cmp;
  assert_equal (Bitboard.file 8) None ~cmp

let suite = "Test bitboard" >::: [
    ("Fold full" >:: fun _ -> test_fold_full ());
    ("Fold full (rev)" >:: fun _ -> test_fold_full_rev ());
    ("Fold empty" >:: fun _ -> test_fold_empty ());
    ("Fold empty (rev)" >:: fun _ -> test_fold_empty_rev ());
    ("Rank accessor" >:: fun _ -> test_rank_accessor ());
    ("File accessor" >:: fun _ -> test_file_accessor ());
  ]

let () = run_test_tt_main suite
