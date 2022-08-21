open Core_kernel
open OUnit2
open Chess

let go_aux pos m expected =
  let result = Position.gives_check pos m in
  assert_equal ~cmp:Bool.equal expected result

let go pos m expected =
  let pos = Position.Fen.of_string_exn pos in
  let m = Move.of_string_exn m in
  go_aux pos m expected

let test_start () =
  let pos = Position.start in
  Position.legal_moves pos |>
  List.iter ~f:(fun m -> go_aux pos m false)

let test_direct () =
  go "5k2/8/8/8/8/8/8/4K2R w K - 0 1" "h1f1" true

let test_discovery () =
  go "rn2kb2/pppqr2p/4b3/1B4p1/8/8/PPPP1PPP/RNB1K1NR b KQq - 15 14" "e6g8" true

let test_en_passant_direct () =
  go "rnbq1bnr/ppppk1pp/8/4Pp2/8/8/PPPP1PPP/RNBQKBNR w KQ f6 0 1" "e5f6" true

let test_en_passant_discovery () =
  go "8/8/3k4/3Pp3/8/6B1/8/2K5 w - e6 0 1" "d5e6" true

let test_kingside_castle () =
  go "5k2/8/8/8/8/8/8/4K2R w K - 0 1" "e1g1" true

let test_queenside_castle () =
  go "3k4/8/8/8/8/8/8/R3K3 w Q - 0 1" "e1c1" true

let test_promotion () =
  go "2k2b2/6P1/8/8/8/8/8/2K5 w - - 0 1" "g7f8q" true

let suite = "Test checks" >::: [
    ("Starting position" >:: fun _ -> test_start ());
    ("Direct" >:: fun _ -> test_direct ());
    ("Discovery" >:: fun _ -> test_discovery ());
    ("En passant direct" >:: fun _ -> test_en_passant_direct ());
    ("En passant discovery" >:: fun _ -> test_en_passant_discovery ());
    ("Kingside castle" >:: fun _ -> test_kingside_castle ());
    ("Queenside castle" >:: fun _ -> test_queenside_castle ());
    ("Promotion" >:: fun _ -> test_promotion ());
  ]

let () = run_test_tt_main suite
