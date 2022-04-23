open Core_kernel
open OUnit2
open Chess

let p = Piece.Kind.value Pawn
let n = Piece.Kind.value Knight
let b = Piece.Kind.value Bishop
let r = Piece.Kind.value Rook
let q = Piece.Kind.value Queen

let test fen move expected =
  let pos = Position.Fen.of_string_exn fen in
  let move = Move.of_string_exn move in
  let m = Position.make_move pos move in
  let see = Option.value_exn (See.go m) in
  assert_equal see expected
    ~cmp:(=) ~msg:(sprintf "Got %d, expected %d" see expected)

let test_1 () =
  test "4R3/2r3p1/5bk1/1p1r3p/p2PR1P1/P1BK1P2/1P6/8 b - - 0 1" "h5g4" 0

let test_2 () =
  test "4R3/2r3p1/5bk1/1p1r1p1p/p2PR1P1/P1BK1P2/1P6/8 b - - 0 1" "h5g4" 0

let test_3 () =
  test "4r1k1/5pp1/nbp4p/1p2p2q/1P2P1b1/1BP2N1P/1B2QPPK/3R4 b - - 0 1" "g4f3" 0

let test_4 () =
  test "2r1r1k1/pp1bppbp/3p1np1/q3P3/2P2P2/1P2B3/P1N1B1PP/2RQ1RK1 b - - 0 1" "d6e5" p

let test_5 () =
  test "3r3k/3r4/2n1n3/8/3p4/2PR4/1B1Q4/3R3K w - - 0 1" "d3d4" p

let test_6 () =
  test "1k1r4/1ppn3p/p4b2/4n3/8/P2N2P1/1PP1R1BP/2K1Q3 w - - 0 1" "d3e5" (n - n + b - r + n)

let suite = "Test SEE" >::: [
    ("Test 1" >:: fun _ -> test_1 ());
    ("Test 2" >:: fun _ -> test_2 ());
    ("Test 3" >:: fun _ -> test_3 ());
    ("Test 4" >:: fun _ -> test_4 ());
    ("Test 5" >:: fun _ -> test_5 ());
    ("Test 6" >:: fun _ -> test_6 ());
  ]

let () = run_test_tt_main suite
