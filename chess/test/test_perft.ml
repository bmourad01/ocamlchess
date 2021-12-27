open Core_kernel
open OUnit2
open Chess

(* The tests here can be found at:
   https://www.chessprogramming.org/Perft_Results *)

let rec perft pos depth =
  if depth <= 0 then 1L
  else
    Position.legal_moves pos |>
    Position.Legal_moves.moves |>
    List.fold ~init:0L ~f:(fun acc mv ->
        let pos = Position.Legal_move.position mv in
        Int64.(acc + perft pos (Int.pred depth)))

let expect pos depth expected =
  let nodes = perft pos depth in
  assert_equal nodes expected ~cmp:Int64.equal
    ~msg:(sprintf "At depth %d, expected %Lu nodes, got %Lu nodes"
            depth expected nodes)

let go pos =
  List.iter ~f:(fun (depth, expected) -> expect pos depth expected)

(* Depth 6 passes, but is very slow. Have not tried depth 7. *)
let test_starting_position () =
  go Position.start [
    1, 20L;
    2, 400L;
    3, 8_902L;
    4, 197_281L;
    5, 4_865_609L;
    (* 6, 119_060_324L; *)
    (* 7, 3_195_901_860L; *)
  ]

let test_position_2 () =
  let pos = Position.Fen.of_string_exn
      "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1" in
  go pos [
    1, 48L;
    2, 2039L;
    3, 97_862L;
    4, 4_085_603L;
  ]

let test_position_3 () =
  let pos = Position.Fen.of_string_exn
      "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1" in
  go pos [
    1, 14L;
    2, 191L;
    3, 2_812L;
    4, 43_238L;
    5, 674_624L;
  ]

let test_position_4 () =
  let pos = Position.Fen.of_string_exn
      "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1" in
  go pos [
    1, 6L;
    2, 264L;
    3, 9_467L;
    4, 422_333L;
    5, 15_833_292L;
  ] 

(* Depth 5 passes, but is very slow. *)
let test_position_5 () =
  let pos = Position.Fen.of_string_exn
      "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8" in
  go pos [
    1, 44L;
    2, 1_486L;
    3, 62_379L;
    4, 2_103_487L;
    (* 5, 89_941_194L; *)
  ]

(* Depth 5 passes, but is very slow. *)
let test_position_6 () =
  let pos = Position.Fen.of_string_exn
      "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10" in
  go pos [
    1, 46L;
    2, 2_079L;
    3, 89_890L;
    4, 3_894_594L;
    (* 5, 164_075_551L; *)
  ]
  
let suite = "Test perft" >::: [
    ("Starting position" >:: fun _ -> test_starting_position ());
    ("Position 2" >:: fun _ -> test_position_2 ());
    ("Position 3" >:: fun _ -> test_position_3 ());
    ("Position 4" >:: fun _ -> test_position_4 ());
    ("Position 5" >:: fun _ -> test_position_5 ());
    ("Position 6" >:: fun _ -> test_position_6 ());
  ]

let () = run_test_tt_main suite
