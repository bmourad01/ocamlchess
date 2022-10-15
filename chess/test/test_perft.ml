open Core_kernel [@@warning "-D"]
open OUnit2
open Chess

module Child = Position.Child

(* The tests here can be found at:

   https://www.chessprogramming.org/Perft_Results
*)

let rec perft_aux child depth =
  if depth <= 0 then 1L
  else
    Child.self child |> Position.children |>
    List.fold ~init:0L ~f:(fun acc child ->
        Int64.(acc + perft_aux child Int.(depth - 1)))

let perft pos depth =
  let depth = depth - 1 in
  Position.children pos |> List.fold ~init:0L
    ~f:(fun acc child -> Int64.(acc + perft_aux child depth))

let expect pos depth expected =
  let nodes = perft pos depth in
  assert_equal nodes expected ~cmp:Int64.equal
    ~msg:(sprintf "At depth %d, expected %Lu nodes, got %Lu nodes"
            depth expected nodes)

let test_start = expect Position.start

let test_2 = expect @@ Position.Fen.of_string_exn
    "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"

let test_3 = expect @@ Position.Fen.of_string_exn
    "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"

let test_4 = expect @@ Position.Fen.of_string_exn
    "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"

let test_5 = expect @@ Position.Fen.of_string_exn
    "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"

let test_6 = expect @@ Position.Fen.of_string_exn
    "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10"

let suite = "Test perft" >::: [
    ("Start position (depth 1)" >:: fun _ -> test_start 1 20L);
    ("Start position (depth 2)" >:: fun _ -> test_start 2 400L);
    ("Start position (depth 3)" >:: fun _ -> test_start 3 8_902L);
    ("Start position (depth 4)" >:: fun _ -> test_start 4 197_281L);
    ("Start position (depth 5)" >:: fun _ -> test_start 5 4_865_609L);
    ("Start position (depth 6)" >:: fun _ -> test_start 6 119_060_324L);

    ("Position 2 (depth 1)" >:: fun _ -> test_2 1 48L);
    ("Position 2 (depth 2)" >:: fun _ -> test_2 2 2039L);
    ("Position 2 (depth 3)" >:: fun _ -> test_2 3 97_862L);
    ("Position 2 (depth 4)" >:: fun _ -> test_2 4 4_085_603L);

    ("Position 3 (depth 1)" >:: fun _ -> test_3 1 14L);
    ("Position 3 (depth 2)" >:: fun _ -> test_3 2 191L);
    ("Position 3 (depth 3)" >:: fun _ -> test_3 3 2_812L);
    ("Position 3 (depth 4)" >:: fun _ -> test_3 4 43_238L);
    ("Position 3 (depth 5)" >:: fun _ -> test_3 5 674_624L);
    ("Position 3 (depth 6)" >:: fun _ -> test_3 6 11_030_083L);

    ("Position 4 (depth 1)" >:: fun _ -> test_4 1 6L);
    ("Position 4 (depth 2)" >:: fun _ -> test_4 2 264L);
    ("Position 4 (depth 3)" >:: fun _ -> test_4 3 9_467L);
    ("Position 4 (depth 4)" >:: fun _ -> test_4 4 422_333L);
    ("Position 4 (depth 5)" >:: fun _ -> test_4 5 15_833_292L);

    ("Position 5 (depth 1)" >:: fun _ -> test_5 1 44L);
    ("Position 5 (depth 2)" >:: fun _ -> test_5 2 1_486L);
    ("Position 5 (depth 3)" >:: fun _ -> test_5 3 62_379L);
    ("Position 5 (depth 4)" >:: fun _ -> test_5 4 2_103_487L);
    ("Position 5 (depth 5)" >:: fun _ -> test_5 5 89_941_194L);

    ("Position 6 (Depth 1)" >:: fun _ -> test_6 1 46L);
    ("Position 6 (depth 2)" >:: fun _ -> test_6 2 2_079L);
    ("Position 6 (depth 3)" >:: fun _ -> test_6 3 89_890L);
    ("Position 6 (depth 4)" >:: fun _ -> test_6 4 3_894_594L);
    ("Position 6 (depth 5)" >:: fun _ -> test_6 5 164_075_551L);
  ]

let () = run_test_tt_main suite
