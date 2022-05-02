open Core_kernel
open OUnit2
open Chess

module Fen = Position.Fen
module Legal = Position.Legal

let expected_hash pos =
  Fen.to_string pos |>
  Fen.of_string_exn |>
  Position.hash

let expected_pawn_hash pos =
  Fen.to_string pos |>
  Fen.of_string_exn |>
  Position.pawn_hash

let expected_material_hash pos =
  Fen.to_string pos |>
  Fen.of_string_exn |>
  Position.material_hash

let test_single_aux m pos =
  let hash = Position.hash pos in
  let expected = expected_hash pos in
  assert_equal hash expected ~cmp:Int64.equal
    ~msg:(Format.asprintf
            "Position '%a' has hash %016LX after making move %a, but was \
             expected to have hash %016LX"
            Position.pp pos hash Move.pp m expected);
  let hash = Position.pawn_hash pos in
  let expected = expected_pawn_hash pos in
  assert_equal hash expected ~cmp:Int64.equal
    ~msg:(Format.asprintf
            "Position '%a' has pawn hash %016LX after making move %a, but \
             was expected to have pawn hash %016LX"
            Position.pp pos hash Move.pp m expected);
  let hash = Position.material_hash pos in
  let expected = expected_material_hash pos in
  assert_equal hash expected ~cmp:Int64.equal
    ~msg:(Format.asprintf
            "Position '%a' has material hash %016LX after making move %a, but \
             was expected to have material hash %016LX"
            Position.pp pos hash Move.pp m expected)

let test_single legal =
  let m = Legal.move legal in
  let pos = Legal.new_position legal in
  test_single_aux m pos

let test pos = Position.legal_moves pos |> List.iter ~f:test_single

let test_starting () = test Position.start

let test_position_2 () =
  test @@ Fen.of_string_exn
    "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"

let test_position_3 () =
  test @@ Fen.of_string_exn
    "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"

let test_position_4 () =
  test @@ Fen.of_string_exn
    "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"

let test_position_5 () =
  test @@ Fen.of_string_exn
    "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"

let test_position_6 () =
  test @@ Fen.of_string_exn
    "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10"

let test_move m pos =
  Position.make_move pos m |> Legal.new_position |> test_single_aux m

let test_7 () =
  let m = Move.of_string_exn "h7h6" in
  let pos = Fen.of_string_exn
      "r1bqkbnr/1ppppppp/p7/8/1n3P2/1P4PN/P1PPP2P/RNBQKB1R b KQkq f3 0 4" in
  test_move m pos

let suite = "Test Zobrist Hashing" >::: [
    ("Test starting position" >:: fun _ -> test_starting ());
    ("Test position 2" >:: fun _ -> test_position_2 ());
    ("Test position 3" >:: fun _ -> test_position_3 ());
    ("Test position 4" >:: fun _ -> test_position_4 ());
    ("Test position 5" >:: fun _ -> test_position_5 ());
    ("Test position 6" >:: fun _ -> test_position_6 ());
    ("Test 7" >:: fun _ -> test_7 ());
  ]

let () = run_test_tt_main suite
