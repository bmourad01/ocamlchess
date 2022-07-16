open Core_kernel
open OUnit2
open Chess

module Threats = Position.Threats
module Legal = Position.Legal

let test_threats fen expected =
  let pos = Position.Fen.of_string_exn fen in
  let c = Position.active pos in
  let threats = Threats.(count @@ get pos c) in
  assert_equal threats expected
    ~cmp:(=) ~msg:(sprintf "Threats: got %d, expected %d" threats expected)

let test_new_threats mstr fen expected =
  let m = Move.of_string_exn mstr in
  let pos = Position.Fen.of_string_exn fen in
  let moves = Position.legal_moves pos in
  let legal = List.find_exn moves ~f:(Fn.flip Legal.is_move m) in
  let threats = Bitboard.count @@ Legal.new_threats legal in
  assert_equal threats expected ~cmp:(=)
    ~msg:(sprintf "New threats (%s): got %d, expected %d"
            mstr threats expected)
  
let test_1 () =
  test_threats "r7/1Pp5/2P3p1/8/6pb/4p1kB/4P1p1/6K1 w - - 0 1" 1

let test_2 () =
  test_new_threats "b7a8r" "r3q3/1Pp5/2P3p1/8/6pb/4p1kB/4P1p1/6K1 w - - 0 1" 1

let suite = "Test Threats" >::: [
    ("Test 1" >:: fun _ -> test_1 ());
    ("Test 2" >:: fun _ -> test_2 ());
  ]

let () = run_test_tt_main suite
