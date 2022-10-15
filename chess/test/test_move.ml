open Core_kernel [@@warning "-D"]
open OUnit2
open Chess

let cmp_move_opt = Option.equal Move.equal
let cmp_square = Square.equal
let cmp_promote = Option.equal Move.Promote.equal

let test_parse () =
  let m = Move.of_string_exn "a1b2" in
  assert_equal (Move.src m) Square.a1 ~cmp:cmp_square;
  assert_equal (Move.dst m) Square.b2 ~cmp:cmp_square;
  assert_equal (Move.promote m) None ~cmp:cmp_promote

let test_parse_empty () =
  assert_equal (Move.of_string "") None ~cmp:cmp_move_opt

let test_parse_promote () =
  let m = Move.of_string_exn "c7c8q" in
  assert_equal (Move.src m) Square.c7 ~cmp:cmp_square;
  assert_equal (Move.dst m) Square.c8 ~cmp:cmp_square;
  assert_equal (Move.promote m) (Some Move.Promote.Queen) ~cmp:cmp_promote

let test_parse_garbage () =
  assert_equal (Move.of_string "abcdefgh") None ~cmp:cmp_move_opt

let suite = "Test move" >::: [
    ("Parse" >:: fun _ -> test_parse ());
    ("Parse empty" >:: fun _ -> test_parse_empty ());
    ("Parse promote" >:: fun _ -> test_parse_promote ());
    ("Parse garbage" >:: fun _ -> test_parse_garbage ());
  ]

let () = run_test_tt_main suite
