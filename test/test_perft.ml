open Core_kernel
open OUnit2
open Chess

(* The tests here can be found at:
   https://www.chessprogramming.org/Perft_Results *)

let rec perft pos depth =
  if depth <= 0 then 1L
  else
    Position.legal_moves pos |>
    List.fold ~init:0L ~f:(fun acc (_, pos) ->
        Int64.(acc + perft pos (Int.pred depth)))

let expect pos depth expected =
  let nodes = perft pos depth in
  assert_equal nodes expected ~cmp:Int64.equal
    ~msg:(sprintf "At depth %d, expected %Ld nodes, got %Ld nodes"
            depth expected nodes)

(* Depth of 6 is currently correct, but very slow. *)
let test_starting_position () =
  let pos = Position.start in
  List.iter [
    1, 20L;
    2, 400L;
    3, 8_902L;
    4, 197_281L;
    5, 4_865_609L;
    (* 6, 119_060_324L; *)
    (* 7, 3_195_901_860L; *)
  ] ~f:(fun (depth, expected) -> expect pos depth expected)

let test_position_2 () =
  let pos = Position.Fen.of_string_exn
      "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1" in
  List.iter [
    1, 48L;
    2, 2039L;
    3, 97_862L;
    4, 4_085_603L;
  ] ~f:(fun (depth, expected) -> expect pos depth expected)

let test_position_3 () =
  let pos = Position.Fen.of_string_exn
      "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1" in
  List.iter [
    1, 14L;
    2, 191L;
    3, 2_812L;
    4, 43_238L;
    5, 674_624L;
  ] ~f:(fun (depth, expected) -> expect pos depth expected)

let suite = "Test perft" >::: [
    ("Starting position" >:: fun _ -> test_starting_position ());
    ("Position 2" >:: fun _ -> test_position_2 ());
    ("Position 3" >:: fun _ -> test_position_3 ());
  ]

let () = run_test_tt_main suite
