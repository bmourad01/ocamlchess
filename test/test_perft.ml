open Core_kernel
open OUnit2
open Chess

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
    0, 1L;
    1, 20L;
    2, 400L;
    3, 8_902L;
    4, 197_281L;
    5, 4_865_609L;
    (* 6, 119_060_324L; *)
    (* 7, 3_195_901_860L; *)
  ] ~f:(fun (depth, expected) -> expect pos depth expected)

let suite = "Test perft" >::: [
    ("Starting position" >:: fun _ -> test_starting_position ());
  ]

let () = run_test_tt_main suite
