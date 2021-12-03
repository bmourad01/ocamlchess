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

let test_starting_position () =
  let pos = Position.start in
  List.iter [
    0, 1L;
    1, 20L;
    2, 400L;
    3, 8902L;
    4, 197281L;
    5, 4865609L;
    (* 6, 119060324L; *)
  ] ~f:(fun (depth, expected) -> expect pos depth expected)
      

let suite = "Test perft" >::: [
    ("Starting position" >:: fun _ -> test_starting_position ());
  ]

let () = run_test_tt_main suite
