(*  *)
open Core_kernel
open OUnit2
open Chess

let to_string = function
  | Some cmd -> Uci.to_string cmd
  | None -> "(none)"

let test_parse s ~expected =
  let cmd = Uci.of_string s in
  let msg = sprintf
      "Parsing string:\n\n`%s`\n\nGot:\n\n`%s`\n\nExpected:\n\n`%s`\n"
      s (to_string cmd) (to_string expected) in
  assert_equal cmd expected ~cmp:(Option.equal Uci.equal) ~msg

let make_moves s = List.map s ~f:Move.of_string_exn

let test_combo_1 () = test_parse
    "option name Style type combo default Normal var Solid var Normal var Risky"
    ~expected:Uci.(Some (Send (Option Send.Option.{
        name = "Style";
        typ = Type.(Combo {
            default = "Normal";
            var = ["Solid"; "Normal"; "Risky"];
          });
      })))

let test_combo_2 () = test_parse
    "option name Style type combo default Normal var var Normal var Risky"
    ~expected:None

let test_setoption_1 () = test_parse
    "setoption name UCI_Opponent value GM 2800 human Gary Kasparow"
    ~expected:Uci.(Some (Recv (Setoption Recv.Setoption.{
        name = "UCI_Opponent";
        value = Some "GM 2800 human Gary Kasparow";
      })))

let test_setoption_2 () =
  test_parse "setoption name value Value" ~expected:None

let test_setoption_3 () =
  test_parse "setoption name Name value" ~expected:None

let test_setoption_4 () = test_parse
    "setoption name Bart Simpson"
    ~expected:Uci.(Some (Recv (Setoption Recv.Setoption.{
        name = "Bart Simpson";
        value = None;
      })))

let test_info_1 () = test_parse
    "info depth 2 score cp 214 time 1242 nodes 2124 nps 34928 pv e2e4 e7e5 g1f3"
    ~expected:Uci.(Some (Send (Info Send.Info.[
        Depth 2;
        Score {cp = 214; mate = None; bound = None};
        Time 1242;
        Nodes 2124;
        Nps 34928;
        Pv (make_moves ["e2e4"; "e7e5"; "g1f3"]);
      ])))

let suite = "Test uci" >::: [
    ("Test combo option 1" >:: fun _ -> test_combo_1 ());
    ("Test combo option 2" >:: fun _ -> test_combo_2 ());
    ("Test setoption 1" >:: fun _ -> test_setoption_1 ());
    ("Test setoption 2" >:: fun _ -> test_setoption_2 ());
    ("Test setoption 3" >:: fun _ -> test_setoption_3 ());
    ("Test setoption 4" >:: fun _ -> test_setoption_4 ());
    ("Test info 1" >:: fun _ -> test_info_1 ());
  ]

let () = run_test_tt_main suite
