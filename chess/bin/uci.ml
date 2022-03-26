open Core_kernel
open Chess

(* Interprets a command. Returns true if the main UCI loop shall continue. *)
let recv ~debug = function
  | Uci.Recv.Quit -> false
  | cmd ->
    if debug then
      eprintf "Unhandled command: %s\n%!" @@ Uci.Recv.to_string cmd;
    true

(* Entry point. *)
let rec run ~debug =
  In_channel.(input_line stdin) |> Option.iter ~f:(input ~debug)

(* Process input. *)
and input line ~debug = match Uci.Recv.of_string line with
  | None ->
    printf "Unknown command: %s\n%!" line;
    run ~debug
  | Some cmd ->
    if debug then
      eprintf "Received command: %s\n%!" @@ Uci.Recv.to_string cmd;
    if recv cmd ~debug then
      run ~debug
