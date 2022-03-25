open Core_kernel
open Chess

let go ~debug =
  let rec loop () =
    In_channel.(input_line stdin) |> Option.iter ~f:(fun line ->
        match Uci.Recv.of_string line with
        | None ->
          if debug then
            eprintf "Failed to parse command: %s\n%!" line
        | Some recv ->
          if debug then
            eprintf "Parsed command: %s\n%!" @@ Uci.Recv.to_string recv;
          loop ()) in
  loop ()
