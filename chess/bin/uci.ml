open Core_kernel
open Chess

let go ~debug =
  let rec loop () =
    In_channel.(input_line stdin) |> Option.iter ~f:(fun line ->
        match Uci.Recv.of_string line with
        | None ->
          printf "Unknown command: %s\n%!" line;
          loop ()
        | Some recv ->
          if debug then
            eprintf "Received command: %s\n%!" @@ Uci.Recv.to_string recv;
          loop ()) in
  loop ()
