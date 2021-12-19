open Core_kernel
open Cmdliner

module Perft = struct
  let go depth pos =
    if depth < 1 then invalid_arg @@
      sprintf "Invalid depth value %d" depth
    else Perft.go depth @@ Chess.Position.Fen.of_string_exn pos

  let depth =
    let doc = "The depth to search in the game tree." in
    Arg.(value & pos 0 int 1 & info [] ~docv:"DEPTH" ~doc)

  let pos =
    let doc = "The position to search from, represented as a FEN string." in
    Arg.(value &
         pos 1 string Chess.Position.Fen.start &
         info [] ~docv:"POSITION" ~doc)

  let t = Term.(const go $ depth $ pos)

  let info =
    let doc = "Runs the performance test; enumerates move paths." in
    Term.info "perft"
      ~version:"%%VERSION%%"
      ~doc
      ~exits:Term.default_exits
      ~man:[]
end

module Gui = struct
  let go pos = Gui.go @@ Chess.Position.Fen.of_string_exn pos

  let pos =
    let doc = "The position to play from, represented as a FEN string." in
    Arg.(value &
         pos 0 string Chess.Position.Fen.start &
         info [] ~docv:"POSITION" ~doc)

  let t = Term.(const go $ pos)

  let info =
    let doc = "Runs the testing GUI." in
    Term.info "gui"
      ~version:"%%VERSION%%"
      ~doc
      ~exits:Term.default_exits
      ~man:[]
end

module Uci = struct
  let go _ = Uci.go ()

  let dummy =
    let doc = "Dummy parameter (does nothing)." in
    Arg.(value & flag & info ["dummy"] ~docv:"DUMMY" ~doc)
  
  let t = Term.(const go $ dummy)

  let info =
    let doc = "Runs the UCI loop." in
    Term.info "uci"
      ~version:"%%VERSION%%"
      ~doc
      ~exits:Term.default_exits
      ~man:[]
end

module Default = struct
  let t = Term.(ret @@ const @@ `Help (`Pager, None))

  let info =
    let doc = "A UCI-compatible chess engine." in
    Term.info "chess"
      ~doc
      ~exits:Term.default_exits
      ~man:[]
end

let () =
  Term.exit @@ Term.eval_choice Default.(t, info) [
    Perft.(t, info);
    Gui.(t, info);
    Uci.(t, info);
  ]
