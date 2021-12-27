open Cmdliner

let man_players = [
  `S "PLAYER (predefined algorithm for the computer)";
  `P "random: the player that makes moves randomly";
  `P "same-color: the player that tries to maximize the number of pieces \
      that are on squares of its color";
  `P "opposite-color: the player that tries to maximize the number of pieces \
      that are on squares that are opposite of its color";
  `P "cccp: the player that plays the 'checkmate, check, capture, push' \
      strategy (in that order)";
  `P "huddle: the player that tries to minimize the distance between its \
      pieces and its king";
  `P "swarm: the player that tries to minimize the distance between its \
      pieces and the enemy king";
  `P "min-oppt-moves: the player that attempts to minimize the number of \
      moves the opponent can make.";
  `P "max-oppt-moves: the player that attempts to maximize the number of \
      moves the opponent can make.";
  `P "suicide-king: the player that attempts to minimize the distance between \
      both kings.";
]

let choose_player ?(none_ok = true) = function
  | "" when none_ok -> None
  | s -> Some (Chess.Choose_player.choose s)

let validate =
  let doc = "Validate the input FEN position" in
  Arg.(value & flag (info ["validate"] ~doc))

module Perft = struct
  let go depth pos validate =
    if depth < 1 then invalid_arg @@
      Format.sprintf "Invalid depth value %d" depth
    else Perft.go depth @@ Chess.Position.Fen.of_string_exn pos ~validate

  let depth =
    let doc = "The depth to search in the game tree." in
    Arg.(value & pos 0 int 1 & info [] ~docv:"DEPTH" ~doc)

  let pos =
    let doc = "The position to search from, represented as a FEN string." in
    Arg.(value &
         pos 1 string Chess.Position.Fen.start &
         info [] ~docv:"POSITION" ~doc)

  let t = Term.(const go $ depth $ pos $ validate)

  let info =
    let doc = "Runs the performance test; enumerates move paths." in
    Term.info "perft"
      ~version:"%%VERSION%%"
      ~doc
      ~exits:Term.default_exits
      ~man:[]
end

module Gui = struct
  let go pos white black delay validate =
    let pos = Chess.Position.Fen.of_string_exn pos ~validate in
    let white = choose_player white in
    let black = choose_player black in
    let delay = match white, black with
      | Some _, Some _ -> fun () -> ignore @@ Unix.sleepf delay
      | _ -> Fun.id in
    Gui.go pos ~white ~black ~delay

  let pos =
    let doc = "The position to play from, represented as a FEN string." in
    Arg.(value &
         pos 0 string Chess.Position.Fen.start &
         info [] ~docv:"POSITION" ~doc)

  let white =
    let doc = "The AI player to play as white, if any." in
    Arg.(value & opt string "" (info ["white"] ~docv:"PLAYER" ~doc))

  let black =
    let doc = "The AI player to play as black, if any." in
    Arg.(value & opt string "" (info ["black"] ~docv:"PLAYER" ~doc))

  let delay =
    let doc = "Delay (in seconds) between AI moves \
               (only applies when both players are AI)" in
    Arg.(value & opt float 0.0 (info ["delay"] ~docv:"DELAY" ~doc))
  
  let t = Term.(const go $ pos $ white $ black $ delay $ validate)

  let info =
    let doc = "Runs the testing GUI." in
    Term.info "gui"
      ~version:"%%VERSION%%"
      ~doc
      ~exits:Term.default_exits
      ~man:[]
end

module Uci = struct
  let go player = match choose_player player ~none_ok:false with
    | None -> failwith "Expected player"
    | Some player -> Uci.go player

  let player =
    let doc = "The AI player." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PLAYER" ~doc)

  let t = Term.(const go $ player)

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
    Term.info "ocamlchess"
      ~doc
      ~exits:Term.default_exits
      ~man:man_players
end

let () =
  Term.exit @@ Term.eval_choice Default.(t, info) [
    Perft.(t, info);
    Gui.(t, info);
    Uci.(t, info);
  ]
