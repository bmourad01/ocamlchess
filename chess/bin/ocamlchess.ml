open Cmdliner

let man_players () =
  Players.register Caml_player.name Caml_player.create;
  Elo_world.init ();  
  `S "PLAYER" ::
  `Pre "Predefined algorithms for the computer." :: begin
    Players.enumerate () |> Core_kernel.Sequence.map
      ~f:(fun Chess.Player.(T player) ->
          `P (Format.sprintf "%s: %s"
                (Chess.Player.name player)
                (Chess.Player.desc player))) |>
    Core_kernel.Sequence.to_list
  end

let choose_player ?(none_ok = true) = function
  | "" when none_ok -> None
  | s -> match Players.lookup s with
    | None -> Core_kernel.invalid_argf "Player %s is not registered" s ()
    | Some _ as player -> player

let no_validate =
  let doc = "Don't validate the input FEN position" in
  Arg.(value & flag (info ["no-validate"] ~doc))

module Perft = struct
  let go depth pos no_validate =
    let validate = not no_validate in
    if depth < 1 then Core_kernel.invalid_argf "Invalid depth value %d" depth ()
    else Perft.go depth @@ Chess.Position.Fen.of_string_exn pos ~validate

  let depth =
    let doc = "The depth to search in the game tree." in
    Arg.(value & pos 0 int 1 & info [] ~docv:"DEPTH" ~doc)

  let pos =
    let doc = "The position to search from, represented as a FEN string." in
    Arg.(value &
         pos 1 string Chess.Position.Fen.start &
         info [] ~docv:"POSITION" ~doc)

  let t = Term.(const go $ depth $ pos $ no_validate)

  let info =
    let doc = "Runs the performance test; enumerates move paths." in
    Cmd.info "perft"
      ~version:"%%VERSION%%"
      ~doc
      ~exits:Cmd.Exit.defaults
      ~man:[]

  let cmd = Cmd.v info t
end

module Gui = struct
  let go pos white black delay depth nodes book no_validate =
    let validate = not no_validate in
    let pos = Chess.Position.Fen.of_string_exn pos ~validate in
    let white = choose_player white in
    let black = choose_player black in
    let delay = match white, black with
      | Some _, Some _ when Float.(delay <= 0.0) -> Fun.id
      | Some _, Some _ -> fun () -> ignore @@ Unix.sleepf delay
      | _ -> Fun.id in
    Caml_player.limits := Some (Chess.Search.Limits.create ~depth ~nodes ());
    Base.Option.iter book ~f:(fun filename ->
        let open Core_kernel in
        let t = Time.now () in
        Caml_player.book := Some (Chess.Book.create filename);
        let t' = Time.now () in
        let sec = Time.(Span.to_sec @@ diff t' t) in
        printf "Loaded book %s in %fs\n\n%!" filename sec);
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
    Arg.(value & opt float 0.0 (info ["delay"] ~docv:"SECONDS" ~doc))

  let depth =
    let doc = "Depth limit for search (only used by the 'caml' player). \
               The value must be positive and non-zero." in
    Arg.(value & opt int 8 (info ["depth"] ~docv:"NUMBER" ~doc))

  let nodes =
    let doc = "Node limit for search (only used by the 'caml' player). \
               The value must be positive and non-zero." in
    Arg.(value & opt (some int) None (info ["nodes"] ~docv:"NUMBER" ~doc))

  let book =
    let doc = "Path of opening book in Polyglot .bin format \
               (only used by the 'caml' player)" in
    Arg.(value & opt (some string) None (info ["book"] ~docv:"FILE" ~doc))


  let t = Term.(
      const go $
      pos $
      white $
      black $
      delay $
      depth $
      nodes $
      book $
      no_validate)

  let info =
    let doc = "Runs the testing GUI." in
    Cmd.info "gui"
      ~version:"%%VERSION%%"
      ~doc
      ~exits:Cmd.Exit.defaults
      ~man:[]

  let cmd = Cmd.v info t
end

module Uci = struct
  let go debug = Uci.go ~debug

  let info =
    let doc = "Runs the UCI loop with the 'caml' player." in
    Cmd.info "uci"
      ~version:"%%VERSION%%"
      ~doc
      ~exits:Cmd.Exit.defaults
      ~man:[]

  let debug =
    let doc = "Enables debug logging to stdout." in
    Arg.(value & flag (info ["debug"] ~doc))

  let t = Term.(const go $ debug)

  let cmd = Cmd.v info t
end

let default = Term.(ret @@ const @@ `Help (`Pager, None))

let info =
  let man = man_players () in
  let doc = "A UCI-compatible chess engine." in
  Cmd.info "ocamlchess" ~doc ~exits:Cmd.Exit.defaults ~man

let () = exit @@ Cmd.eval @@ Cmd.group ~default info [
    Perft.cmd;
    Gui.cmd;
    Uci.cmd;
  ]
