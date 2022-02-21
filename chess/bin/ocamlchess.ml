open Cmdliner

module Default_player = struct
  open Chess
  open Core_kernel

  let limits = Search.Limits.create ~depth:6 ~nodes:(Some 1_000_000) ()
  let update_transp m pos =
    Position.hash pos |> Map.update m ~f:(function
        | Some n -> n + 1
        | None -> 1)

  let choice transpositions moves =
    let root = Position.Legal.parent @@ List.hd_exn moves in
    let transpositions = update_transp transpositions root in
    let search = Search.create ~limits ~root ~transpositions in
    let m = Search.(Result.best @@ go search) in
    let new_pos = Position.Legal.new_position m in
    let transpositions = update_transp transpositions new_pos in
    m, transpositions

  let player = Player.create ~choice ~state:Int64.Map.empty ~name:"caml"
      ~desc:"The flagship player, based on traditional game tree search and \
             evaluation."
end

let man_players () =
  Players.register Default_player.player;
  Elo_world.init ();  
  `S "PLAYER" ::
  `Pre "Predefined algorithms for the computer." :: begin
    Players.enumerate () |> Base.List.map
      ~f:(fun Chess.Player.(T player) ->
          `P (Format.sprintf "%s: %s"
                (Chess.Player.name player)
                (Chess.Player.desc player)))
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
  let go pos white black delay no_validate =
    let validate = not no_validate in
    let pos = Chess.Position.Fen.of_string_exn pos ~validate in
    let white = choose_player white in
    let black = choose_player black in
    let delay = match white, black with
      | Some _, Some _ when Float.(delay <= 0.0) -> Fun.id
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

  let t = Term.(const go $ pos $ white $ black $ delay $ no_validate)

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
  let t = Term.(const Uci.go $ const ())

  let info =
    let doc = "Runs the UCI loop." in
    Cmd.info "uci"
      ~version:"%%VERSION%%"
      ~doc
      ~exits:Cmd.Exit.defaults
      ~man:[]

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
