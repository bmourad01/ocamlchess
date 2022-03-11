open Cmdliner

module Caml_player = struct
  open Chess
  open Core_kernel

  let limits = Search.Limits.create ~depth:7 ~nodes:None ()

  let update_history m pos =
    Position.hash pos |> Map.update m ~f:(function
        | Some n -> n + 1
        | None -> 1)

  let print_res res =
    let pv =
      Search.Result.pv res |>
      List.map ~f:(fun m -> Position.San.of_legal m) |>
      String.concat ~sep:" " in
    let score =
      let s = Search.Result.score res in
      if s = Int.max_value then "inf"
      else if s = (-Int.max_value) then "-inf"
      else Int.to_string s in
    printf "Principal variation: %s\n%!" pv;
    printf "Depth: %d\n%!" @@ Search.Result.depth res;
    printf "Nodes searched: %d\n%!" @@ Search.Result.evals res;
    printf "Score: %s\n%!" score;
    printf "\n%!"

  let book = ref None

  let try_book in_book root history =
    if not in_book then None
    else Option.bind !book ~f:(fun book ->
        match Book.lookup book root with
        | Ok m ->
          printf "Book move: %s\n%!" @@ Position.San.of_legal m;
          let new_pos = Position.Legal.new_position m in
          let history = update_history history new_pos in
          Some (m, history)
        | Error (Book.Error.Position_not_found _) -> None
        | Error err ->
          failwithf "Opening book error: %s" (Book.Error.to_string err) ())

  let choice (history, tt, in_book) moves =
    let root = Position.Legal.parent @@ List.hd_exn moves in
    let history = update_history history root in
    match try_book in_book root history with
    | Some (m, history) -> m, (history, tt, true)
    | None ->
      let search = Search.create ~limits ~root ~history ~tt in
      let res = Search.go search in
      let m = Search.Result.best res in
      print_res res;
      let new_pos = Position.Legal.new_position m in
      let history = update_history history new_pos in
      m, (history, tt, false)

  let name = "caml"
  let create () =
    let player =
      let state = Int64.Map.empty, Search.Tt.create (), true in
      Player.create ~choice ~state ~name
        ~desc:"The flagship player, based on traditional game tree search and \
               evaluation." in
    Player.T player
end

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
  let go pos white black delay book no_validate =
    let validate = not no_validate in
    let pos = Chess.Position.Fen.of_string_exn pos ~validate in
    let white = choose_player white in
    let black = choose_player black in
    let delay = match white, black with
      | Some _, Some _ when Float.(delay <= 0.0) -> Fun.id
      | Some _, Some _ -> fun () -> ignore @@ Unix.sleepf delay
      | _ -> Fun.id in
    Base.Option.iter book ~f:(fun filename ->
        Caml_player.book := Some (Chess.Book.create filename));
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

  let book =
    let doc = "Path of opening book in Polyglot .bin format \
               (only used by the 'caml' player)" in
    Arg.(value & opt (some string) None (info ["book"] ~docv:"FILE" ~doc))

  let t = Term.(const go $ pos $ white $ black $ delay $ book $ no_validate)

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
