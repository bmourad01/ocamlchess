open Chess
open Core_kernel

let limits = ref None

let update_history m pos =
  Position.hash pos |> Map.update m ~f:(function
      | Some n -> n + 1
      | None -> 1)

let print_res res sec =
  let pv =
    Search.Result.pv res |>
    List.map ~f:(fun m -> Position.San.of_legal m) |>
    String.concat ~sep:" " in
  let score =
    let s = Search.Result.score res in
    if s = Search.max_score then "win"
    else if s = -Search.max_score then "lose"
    else Int.to_string s in
  printf "Time taken: %fs\n%!" sec;
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
        printf "Book move: %s\n\n%!" @@ Position.San.of_legal m;
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
    let limits = Option.value_exn !limits in
    let search = Search.create ~limits ~root ~history ~tt in
    let t = Time.now () in
    let res = Search.go search in
    let t' = Time.now () in
    let sec = Time.(Span.to_sec @@ diff t' t) in
    let m = Search.Result.best res in
    print_res res sec;
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