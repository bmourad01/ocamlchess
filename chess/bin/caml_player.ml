open Core_kernel
open Chess

let limits = ref None

let update_history m pos =
  Position.hash pos |> Map.update m ~f:(function
      | Some n -> n + 1
      | None -> 1)

let print_res res =
  let pv =
    Search.Result.pv res |>
    List.map ~f:(fun m -> Position.San.of_legal m) |>
    String.concat ~sep:" " in
  let score = match Search.Result.score res with
    | Mate n when n < 0 -> sprintf "lose (mate in %d ply)" (-n)
    | Mate n -> sprintf "win (mate in %d ply)" n
    | Cp (s, None) -> Int.to_string s
    | Cp (s, Some `lower) -> sprintf "%d (lower-bound)" s
    | Cp (s, Some `upper) -> sprintf "%d (upper-bound)" s in
  printf "Time taken: %dms\n%!" @@ Search.Result.time res;
  printf "Principal variation: %s\n%!" pv;
  printf "Depth: %d\n%!" @@ Search.Result.depth res;
  printf "Nodes evaluated: %d\n%!" @@ Search.Result.nodes res;
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
    let res = Search.go ~root ~limits ~history ~tt () in
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
