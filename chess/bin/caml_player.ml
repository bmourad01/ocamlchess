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
    | Mate n when n < 0 -> sprintf "lose (mate in %d)" (-n)
    | Mate n -> sprintf "win (mate in %d)" n
    | Cp (s, None) -> Int.to_string s
    | Cp (s, Some `lower) -> sprintf "%d (lower-bound)" s
    | Cp (s, Some `upper) -> sprintf "%d (upper-bound)" s in
  printf "Time taken: %dms\n%!" @@ Search.Result.time res;
  printf "Principal variation: %s\n%!" pv;
  printf "Depth: %d\n%!" @@ Search.Result.depth res;
  printf "Maximum ply reached: %d\n%!" @@ Search.Result.seldepth res;
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
        let new_pos = Position.Legal.child m in
        let history = update_history history new_pos in
        Some (m, history)
      | Error (Book.Error.Position_not_found _) -> None
      | Error (Book.Error.No_moves _) -> None
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
    let new_pos = Position.Legal.child m in
    let history = update_history history new_pos in
    Zobrist.Table.age tt;
    m, (history, tt, false)

let name = "caml"
let capacity = 0x40000

module Slot = Zobrist.Table.Slot

(* Replace entries with the same key, otherwise prefer depth. *)
let replace ~prev entry key =
  let open Search.Tt.Entry in
  let prev_entry = Slot.entry prev in
  Zobrist.equal_key key (Slot.key prev) ||
  depth prev_entry <= depth entry

(* Use the entry's depth as an age threshold. *)
let age slot =
  let open Search.Tt.Entry in
  let entry = Slot.entry slot in
  Slot.age slot < depth entry / 2

let create_tt () =
  Zobrist.Table.create ~capacity ~replace ~age

let create () =
  let player =
    let state = Int64.Map.empty, create_tt (), true in
    Player.create ~choice ~state ~name
      ~desc:"The flagship player, based on traditional game tree search and \
             evaluation." in
  Player.T player
