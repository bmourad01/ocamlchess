open Core_kernel
open Chess

let limits = ref None

let update_history m pos =
  Position.hash pos |> Hashtbl.update m ~f:(function
      | Some n -> n + 1 | None -> 1)

let pp_pv ppf pv =
  let rec aux = function
    | [] -> ()
    | [m] -> Format.fprintf ppf "%a%!" Position.San.pp m
    | m :: ms ->
      Format.fprintf ppf "%a %!" Position.San.pp m;
      aux ms in
  aux pv

let pp_score ppf : Uci.Send.Info.score -> unit = function
  | Mate n when n < 0 -> Format.fprintf ppf "lose (mate in %d)%!" (-n)
  | Mate n -> Format.fprintf ppf "win (mate in %d)%!" n
  | Cp (cp, None) -> Format.fprintf ppf "%d%!" cp
  | Cp (cp, Some `lower) -> Format.fprintf ppf "%d (lower-bound)%!" cp
  | Cp (cp, Some `upper) -> Format.fprintf ppf "%d (upper-bound)%!" cp

let print_res res =
  Format.printf "Time taken: %dms\n%!" @@ Search.Result.time res;
  Format.printf "Principal variation: %a\n%!" pp_pv @@ Search.Result.pv res;
  Format.printf "Depth: %d\n%!" @@ Search.Result.depth res;
  Format.printf "Nodes searched: %d\n%!" @@ Search.Result.nodes res;
  Format.printf "Score: %a\n%!" pp_score @@ Search.Result.score res;
  Format.printf "\n%!"

let book = ref None

let try_book in_book root history =
  if not in_book then None
  else Option.bind !book ~f:(fun book ->
      match Book.lookup book root with
      | Ok m ->
        Format.printf "Book move: %a\n\n%!" Position.San.pp m;
        let new_pos = Position.Legal.child m in
        update_history history new_pos;
        Some m
      | Error err ->
        Format.printf "%a; using search\n\n%!" Book.Error.pp err;
        None)

let choice (history, tt, in_book) moves =
  let root = Position.Legal.parent @@ List.hd_exn moves in
  update_history history root;
  match try_book in_book root history with
  | Some m -> m, (history, tt, true)
  | None ->
    let limits = Option.value_exn !limits in
    let res = Search.go ~root ~limits ~history ~tt ~iter:print_res () in
    let m = Search.Result.best_exn res in
    let new_pos = Position.Legal.child m in
    update_history history new_pos;
    m, (history, tt, false)

let name = "caml"

let create () =
  let player =
    let state = Hashtbl.create (module Int64), Search.Tt.create (), true in
    Player.create ~choice ~state ~name
      ~desc:"The flagship player, based on traditional game tree search and \
             evaluation." in
  Player.T player

let init =
  let once = ref false in
  fun () -> if not !once then begin
      Players.register name create;
      once := true;
    end
