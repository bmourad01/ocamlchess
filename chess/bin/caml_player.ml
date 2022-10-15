open Core_kernel [@@warning "-D"]
open Chess

let limits = ref None

let incr m pos = Hashtbl.incr m @@ Position.hash pos

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
  | Cp (cp, Exact) -> Format.fprintf ppf "%d%!" cp
  | Cp (cp, Lower) -> Format.fprintf ppf "%d (lower-bound)%!" cp
  | Cp (cp, Upper) -> Format.fprintf ppf "%d (upper-bound)%!" cp

let print_res res =
  Format.printf "Time taken: %dms\n%!" @@ Search.Result.time res;
  Format.printf "Principal variation: %a\n%!" pp_pv @@ Search.Result.pv res;
  Format.printf "Depth: %d\n%!" @@ Search.Result.depth res;
  Format.printf "Selective depth: %d\n%!" @@ Search.Result.seldepth res;
  Format.printf "Nodes searched: %d\n%!" @@ Search.Result.nodes res;
  Format.printf "Score: %a\n%!" pp_score @@ Search.Result.score res;
  Format.printf "\n%!"

let book = ref None

let try_book in_book root frequency =
  if not in_book then None
  else Option.bind !book ~f:(fun book ->
      match Book.lookup book root with
      | Ok m ->
        Format.printf "Book move: %a\n\n%!" Position.San.pp m;
        let new_pos = Position.Child.self m in
        incr frequency new_pos;
        Some m
      | Error err ->
        Format.printf "%a; using search\n\n%!" Book.Error.pp err;
        None)

let choice (frequency, tt, in_book) moves =
  let root = Position.Child.parent @@ List.hd_exn moves in
  incr frequency root;
  match try_book in_book root frequency with
  | Some m -> m, (frequency, tt, true)
  | None ->
    let limits = Option.value_exn !limits in
    let res = Search.go ~root ~limits ~frequency ~tt ~iter:print_res () in
    let m = Search.Result.best_exn res in
    let new_pos = Position.Child.self m in
    incr frequency new_pos;
    m, (frequency, tt, false)

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
