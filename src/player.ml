open Core_kernel

type limits = {
  depth : int;
  nodes : int;
}

exception No_moves

type choose = Position.legal_moves -> Position.legal_move

type t = {
  choose : choose;
  limits : limits option;
} [@@deriving fields]

type create = ?limits:limits option -> unit -> t

let create ~choose = fun ?(limits = None) () -> {choose; limits}

let best_moves moves ~eval =
  let open Option.Monad_infix in
  List.filter_map moves ~f:(fun m -> eval m >>| fun score -> (m, score)) |>
  List.sort ~compare:(fun (_, score) (_, score') ->
      Int.compare score' score) |>
  List.fold_until ~init:([], 0) ~finish:fst
    ~f:(fun (acc, score') (m, score) ->
        match acc with
        | [] -> Continue (m :: acc, score)
        | _ when score' > score -> Stop acc
        | _ -> Continue (m :: acc, score'))
