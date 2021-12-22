open Core_kernel

type limits = {
  depth : int;
  nodes : int;
}

exception No_moves

type t = {
  choose : Position.t -> Position.legal_move;
  limits : limits option;
}

type create = ?limits:limits option -> unit -> t

let equal_eval moves ~eval =
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
