open Core_kernel

type limits = {
  depth : int;
  nodes : int;
}

exception No_moves

type t = <
  choose : Position.legal_moves -> Position.legal_move;
  limits : limits option;
  name : string;
>

type create = ?limits:limits option -> unit -> t

let best_moves moves ~eval =
  let open Option.Monad_infix in
  List.filter_map moves ~f:(fun m -> eval m >>| fun score -> (m, score)) |>
  List.sort ~compare:(fun (_, a) (_, b) -> Int.compare b a) |>
  List.fold_until ~init:([], 0) ~finish:fst
    ~f:(fun (acc, score') (m, score) -> match acc with
        | [] -> Continue (m :: acc, score)
        | _ when score' > score -> Stop acc
        | _ -> Continue (m :: acc, score'))
