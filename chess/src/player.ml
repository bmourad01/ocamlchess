open Core_kernel

module Legal = Position.Legal

type limits = {
  depth : int;
  nodes : int;
}

exception No_moves
exception Invalid_move of Position.t * Position.legal

type 'a choice =
  limits option -> 'a -> Position.legal list -> Position.legal * 'a

type 'a t = {
  choice : 'a choice;
  limits : limits option;
  name : string;
  desc : string;
  state : 'a;
} [@@deriving fields]

type e = T : 'a t -> e

let create ?(limits = None) ~choice ~name ~desc ~state () =
  Fields.create ~choice ~limits ~name ~desc ~state

let choose {choice; limits; state; _} pos =
  match Position.legal_moves pos with
  | [] -> raise No_moves
  | moves ->
    let m, state = choice limits state moves in
    if List.mem moves m ~equal:Legal.equal then m, state
    else raise @@ Invalid_move (pos, m)

let update player state = {player with state}
let with_limits player limits = {player with limits}
