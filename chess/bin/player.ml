open Core_kernel
open Chess

module Legal = Position.Legal

exception No_moves
exception Invalid_move of Position.t * Position.legal

type 'a choice = 'a -> Position.legal list -> Position.legal * 'a

type 'a t = {
  choice : 'a choice;
  name   : string;
  desc   : string;
  state  : 'a;
} [@@deriving fields]

type e = T : 'a t -> e

let create = Fields.create

let choose {choice; state; _} pos =
  match Position.legal_moves pos with
  | [] -> raise No_moves
  | moves ->
    let m, state = choice state moves in
    if List.mem moves m ~equal:Legal.equal then m, state
    else raise @@ Invalid_move (pos, m)

let update player state = {player with state}
