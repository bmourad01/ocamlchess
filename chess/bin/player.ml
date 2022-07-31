open Core_kernel
open Chess

module Child = Position.Child

exception No_moves
exception Invalid_move of Position.t * Position.child

type 'a choice = 'a -> Position.child list -> Position.child * 'a

type 'a t = {
  choice : 'a choice;
  name   : string;
  desc   : string;
  state  : 'a;
} [@@deriving fields]

type e = T : 'a t -> e

let create = Fields.create

let choose {choice; state; _} pos =
  match Position.children pos with
  | [] -> raise No_moves
  | moves ->
    let m, state = choice state moves in
    if List.mem moves m ~equal:Child.equal then m, state
    else raise @@ Invalid_move (pos, m)

let update player state = {player with state}
