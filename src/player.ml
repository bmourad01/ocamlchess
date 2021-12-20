(** The interface used by a computer player. *)

open Core_kernel

(** Search limits. *)
type limits = {
  depth : int;
  nodes : int;
}

(** Raised when no legal moves are available for the player. *)
exception No_moves

(** The player interface. *)
type t = <
  move : Position.t -> Position.legal_move;
  name : string;
>

(** [player ~limits] will construct a player object with search limits
    [limits] *)
class virtual cls ?(limits = None) () = object
  (** The search limits, if they exist. *)
  val limits : limits option = limits

  (** [move pos] picks the move to play according to the player's evaluation
      of the position [pos]. This is a move/position pair, and it is guaranteed
      to be a legal move. If the current position has no legal moves for the
      player, then [No_moves] may be raised. *)
  method virtual move : Position.t -> Position.legal_move

  (** The player's name. *)
  method virtual name : string

  (** [equal_eval moves ~eval] will take a list of moves, evaluate them with
      [eval], and then return a list of the highest scoring moves (with the
      same score). If [eval] returns [None] for a particular move, then it
      is discarded from the final solution. *)
  method private equal_eval moves ~eval =
    let open Option.Monad_infix in
    List.filter_map moves ~f:(fun m -> eval m >>| fun score -> (m, score)) |>
    List.sort ~compare:(fun (_, score) (_, score') ->
        Int.compare score' score) |>
    List.fold_until ~init:([], 0) ~finish:fst
      ~f:(fun (acc, score') (m, score) ->
          match acc with
          | [] -> Continue (m :: acc, score)
          | _ ->
            if score' > score then Stop acc else Continue (m :: acc, score'))
end
