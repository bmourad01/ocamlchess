(** The move ordering object.

    [best] will return the best move from the moment that the object was
    created.
    
    [next] will return the next most desirable move, if it exists.
*)
type t = < best : Position.legal; next : Position.legal option >

(** Creates the move ordering object from a list of legal moves. It is assumed
    that each move in the list is derived from the same parent position.

    Raises [Invalid_argument] if the list is empty
*)
val create : Position.legal list -> t
