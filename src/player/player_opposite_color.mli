(** Creates the player that prefers to put its pieces on squares opposite its
    color, such that the number of pieces on these squares is maximized. Ties
    are broken randomly. *)
val create : Player.create
