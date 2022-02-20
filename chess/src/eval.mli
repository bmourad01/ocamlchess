(** Returns a score for a position. The score represents the relative
    advantage of the active player. Note that draws and checkmates
    are not explored here, but instead are considered when searching
    positions (see the [Search]) module. *)
val go : Position.t -> int
