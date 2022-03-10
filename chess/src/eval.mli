(** Given the standard valuations of pieces in chess:

    https://en.wikipedia.org/wiki/Chess_piece_relative_value#Standard_valuations

    [material_weight] is the weight given to these valuations relative to the
    entire score of the position.
*)
val material_weight : int

(** Returns a score for a position.

    The score represents the relative advantage of the active player.
    Note that draws and checkmates are not explored here, but instead
    are considered when searching positions (see the [Search] module).
*)
val go : Position.t -> int
