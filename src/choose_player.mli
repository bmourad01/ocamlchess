(** Chooses a player from a string. The valid choices are:

    - ["random"]: the player that chooses random moves
    - ["same-color"]: the player that puts its pieces on squares
      of its color, breaking ties randomly.
    - ["opposite-color"]: the player that puts its pieces on squares
      opposite its color, breaking ties randomly.
    - ["cccp"]: this player attempts, in the following order,
      checkmates, checks, captures, and pushes. Here, "pushes" means
      the player will attempt to control (i.e. attack) the greatest
      number of squares.
    - ["huddle"]: this player will move its pieces such that they
      minimize their Chebyshev distance from its king.
    - ["swarm"]: this player will move its pieces such that they
      minimize their Chebyshev distance from the enemy king.

    Any other choices will raise [Invalid_argument].
 *)
val choose :
  ?limits:Player.limits option ->
  string ->
  Player.t
