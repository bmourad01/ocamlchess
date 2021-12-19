(** Chooses a player from a string. The valid choices are:

    - ["random"]: the player that chooses random moves

    Any other choices will raise [Invalid_argument].
 *)
val choose :
  ?limits:Player.limits option ->
  string ->
  Player.t
