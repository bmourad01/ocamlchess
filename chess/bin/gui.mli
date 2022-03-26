open Chess

(** [run pos ~white ~black ~delay] starts the GUI with position [pos].

    [white] and [black] are optional AI players. If either are none, then
    they are human players and thus require manual interaction with the GUI.

    [delay] is a function that will artificially delay the main game loop
    in between halfmoves. This is useful for when AI players are playing
    against eachother, and thus may make moves very quickly.
*)
val run :
  Position.t ->
  white:Player.e option ->
  black:Player.e option ->
  delay:(unit -> unit) ->
  unit
