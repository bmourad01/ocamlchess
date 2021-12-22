open Chess

val go :
  Position.t ->
  white:Player.t option ->
  black:Player.t option ->
  delay:(unit -> unit) ->
  unit
