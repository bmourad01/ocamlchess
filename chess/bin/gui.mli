open Chess

val go :
  Position.t ->
  white:Player.e option ->
  black:Player.e option ->
  delay:(unit -> unit) ->
  unit
