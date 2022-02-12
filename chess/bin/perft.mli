open Chess

(** [go depth pos] runs the performance test for position [pos]
    at depth [depth]. Prints out move enumerations and other
    statistics. *)
val go : int -> Position.t -> unit
