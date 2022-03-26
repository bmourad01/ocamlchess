open Chess

(** [run depth pos] runs the performance test for position [pos]
    at depth [depth]. Prints out move enumerations and other
    statistics. *)
val run : int -> Position.t -> unit
