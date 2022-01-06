(** Creates the player which avoids, in the following order, checkmating the
    opponent's king, checking the opponent's king, and capturing an enemy
    piece. Failing that, it will attempt to capture the lowest-value piece. *)
val create : Player_intf.create
