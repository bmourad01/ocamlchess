(** This module implements the Static Exchange Evaluation algorithm.

    From the chessprogramming wiki:

    "A Static Exchange Evaluation (SEE) examines the consequence of a
     series of exchanges on a single square after a given move, and
     calculates the likely evaluation change (material) to be lost or
     gained"

    For more information, see:
    https://www.chessprogramming.org/Static_Exchange_Evaluation
*)

(** [go m] runs the SEE evaluation for a legal move [m] and returns
    the score. If [m] is not a capture move, then [None] is returned.
    If the capture results in an exchange that is winning for the
    active player, then a positive score is returned. Likewise,
    a negative score is returned if this player ends up at a
    disadvantage. If no advantage is gained by either side, then
    the score is 0. *)
val go : Position.legal -> int option
