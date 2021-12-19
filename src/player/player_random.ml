open Core_kernel

(** The player that chooses moves randomly. *)
class t ?limits () = object
  inherit Player.t ~limits ()

  method move pos =
    match List.random_element @@ Position.legal_moves pos with
    | None -> raise Player.No_moves
    | Some m -> m
end
