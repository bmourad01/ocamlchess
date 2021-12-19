open Core_kernel

(** The player that chooses moves randomly. *)
class cls ?(limits = None) () = object
  inherit Player.cls ~limits ()

  method move pos =
    match List.random_element @@ Position.legal_moves pos with
    | None -> raise Player.No_moves
    | Some m -> m
end
