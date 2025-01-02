open Core_kernel [@@warning "-D"]

(* This should work, with questionable efficiency. We can't use the
   derivers since the record contains lazy fields.

   We could instead just compare the Zobrist hashes, but we run the
   chance of a collision.
*)
let compare x y = String.compare (Position_fen.to_string x) (Position_fen.to_string y)
let equal x y = compare x y = 0

let t_of_sexp = function
  | Sexp.Atom fen -> Position_fen.of_string_exn fen
  | sexp -> invalid_argf "Invalid position sexp %s" (Sexp.to_string sexp) ()

let sexp_of_t pos = Sexp.Atom (Position_fen.to_string pos)
