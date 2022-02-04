type draw =
  | Stalemate
  | Insufficient_material
  | Mutual_agreement
  | Threefold_repetition
  | Fifty_move_rule
[@@deriving compare, equal, sexp]

module Draw : sig
  type t = draw [@@deriving compare, equal, sexp]

  include Base.Comparable.S with type t := t
end

type result =
  | Checkmate of Piece.color
  | Resigned of Piece.color
  | Draw of draw
  | Ongoing
[@@deriving compare, equal, sexp]

module Result : sig
  type t = result [@@deriving compare, equal, sexp]

  include Base.Comparable.S with type t := t
end

type t [@@deriving compare, equal, sexp]

include Base.Comparable.S with type t := t

val event : t -> string option
val site : t -> string option
val date : t -> Core_kernel.Date.t option
val round : t -> int option
val white : t -> string option
val black : t -> string option
val result : t -> result
val moves : t -> Position.legal list

val empty :
  ?event:string option ->
  ?site:string option ->
  ?date:Core_kernel.Date.t option ->
  ?round:int option ->
  ?white:string option ->
  ?black:string option ->
  unit ->
  t

val add_move : t -> Position.legal -> result -> t
val to_string : t -> string
