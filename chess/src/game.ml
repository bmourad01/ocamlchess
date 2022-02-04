open Core_kernel

module Legal = Position.Legal

type draw =
  | Stalemate
  | Insufficient_material
  | Mutual_agreement
  | Threefold_repetition
  | Fivefold_repetition
  | Fifty_move_rule
[@@deriving compare, equal, sexp]

module Draw = struct
  module T = struct  
    type t = draw [@@deriving compare, equal, sexp]
  end

  include T
  include Comparable.Make(T)
end

type result =
  | Checkmate of Piece.color
  | Resigned of Piece.color
  | Draw of draw
  | Ongoing
[@@deriving compare, equal, sexp]

module Result = struct
  module T = struct
    type t = result [@@deriving compare, equal, sexp]
  end

  include T
  include Comparable.Make(T)

  let to_pgn = function
    | Checkmate White -> "0-1"
    | Checkmate Black -> "1-0"
    | Resigned White -> "0-1"
    | Resigned Black -> "1-0"
    | Draw _ -> "1/2-1/2"
    | Ongoing -> "*"
end

module T = struct
  type t = {
    event : string option;
    site : string option;
    date : Date.t option;
    round : int option;
    white : string option;
    black : string option;
    result : result;
    start : Position.t;
    moves : Position.legal list;
  } [@@deriving compare, equal, sexp, fields]
end

include T

let position game = match game.moves with
  | prev :: _ -> Legal.new_position prev
  | [] -> game.start

let is_over game = match game.result with
  | Ongoing -> false
  | _ -> true

let create
    ?(event = None)
    ?(site = None)
    ?(date = None)
    ?(round = None)
    ?(white = None)
    ?(black =  None)
    ?(start = Position.start)
    () = {
  event;
  site;
  date;
  round;
  white;
  black;
  result = Ongoing;
  start;
  moves = [];
}

let add_move game legal result =
  if is_over game then failwith "Game is over, cannot add any more moves"
  else
    let moves =
      let prev = position game in
      let parent = Legal.parent legal in
      (* We do hard comparison instead of checking the hashes because we also
         care about the halfmove and fullmove clocks. *)
      if Position.(prev <> parent) then
        invalid_argf
          "Parent position of move %s differs from the previous \
           move" (Move.to_string @@ Legal.move legal) ()
      else legal :: game.moves in
    {game with moves; result}

let to_string game =
  let buf = Buffer.create 256 in
  let adds = Buffer.add_string buf in
  let addc = Buffer.add_char buf in
  (* Event *)
  adds "[Event \"";
  adds @@ Option.value ~default:"" game.event;
  adds "\"]\n";
  (* Site *)
  adds "[Site \"";
  adds @@ Option.value ~default:"" game.site;
  adds "\"]\n";
  (* Date *)
  adds "[Date \"";
  adds @@ Option.value_map ~default:"" game.date ~f:Date.to_string;
  adds "\"]\n";
  (* White *)
  adds "[White \"";
  adds @@ Option.value ~default:"" game.white;
  adds "\"]\n";
  (* Black *)
  adds "[Black \"";
  adds @@ Option.value ~default:"" game.black;
  adds "\"]\n";
  (* Result *)
  let result = Result.to_pgn game.result in
  adds "[Result \"";
  adds result;
  adds "\"]\n\n";
  (* Moves *)
  List.rev game.moves |> List.iter ~f:(fun legal ->
      let parent = Legal.parent legal in
      let parent_halfmove = Position.halfmove parent in
      let is_new_move = (parent_halfmove land 1) = 0 in
      if is_new_move then adds @@ sprintf "%d. " @@ Position.fullmove parent;
      adds @@ Position.Algebraic.of_legal legal;
      addc ' ');
  adds result;
  Buffer.contents buf

include Comparable.Make(T)
