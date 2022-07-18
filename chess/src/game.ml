open Core_kernel

module Legal = Position.Legal

type declared_draw = [
  | `Mutual_agreement
  | `Threefold_repetition
  | `Fifty_move_rule
] [@@deriving compare, equal, sexp]

type automatic_draw = [
  | `Stalemate
  | `Insufficient_material
  | `Fivefold_repetition
  | `Seventy_five_move_rule
] [@@deriving compare, equal, sexp]

type draw = [declared_draw | automatic_draw] [@@deriving compare, equal, sexp]

module Draw = struct
  module T = struct  
    type t = draw [@@deriving compare, equal, sexp]
  end

  include T
  include Comparable.Make(T)

  let is_declared = function
    | #declared_draw -> true
    | _ -> false

  let is_automatic = function
    | #automatic_draw -> true
    | _ -> false
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
    event   : string option;
    site    : string option;
    date    : Date.t option;
    round   : int option;
    white   : string option;
    black   : string option;
    result  : result;
    start   : Position.t;
    moves   : Position.legal list;
    history : int Int64.Map.t;
  } [@@deriving compare, equal, sexp, fields]
end

include T

let moves game = List.rev game.moves

let position game = match game.moves with
  | prev :: _ -> Legal.child prev
  | [] -> game.start

let is_over game = match game.result with
  | Ongoing -> false
  | _ -> true

let result_of pos history =
  let c = Position.active pos in
  let in_check = Position.in_check pos in
  let no_moves = List.is_empty @@ Position.legal_moves pos in
  let hash = Position.hash pos in
  if in_check && no_moves then
    Checkmate c
  else if no_moves then
    Draw `Stalemate
  else if Position.is_insufficient_material pos then
    Draw `Insufficient_material
  else if Position.halfmove pos >= 150 then
    Draw `Seventy_five_move_rule
  else if Map.find_exn history hash >= 5 then
    Draw `Fivefold_repetition
  else Ongoing 

let create
    ?(event = None)
    ?(site = None)
    ?(date = None)
    ?(round = None)
    ?(white = None)
    ?(black =  None)
    ?(start = Position.start)
    () =
  let history = Int64.Map.singleton (Position.hash start) 1 in
  let result = result_of start history in
  Fields.create ~event ~site ~date ~round ~white ~black
    ~result ~start ~moves:[] ~history

exception Game_over
exception Invalid_parent
exception Invalid_threefold
exception Invalid_fifty_move

let add_move ?(resigned = None) ?(declared_draw = None) game legal =
  if not @@ is_over game then
    let moves =
      let prev = position game in
      let parent = Legal.parent legal in
      (* We do hard comparison instead of checking the hashes because we also
         care about the halfmove and fullmove clocks. *)
      if Position.(prev <> parent) then raise Invalid_parent
      else legal :: game.moves in
    let pos = Legal.child legal in
    let hash = Position.hash pos in
    let history =
      Map.update game.history hash ~f:(function
          | Some n -> n + 1
          | None -> 1) in
    (* In the following order, we check:
       1. Automatic end to the game
       2. Resignation
       3. Declared draw (must be validated). *)
    let result = 
      let result = result_of pos history in
      match result with
      | Ongoing -> begin
          match resigned with
          | Some c -> Resigned c
          | None ->  match declared_draw with
            | None -> result
            | Some draw -> match draw with
              | `Mutual_agreement -> Draw (draw :> draw)
              | `Threefold_repetition ->
                if Map.find_exn history hash >= 3
                then Draw (draw :> draw) else raise Invalid_threefold
              | `Fifty_move_rule ->
                if Position.halfmove pos >= 100
                then Draw (draw :> draw) else raise Invalid_fifty_move
        end
      | _ -> result in
    {game with moves; result; history}
  else raise Game_over

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
  adds "\"]\n";
  (* FEN *)
  if Position.(game.start <> start) then begin
    adds "[FEN \"";
    adds @@ Position.Fen.to_string game.start;
    adds "\"]\n";
  end;
  (* Moves *)
  addc '\n';
  moves game |> List.fold ~init:true ~f:(fun full legal ->
      let parent = Legal.parent legal in
      if full then adds @@ sprintf "%d." @@ Position.fullmove parent;
      let full =
        (* We may have started this game from a position where black
           moves first. *)
        let active = Position.active parent in
        if full && Piece.Color.(active = Black)
        then (adds ".."; full) else not full in
      adds @@ Position.San.of_legal legal;
      addc ' ';
      full) |> ignore;
  adds result;
  Buffer.contents buf

include Comparable.Make(T)
