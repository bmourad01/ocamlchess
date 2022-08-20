open Core_kernel

module Child = Position.Child

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

  let pp ppf = function
    | Checkmate White -> Format.fprintf ppf "0-1%!"
    | Checkmate Black -> Format.fprintf ppf "1-0%!"
    | Resigned White -> Format.fprintf ppf "0-1%!"
    | Resigned Black -> Format.fprintf ppf "1-0%!"
    | Draw _ -> Format.fprintf ppf "1/2-1/2%!"
    | Ongoing -> Format.fprintf ppf "%%!"
end

module T = struct
  type t = {
    event    : string option;
    site     : string option;
    date     : Date.t option;
    round    : int option;
    white    : string option;
    black    : string option;
    result   : result;
    start    : Position.t;
    moves    : Move.t list;
    position : Position.t;
    history  : int Int64.Map.t;
  } [@@deriving compare, equal, sexp, fields]
end

include T

let moves game = List.rev game.moves

let is_over game = match game.result with
  | Ongoing -> false
  | _ -> true

let result_of pos history =
  let c = Position.active pos in
  let in_check = Position.in_check pos in
  let no_moves = List.is_empty @@ Position.children pos in
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
    ~result ~start ~moves:[] ~history ~position:start

exception Game_over
exception Invalid_parent
exception Invalid_threefold
exception Invalid_fifty_move

let add_move ?(resigned = None) ?(declared_draw = None) game child =
  if not @@ is_over game then
    let moves =
      let prev = position game in
      let parent = Child.parent child in
      (* We do hard comparison instead of checking the hashes because we also
         care about the halfmove and fullmove clocks. *)
      if Position.equal prev parent
      then Child.move child :: game.moves
      else raise Invalid_parent in
    let pos = Child.self child in
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
    {game with moves; result; history; position = Child.self child}
  else raise Game_over

let pp_option_string ppf = function
  | None -> Format.fprintf ppf "%!"
  | Some s -> Format.fprintf ppf "%s%!" s

let pp_option_date ppf = function
  | None -> Format.fprintf ppf "%!"
  | Some d -> Format.fprintf ppf "%a%!" Date.pp d

let pp ppf game =
  Format.fprintf ppf "[Event \"%a\"]\n%!" pp_option_string game.event;
  Format.fprintf ppf "[Site \"%a\"]\n%!" pp_option_string game.site;
  Format.fprintf ppf "[Date \"%a\"]\n%!" pp_option_date game.date;
  Format.fprintf ppf "[White \"%a\"]\n%!" pp_option_string game.white;
  Format.fprintf ppf "[Black \"%a\"]\n%!" pp_option_string game.black;
  Format.fprintf ppf "[Result \"%a\"]\n%!" Result.pp game.result;
  if not Position.(equal game.start start) then
    Format.fprintf ppf "[FEN \"%a\"]\n%!" Position.pp game.start;
  Format.fprintf ppf "\n%!";
  moves game |> List.fold ~init:(game.start, true) ~f:(fun (pos, full) m ->
      let child = Position.make_move_exn pos m in
      let parent = Child.parent child in
      if full then
        Format.fprintf ppf "%d.%!" @@ Position.fullmove parent;
      let full =
        (* We may have started this game from a position where black
           moves first. *)
        let active = Position.active parent in
        if full && Piece.Color.(active = Black) then begin
          Format.fprintf ppf "..%!"; full
        end else not full in
      Format.fprintf ppf "%a %!" Position.San.pp child;
      Child.self child, full) |> ignore;
  Format.fprintf ppf "%a\n%!" Result.pp game.result

let to_string = Format.asprintf "%a\n%!" pp

include Base.Comparable.Make(T)
