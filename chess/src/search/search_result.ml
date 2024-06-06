open Core_kernel [@@warning "-D"]
open Search_common

module Line = struct
  type t = {
    pv       : Child.t list;
    score    : Uci.Send.Info.score;
    seldepth : int;
  } [@@deriving fields]
end

type line = Line.t

type t = {
  lines : line list;
  nodes : int;
  depth : int;
  time  : int;
} [@@deriving fields]

let pv_exn {lines; _} = List.hd_exn lines
let pv {lines; _} = List.hd lines
let best_exn r = List.hd_exn (pv_exn r).pv
let best r = pv r |> Option.bind ~f:(fun l -> List.hd l.Line.pv)
