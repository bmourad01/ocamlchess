open Core_kernel [@@warning "-D"]
open Position_common

let white_idx = attacks_len - 2
let black_idx = white_idx + 1

let calculate pos = set_attacks pos @@ lazy begin
    let open Bb.Syntax in
    let occupied = all_board pos in
    let a = Array.init attacks_len ~f:(fun i ->
        if i < white_idx then
          let p = Piece.of_int_unsafe i in
          let c, k = Piece.decomp p in
          collect_piece pos p |>
          List.fold ~init:Bb.empty ~f:(fun acc sq ->
              acc + Pre.attacks sq occupied c k)
        else Bb.empty) in
    let[@inline] get p = Array.unsafe_get a @@ Piece.to_int p in
    let white =
      let open Piece  in
      get white_pawn   +
      get white_knight +
      get white_bishop +
      get white_rook   +
      get white_queen  +
      get white_king  in
    let black =
      let open Piece  in
      get black_pawn   +
      get black_knight +
      get black_bishop +
      get black_rook   +
      get black_queen  +
      get black_king  in
    Array.unsafe_set a white_idx white;
    Array.unsafe_set a black_idx black;
    a
  end

let[@inline] get pos c k =
  let i = attacks_idx c k in
  let a = Lazy.force pos.attacks in
  Array.unsafe_get a i

let[@inline] pawn   pos c = get pos c Pawn
let[@inline] knight pos c = get pos c Knight
let[@inline] bishop pos c = get pos c Bishop
let[@inline] rook   pos c = get pos c Rook
let[@inline] queen  pos c = get pos c Queen
let[@inline] king   pos c = get pos c King

let[@inline] piece pos p =
  let a = Lazy.force pos.attacks in
  Array.unsafe_get a @@ Piece.to_int p

let[@inline] all pos c =
  let a = Lazy.force pos.attacks in
  Array.unsafe_get a (white_idx + Piece.Color.to_int c)
