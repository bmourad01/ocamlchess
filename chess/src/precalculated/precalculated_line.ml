open Core_kernel [@@warning "-D"]
open Precalculated_common
open Precalculated_piece

let line_tbl =
  let tbl = Array.create Bb.empty ~len:Square.(count * count) in
  for i = 0 to Square.count - 1 do
    let x = Square.of_int_unsafe i in
    for j = 0 to Square.count - 1 do
      let open Bb in
      let y = Square.of_int_unsafe j in
      let bx = bishop x empty in
      let by = if y @ bx then bishop y empty else empty in
      let rx = rook x empty in
      let ry = if y @ rx then rook y empty else empty in
      if by <> empty || ry <> empty then
        tbl.(Int.(i + j * Square.count)) <- (bx & by) + (rx & ry) ++ x ++ y;
    done
  done;
  tbl

let[@inline] line x y =
  let i = Square.to_int x in
  let j = Square.to_int y in
  Array.unsafe_get line_tbl (i + j * Square.count)
