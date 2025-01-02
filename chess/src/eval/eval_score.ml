(* To avoid excessive allocations, we'll represent the scores as a
   single integer, with the middle and endgame scores taking up 16
   bits each.

   Since the scores may be negative, we need to explicitly perform a
   sign extension when we try to extract them. Such operations aren't
   as common as simple arithmetic on the evaluations, which can be done
   with the normal integer operations.

   Note that since we need 32 bits of precision, we will use the Int63
   type since it is unboxed on a 64-bit machine.
*)

open Core_kernel [@@warning "-D"]

type t = Int63.t

let bits = 16
let mask = Int63.of_int ((1 lsl bits) - 1)
let smask = Int63.of_int (1 lsl (bits - 1))

let[@inline] signext x = Int63.((x lxor smask) - smask)
let[@inline] make mg eg = Int63.((of_int eg lsl bits) + of_int mg)
let[@inline] mg s = Int63.(to_int_exn (signext (s land mask)))
let[@inline] eg s = Int63.(to_int_exn (signext (((s + smask) lsr bits) land mask)))

module Syntax = struct
  let ($) = make
  let (+$) = Int63.(+)
  let (-$) = Int63.(-)
  let ( *$ ) = Int63.( * )
  let ( ** ) x y = Int63.(x * of_int y)
end
