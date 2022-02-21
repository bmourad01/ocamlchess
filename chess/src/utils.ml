open Core_kernel

module Prng = struct
  type t = < rand : int64 >

  let create : int64 -> t = function
    | 0L -> invalid_arg "Seed must be nonzero"
    | seed -> object
      val mutable state = seed
      method rand =
        let s = state in
        let s = Int64.(s lxor (s lsr 12)) in
        let s = Int64.(s lxor (s lsl 25)) in
        let s = Int64.(s lxor (s lsr 27)) in
        state <- s;
        Int64.(s * 2685821657736338717L)
    end
end
