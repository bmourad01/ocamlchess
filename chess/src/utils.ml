open Core_kernel

module Prng = struct
  type t = unit -> int64

  let create : int64 -> t = function
    | 0L -> invalid_arg "Seed must be nonzero"
    | seed ->
      let state = ref seed in
      fun () ->
        let s = !state in
        let s = Int64.(s lxor (s lsr 12)) in
        let s = Int64.(s lxor (s lsl 25)) in
        let s = Int64.(s lxor (s lsr 27)) in
        state := s;
        Int64.(s * 2685821657736338717L)
end
