open Core_kernel

module Prng = struct
  type t = < rand : int64 >

  let create : int64 -> t = function
    | 0L -> invalid_arg "Seed must be nonzero"
    | seed -> object(self)
      val state = ref seed
      method rand =
        let s = !state in
        let s = Int64.(s lxor (s lsr 12)) in
        let s = Int64.(s lxor (s lsl 25)) in
        let s = Int64.(s lxor (s lsr 27)) in
        state := s; s
    end
end
