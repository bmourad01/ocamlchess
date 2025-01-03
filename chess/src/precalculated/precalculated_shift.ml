(* Magic shift constants. *)

open Core_kernel [@@warning "-D"]

let diagonal = [|
  6; 5; 5; 5; 5; 5; 5; 6;
  5; 5; 5; 5; 5; 5; 5; 5;
  5; 5; 7; 7; 7; 7; 5; 5;
  5; 5; 7; 9; 9; 7; 5; 5;
  5; 5; 7; 9; 9; 7; 5; 5;
  5; 5; 7; 7; 7; 7; 5; 5;
  5; 5; 5; 5; 5; 5; 5; 5;
  6; 5; 5; 5; 5; 5; 5; 6;
|]

let diagonal' = Array.map diagonal ~f:(fun i -> 64 - i)

let orthogonal = [|
  12; 11; 11; 11; 11; 11; 11; 12;
  11; 10; 10; 10; 10; 10; 10; 11;
  11; 10; 10; 10; 10; 10; 10; 11;
  11; 10; 10; 10; 10; 10; 10; 11;
  11; 10; 10; 10; 10; 10; 10; 11;
  11; 10; 10; 10; 10; 10; 10; 11;
  11; 10; 10; 10; 10; 10; 10; 11;
  12; 11; 11; 11; 11; 11; 11; 12;
|]

let orthogonal' = Array.map orthogonal ~f:(fun i -> 64 - i)
