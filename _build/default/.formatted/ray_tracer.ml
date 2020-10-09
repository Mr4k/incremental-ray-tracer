open Core
open Graphics

module Inc : Incremental.S = Incremental.Make ()

open Inc

let () = open_graph ""

type vec3 = { x : int; y : int; z : int }

type ray = { direction : vec3; origin : vec3 }

let x = Var.create 13

let y = Var.create 17

let z = map2 (Var.watch x) (Var.watch y) ~f:(fun x y -> x + y)

let z_o = observe z

(*let rec trace = fun x y *)

let () =
  stabilize ();
  print_endline (string_of_int (Observer.value_exn z_o));
  Var.set x 19;
  stabilize ();
  print_endline (string_of_int (Observer.value_exn z_o));
  while 0 < 1 do
    ()
  done
