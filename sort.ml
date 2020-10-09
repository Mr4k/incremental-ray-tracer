open Core

module Inc : Incremental.S = Incremental.Make ()
open Inc

let arr = Array.init 8 ~f:(fun i -> (Var.watch (Var.create i)))
let z = reduce_balanced arr ~f:(fun a -> a) ~reduce:(fun a b -> max a b)
let z_o = observe (match z with
| None -> (return 0)
|  Some v -> v )

let () =
  stabilize ();
  print_endline (string_of_int (Observer.value_exn z_o));
