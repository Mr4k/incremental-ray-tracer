open Core

module Inc : Incremental.S = Incremental.Make ()

open Inc

let i1 = Var.create 1

let i4 = Var.create 4

let i5 = Var.create 4

let ref_x = ref (Var.watch i1)

let ref_y = ref (Var.watch i1)

let x = bind (Var.watch i4) ~f:(fun _ -> !ref_x)

let y = bind (Var.watch i4) ~f:(fun _ -> !ref_y)

let p = map x ~f:(fun _ -> 0)

let q = map y ~f:(fun y -> -y)

let ref_z = ref q

let z = bind (Var.watch i4) ~f:(fun _ -> !ref_z)

let z_o = observe z

let p_o = observe p

let q_o = observe q

let () =
  for _ = 1 to 1000 do
    Var.set i4 7;
    Var.set i1 8;
    ref_x := Var.watch i1;
    ref_y := Var.watch i1;
    stabilize ();
    Var.set i4 0;
    Var.set i1 2;
    ref_x := Var.watch i1;
    ref_y := p;
    ref_z := q;
    stabilize ();
    (*TODO pair on this*)
    (*print_endline (string_of_int (State.max_height_seen p.state));*)
    print_endline (string_of_int (Observer.value_exn z_o));
    Var.set i4 1;
    Var.set i1 1;
    ref_x := Var.watch i1;
    ref_y := Var.watch i1;
    ref_z := p;
    stabilize ();
    print_endline "here?";
    Var.set i4 2;
    Var.set i1 3;
    ref_x := q;
    stabilize ();
    print_endline (string_of_int (Observer.value_exn z_o));
    print_endline "stable?"
  done
