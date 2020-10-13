open Core
open Graphics

module Inc : Incremental.S = Incremental.Make ()

open Inc

let time label f =
  let t = Unix.gettimeofday () in
  let res = f () in
  Printf.printf "Execution time for %s: %f seconds\n" label (Unix.gettimeofday () -. t);
  print_endline "";
  res

let () = open_graph ""

type vec3 = { x : float; y : float; z : float }

type point = vec3

type color = vec3

type ray = { direction : vec3; origin : vec3 }

let dot a b = (a.x *. b.x) +. (a.y *. b.y) +. (a.z *. b.z)

let scale a b = { x = a.x *. b; y = a.y *. b; z = a.z *. b }

let add a b = { x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z }

let sub a b = { x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z }

let hammard a b = { x = a.x *. b.x; y = a.y *. b.y; z = a.z *. b.z }

let len a = Float.sqrt (dot a a)

let cross a b =
  {
    x = (a.y *. b.z) -. (a.z *. b.y);
    y = (a.z *. b.x) -. (a.x *. b.z);
    z = (a.x *. b.y) -. (a.y *. b.x);
  }

let reflect v n = sub v (scale n (2.0 *. dot v n))

let norm a =
  let l = len a in
  { x = a.x /. l; y = a.y /. l; z = a.z /. l }

type sphere = { center : vec3; radius : float }

type hit_record = { t : float; position : vec3; face_normal : vec3; sphere: sphere Var.t }

let check_collision_with_sphere ray sphere_var t_min t_max =
  let sphere = Var.value sphere_var in
  let oc = sub ray.origin sphere.center in
  let a = dot ray.direction ray.direction in
  let half_b = dot oc ray.direction in
  let c = dot oc oc -. (sphere.radius *. sphere.radius) in
  let discriminant = (half_b *. half_b) -. (a *. c) in
  if Float.(discriminant >. 0.0) then
    let root = sqrt discriminant in
    let solution_1 = (-.half_b -. root) /. a in
    let solution_2 = (-.half_b -. root) /. a in
    let solution_1_valid = Float.(solution_1 < t_max && solution_1 > t_min) in
    let solution_2_valid = Float.(solution_2 < t_max && solution_2 > t_min) in
    let temp =
      match (solution_1_valid, solution_2_valid) with
      | true, true -> Some solution_1
      | true, false -> Some solution_1
      | false, true -> Some solution_2
      | false, false -> None
    in

    match temp with
    | Some temp ->
        let hit_pos = add (scale ray.direction temp) ray.origin in
        let outward_normal =
          scale (sub hit_pos sphere.center) (1.0 /. sphere.radius)
        in
        let face_dir =
          if Float.(dot ray.direction outward_normal < 0.0) then 1.0 else -1.0
        in
        Some
          {
            t = temp;
            position = hit_pos;
            face_normal = scale outward_normal face_dir;
            sphere = sphere_var;
          }
    | None -> None
  else None

let zero = { x=0.0;y=0.0;z=0.0 }
let white = { x = 1.0; y = 1.0; z = 1.0 }

let sky_dark_blue = { x = 0.5; y = 0.7; z = 1.0 }

let check_collision_with_world ray world t_min t_max =
  let _, _, record, idx =
    List.foldi world ~init:(t_min, t_max, None, -1) ~f:(fun i acc sphere ->
        let t_min, t_max, prev, prev_i = acc in
        match check_collision_with_sphere ray sphere t_min t_max with
        | None -> (t_min, t_max, prev, prev_i)
        | Some hit_record -> (t_min, hit_record.t, Some hit_record, i))
  in
  (record, idx)

let num_spheres = 100

let () = Random.init 5

let world = 
  let lst = List.init num_spheres ~f:(fun _ ->
    let radius = Random.float_range 0.05 1.0 in
    Var.create { center = {
      x = Random.float_range (-.5.0) 5.0;
      y = radius -. 0.5;
      z = Random.float_range (-.5.0) 5.0;
    }; radius = radius; } ) in
  List.append lst [Var.create { center = { x = 0.0; y = -100.5; z = -1.0 }; radius = 100.0; }]

type lambertian = { color : vec3 }

type metal = { albedo : vec3; fuzziness : float }

type material = L' of lambertian | M' of metal

let _materials_list =
  let lst = List.init num_spheres ~f:(fun _ ->
    let fuzziness = Random.float 1.0 in 
    Var.create (M' { albedo = { x = Random.float 1.0; y = Random.float 1.0; z = Random.float 1.0 }; fuzziness=fuzziness; })) in
  List.append lst [Var.create (L' { color = { x = 0.8; y = 0.8; z = 0.0 } })]

let materials = Array.of_list _materials_list

(* this function contains a gross hack to take a fake argument which by passes ocaml's apparent restriction on recursive no argument functions*)
let random_unit_vector () =
  let a = Random.float (2.0 *. Float.pi) in
  let z = Random.float 2.0 -. 1.0 in
  let r = Float.sqrt (1.0 -. (z *. z)) in
  { x = r *. Float.cos a; y = r *. Float.sin a; z }

let rec trace params n =
    match n with
    | 0 -> return {x=0.0;y=0.0;z=0.0}
    | _ -> let r = map params ~f:(fun params -> (
        let (_, next_ray) = params in
        next_ray)) in
      let hit_incr = map r ~f:(fun r -> check_collision_with_world r world 0.001 10000.0) in
      bind2 params hit_incr ~f:(fun params collision ->
        let (prev_color, prev_ray) = params in 
        match collision with
          | None, _ -> 
              let n = norm prev_ray.direction in
              let t = 0.5 *. (n.y +. 1.0) in
              return (hammard prev_color (add (scale white (1.0 -. t)) (scale sky_dark_blue t)))
          | Some hit_record, material_index ->
              (*scale (add hit_record.face_normal { x = 1.0; y = 1.0; z = 1.0 }) 0.5*)
              let material = materials.(material_index) in
              let next_trace_params = Inc.map (Var.watch material) ~f:(fun material ->
                  match material with
                  | L' lambertian ->
                      let direction =
                        add (random_unit_vector ()) hit_record.face_normal
                      in
                        (hammard prev_color lambertian.color, { origin = hit_record.position; direction })
                  | M' metal ->
                      let direction =
                        add
                          (scale (random_unit_vector ()) metal.fuzziness)
                          (reflect prev_ray.direction hit_record.face_normal)
                      in
                      if Float.(dot direction hit_record.face_normal > 0.0) then
                        (hammard prev_color metal.albedo, { origin = hit_record.position; direction })
                      else
                        ({ x = 0.0; y = 0.0; z = 0.0 }, { origin = hit_record.position; direction })) in
            trace next_trace_params (n - 1)
        )

let aspect_ratio : float = 16.0 /. 9.0

let screen_width = 480

let screen_height = 270

type camera = {
  origin : vec3;
  lower_left_corner : vec3;
  horizontal : vec3;
  vertical : vec3;
}

let make_camera look_from look_at up vfov aspect_ratio =
  let theta = vfov /. 180.0 *. Float.pi in
  let h = Float.tan (theta /. 2.0) in
  let viewpoint_height = 2.0 *. h in
  let viewpoint_width = viewpoint_height *. aspect_ratio in

  let w = norm (sub look_from look_at) in
  let u = norm (cross up w) in
  let v = cross w u in

  let horizontal = scale u viewpoint_width in
  let vertical = scale v viewpoint_height in
  let lower_left_corner =
    scale
      (add
         (add (scale horizontal (1.0 /. 2.0)) (scale vertical (1.0 /. 2.0)))
         w)
      (-1.0)
  in
  { origin = look_from; lower_left_corner; horizontal; vertical }

let get_ray_from_camera u v camera =
  {
    origin = camera.origin;
    direction =
      add
        (add camera.lower_left_corner (scale camera.horizontal u))
        (scale camera.vertical v);
  }

let scene_camera =
  make_camera
    { x = -5.0; y = 5.0; z = 5.0 }
    { x = 0.0; y = 0.0; z = -1.0 }
    { x = 0.0; y = 1.0; z = 0.0 }
    60.0 aspect_ratio

let screen_coords =
  Array.init (screen_width * screen_height) ~f:(fun i ->
      let x = float_of_int (i % screen_width) /. float_of_int screen_width in
      let y =
        1.0 -. (float_of_int (i / screen_width) /. float_of_int screen_height)
      in
      (x, y))

let samples_per_pixel = 20

let max_depth = 10

(* low hanging fruit replace samples with a generator*)
let samples = Array.init samples_per_pixel ~f:(fun i -> i)

let fsamples_per_pixel = float_of_int samples_per_pixel

let resulting_colors =
  Array.map screen_coords ~f:(fun screen_coord ->
      let x, y = screen_coord in
      (*printf "%f %f\n" x y;
      print_endline "";*)
      let pixel_samples =
        Array.map samples ~f:(fun _ ->
            let jitter_x = Random.float (1.0 /. float_of_int screen_width) in
            let jitter_y = Random.float (1.0 /. float_of_int screen_height) in
            trace
              (Inc.return
                 (white, (get_ray_from_camera (x +. jitter_x) (y +. jitter_y)
                    scene_camera)))
              max_depth)
      in
      let result =
        Inc.sum pixel_samples
          ~zero:{ x = 0.0; y = 0.0; z = 0.0 }
          ~add:(fun old element -> add old element)
          ~sub:(fun old element -> sub old element)
      in
      observe result)

let pack_color_to_int color =
  let r = min (int_of_float (Float.sqrt color.x *. 256.0)) 255 in
  let g = min (int_of_float (Float.sqrt color.y *. 256.0)) 255 in
  let b = min (int_of_float (Float.sqrt color.z *. 256.0)) 255 in
  b lor ((g lor (r lsl 8)) lsl 8)

let () =
  printf "first render\n";
  print_endline "";
  time "stabilize" stabilize;
  let n = Inc.State.num_nodes_changed Inc.State.t in
  printf "num nodes created %i" n;
  print_endline ""

let () =
  let i = ref 0 in
  while 0 < 1 do
    Var.set (materials.(!i)) (M' { albedo = { x = Random.float 1.0; y = Random.float 1.0; z = Random.float 1.0 }; fuzziness = Random.float 1.0; });

    printf "modified render fuzziness %i\n " !i;
    print_endline "";
    time "stabilize" stabilize;
    print_endline "";


    let packed_color_array =
      Array.map resulting_colors ~f:(fun color ->
          pack_color_to_int
            (scale (Observer.value_exn color) (1.0 /. fsamples_per_pixel)))
    in
    let color_matrix =
      Array.init screen_height ~f:(fun i ->
          Array.sub packed_color_array ~pos:(i * screen_width) ~len:screen_width)
    in
    let img = make_image color_matrix in
    draw_image img 0 0;
    i := (!i + 1) % num_spheres;
  done
