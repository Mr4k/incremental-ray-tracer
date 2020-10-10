open Core
open Graphics

let () = open_graph ""

type vec3 = { x : float; y : float; z : float }

type point = vec3

type color = vec3

type ray = { direction : vec3; origin : vec3 }

let aspect_ratio : float = 16.0 /. 9.0

let viewpoint_height = 2.0

let viewpoint_width = viewpoint_height *. aspect_ratio

let focal_length = 1.0

let screen_width = 480

let screen_height = 270

let lower_left_corner =
  {
    x = -.viewpoint_width /. 2.0;
    y = -.viewpoint_height /. 2.0;
    z = -.focal_length;
  }

let horizontal = { x = viewpoint_width; y = 0.0; z = 0.0 }

let vertical = { x = 0.0; y = viewpoint_height; z = 0.0 }

let dot a b = (a.x *. b.x) +. (a.y *. b.y) +. (a.z *. b.z)

let scale a b = { x = a.x *. b; y = a.y *. b; z = a.z *. b }

let add a b = { x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z }

let sub a b = { x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z }

let len a = dot a a

let norm a =
  let l = len a in
  { x = a.x /. l; y = a.y /. l; z = a.z /. l }

type sphere = { center : vec3; radius : float }

type hit_record = { t : float; position : vec3; face_normal : vec3 }

let check_collision_with_sphere ray sphere t_min t_max =
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
          }
    | None -> None
  else None

let white = { x = 1.0; y = 1.0; z = 1.0 }

let sky_dark_blue = { x = 0.5; y = 0.7; z = 1.0 }

let check_collision_with_world ray world t_min t_max =
  let _, _, record =
    List.fold world ~init:(t_min, t_max, None) ~f:(fun acc sphere ->
        let t_min, t_max, prev = acc in
        match check_collision_with_sphere ray sphere t_min t_max with
        | None -> (t_min, t_max, prev)
        | Some hit_record -> (t_min, hit_record.t, Some hit_record))
  in
  record

let world =
  [
    { center = { x = 0.0; y = 0.0; z = -1.0 }; radius = 0.5 };
    { center = { x = 0.0; y = -100.5; z = -1.0 }; radius = 100.0 };
  ]

let trace r =
  match check_collision_with_world r world 0.0 10000.0 with
  | None ->
      let n = norm r.direction in
      let t = 0.5 *. (n.y +. 1.0) in
      add (scale white (1.0 -. t)) (scale sky_dark_blue t)
  | Some hit_record ->
      scale (add hit_record.face_normal { x = 1.0; y = 1.0; z = 1.0 }) 0.5

let screen_coords =
  Array.init (screen_width * screen_height) ~f:(fun i ->
      let x = float_of_int (i % screen_width) /. float_of_int screen_width in
      let y =
        1.0 -. (float_of_int (i / screen_width) /. float_of_int screen_height)
      in
      (x, y))

let samples_per_pixel = 100

(* low hanging fruit replace samples with a generator*)
let samples = Array.init samples_per_pixel ~f:(fun i -> i)

let fsamples_per_pixel = float_of_int samples_per_pixel

let () = Random.init 0

let resulting_colors =
  Array.map screen_coords ~f:(fun screen_coord ->
      let x, y = screen_coord in
      scale
        (Array.fold samples ~init:{ x = 0.0; y = 0.0; z = 0.0 }
           ~f:(fun color _ ->
             let jitter_x = Random.float (1.0 /. float_of_int screen_width) in
             let jitter_y = Random.float (1.0 /. float_of_int screen_height) in
             add
               (trace
                  {
                    origin = { x = 0.0; y = 0.0; z = 0.0 };
                    direction =
                      add lower_left_corner
                        (add
                           (scale horizontal (x +. jitter_x))
                           (scale vertical (y +. jitter_y)));
                  })
               color))
        (1.0 /. fsamples_per_pixel))

let pack_color_to_int color =
  let r = int_of_float (Float.min (color.x *. 255.0) 255.0) in
  let g = int_of_float (Float.min (color.y *. 255.0) 255.0) in
  let b = int_of_float (color.z *. 255.0) in
  b lor ((g lor (r lsl 8)) lsl 8)

let () =
  let packed_color_array =
    Array.map resulting_colors ~f:(fun color -> pack_color_to_int color)
  in
  let color_matrix =
    Array.init screen_height ~f:(fun i ->
        Array.sub packed_color_array ~pos:(i * screen_width) ~len:screen_width)
  in
  let img = make_image color_matrix in
  draw_image img 0 0

let () =
  while 0 < 1 do
    ()
  done
