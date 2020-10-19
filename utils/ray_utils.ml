open Core

let aspect_ratio : float = 16.0 /. 9.0

let screen_width = 480

let screen_height = 270

let samples_per_pixel = 10

let max_depth = 10

(*
  The rest of these functions and structures basically follow those defined in Ray Tracing in One Weekend https://www.realtimerendering.com/raytracing/Ray%20Tracing%20in%20a%20Weekend.pdf
*)

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

type hit_record = { t : float; position : vec3; face_normal : vec3 }

type lambertian = { color : vec3 }

type metal = { albedo : vec3; fuzziness : float }

type material = L' of lambertian | M' of metal

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

let random_unit_vector () =
  let a = Random.float (2.0 *. Float.pi) in
  let z = Random.float 2.0 -. 1.0 in
  let r = Float.sqrt (1.0 -. (z *. z)) in
  { x = r *. Float.cos a; y = r *. Float.sin a; z }

let pack_color_to_int color =
  let r = min (int_of_float (Float.sqrt color.x *. 256.0)) 255 in
  let g = min (int_of_float (Float.sqrt color.y *. 256.0)) 255 in
  let b = min (int_of_float (Float.sqrt color.z *. 256.0)) 255 in
  b lor ((g lor (r lsl 8)) lsl 8)
