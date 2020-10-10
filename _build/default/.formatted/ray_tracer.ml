open Core
open Graphics

module Inc : Incremental.S = Incremental.Make ()

open Inc

let () = open_graph ""

type vec3 = { x : float; y : float; z : float }

type point = vec3

type color = vec3

type ray = { direction : vec3; origin : vec3 }

let x = Var.create 13

let y = Var.create 17

let z = map2 (Var.watch x) (Var.watch y) ~f:(fun x y -> x + y)

let z_o = observe z

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

let dot a b = Float.sqrt ((a.x *. b.x) +. (a.y *. b.y) +. (a.z *. b.z))

let scale a b = { x = a.x *. b; y = a.y *. b; z = a.z *. b }

let add a b = { x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z }

let len a = dot a a

let norm a =
  let l = len a in
  { x = a.x /. l; y = a.y /. l; z = a.z /. l }

let white = { x = 1.0; y = 1.0; z = 1.0 }

let sky_dark_blue = { x = 0.5; y = 0.7; z = 1.0 }

let trace r =
  let n = norm r.direction in
  let t = 0.5 *. (n.y +. 1.0) in
  add (scale white (1.0 -. t)) (scale sky_dark_blue t)

let screen_coords =
  Array.init (screen_width * screen_height) ~f:(fun i ->
      ( float_of_int (i % screen_width) /. float_of_int screen_width,
        float_of_int (i / screen_width) /. float_of_int screen_height ))

let resulting_colors =
  Array.map screen_coords ~f:(fun screen_coord ->
      let x, y = screen_coord in
      trace
        {
          origin = { x = 0.0; y = 0.0; z = 0.0 };
          direction =
            add lower_left_corner (add (scale horizontal x) (scale vertical y));
        })

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
