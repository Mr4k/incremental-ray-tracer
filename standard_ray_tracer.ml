open Core
open Graphics
open Ray_utils

let () =
  open_graph "";
  resize_window screen_width screen_height

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
  let lst =
    List.init num_spheres ~f:(fun _ ->
        let radius = Random.float_range 0.05 1.0 in
        {
          center =
            {
              x = Random.float_range (-5.0) 5.0;
              y = radius -. 0.5;
              z = Random.float_range (-5.0) 5.0;
            };
          radius;
        })
  in
  List.append lst
    [ { center = { x = 0.0; y = -100.5; z = -1.0 }; radius = 100.0 } ]

let _materials_list =
  let lst =
    List.init num_spheres ~f:(fun _ ->
        let fuzziness = Random.float 1.0 in
        M'
          {
            albedo =
              {
                x = Random.float 1.0;
                y = Random.float 1.0;
                z = Random.float 1.0;
              };
            fuzziness;
          })
  in
  List.append lst [ L' { color = { x = 0.8; y = 0.8; z = 0.0 } } ]

let materials = Array.of_list _materials_list

let rec trace r n =
  if n <= 0 then { x = 0.0; y = 0.0; z = 0.0 }
  else
    match check_collision_with_world r world 0.001 10000.0 with
    | None, _ ->
        let n = norm r.direction in
        let t = 0.5 *. (n.y +. 1.0) in
        add (scale white (1.0 -. t)) (scale sky_dark_blue t)
    | Some hit_record, material_index -> (
        (*scale (add hit_record.face_normal { x = 1.0; y = 1.0; z = 1.0 }) 0.5*)
        let material = materials.(material_index) in
        match material with
        | L' lambertian ->
            let direction =
              add (random_unit_vector ()) hit_record.face_normal
            in
            hammard
              (trace { origin = hit_record.position; direction } (n - 1))
              lambertian.color
        | M' metal ->
            let direction =
              add
                (scale (random_unit_vector ()) metal.fuzziness)
                (reflect r.direction hit_record.face_normal)
            in
            if Float.(dot direction hit_record.face_normal > 0.0) then
              hammard
                (trace { origin = hit_record.position; direction } (n - 1))
                metal.albedo
            else { x = 0.0; y = 0.0; z = 0.0 } )

let samples = Array.init samples_per_pixel ~f:(fun i -> i)

let fsamples_per_pixel = float_of_int samples_per_pixel

let time f =
  let t = Unix.gettimeofday () in
  let res = f () in
  Printf.printf "Execution time: %f seconds\n" (Unix.gettimeofday () -. t);
  res

let resulting_colors =
  time (fun _ ->
      Array.map screen_coords ~f:(fun screen_coord ->
          let x, y = screen_coord in
          scale
            (Array.fold samples ~init:{ x = 0.0; y = 0.0; z = 0.0 }
               ~f:(fun color _ ->
                 let jitter_x =
                   Random.float (1.0 /. float_of_int screen_width)
                 in
                 let jitter_y =
                   Random.float (1.0 /. float_of_int screen_height)
                 in
                 add
                   (trace
                      (get_ray_from_camera (x +. jitter_x) (y +. jitter_y)
                         scene_camera)
                      max_depth)
                   color))
            (1.0 /. fsamples_per_pixel)))

let () =
  Gc.print_stat stdout;
  print_endline "";
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
