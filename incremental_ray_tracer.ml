open Core
open Graphics
open Owl
open Ray_utils

module Inc : Incremental.S = Incremental.Make ()

open Inc

let () =
  open_graph "";
  resize_window screen_width screen_height

let zero = { x = 0.0; y = 0.0; z = 0.0 }

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
        let fuzziness = 1.0 in
        Var.create
          (M'
             {
               albedo =
                 {
                   x = Random.float 1.0;
                   y = Random.float 1.0;
                   z = Random.float 1.0;
                 };
               fuzziness;
             }))
  in
  List.append lst [ Var.create (L' { color = { x = 0.8; y = 0.8; z = 0.0 } }) ]

let materials = Array.of_list _materials_list

let materials_watch = Array.map materials ~f:(fun mat -> Var.watch mat)

let rec trace params n =
  match n with
  | 0 -> return { x = 0.0; y = 0.0; z = 0.0 }
  | _ ->
      (*
        Omg so many binds! Isn't allocation killing this program?  
        In my profiling allocation was not actually a problem, I wrote a version where 
        the new nodes weren't reallocated each time and it actually performed worse, 
        I suspect it was because that program used more nodes overall but I'm not really sure.
        I'm not an expert! If you've used Incremental before and have a better way to do this
        please reach out at pstefek.dev@gmail.com
      *)
      bind params ~f:(fun params ->
          let prev_color, ray = params in
          match check_collision_with_world ray world 0.001 10000.0 with
          | None, _ ->
              let n = norm ray.direction in
              let t = 0.5 *. (n.y +. 1.0) in
              return
                (hammard prev_color
                   (add (scale white (1.0 -. t)) (scale sky_dark_blue t)))
          | Some hit_record, material_index ->
              (*scale (add hit_record.face_normal { x = 1.0; y = 1.0; z = 1.0 }) 0.5*)
              let material = materials_watch.(material_index) in
              let next_trace_params =
                Inc.map material ~f:(fun material ->
                    match material with
                    | L' lambertian ->
                        let direction =
                          add (random_unit_vector ()) hit_record.face_normal
                        in
                        ( hammard prev_color lambertian.color,
                          { origin = hit_record.position; direction } )
                    | M' metal ->
                        let direction =
                          add
                            (scale (random_unit_vector ()) metal.fuzziness)
                            (reflect ray.direction hit_record.face_normal)
                        in
                        if Float.(dot direction hit_record.face_normal > 0.0)
                        then
                          ( hammard prev_color metal.albedo,
                            { origin = hit_record.position; direction } )
                        else
                          ( { x = 0.0; y = 0.0; z = 0.0 },
                            { origin = hit_record.position; direction } ))
              in
              trace next_trace_params (n - 1))

let samples = Array.init samples_per_pixel ~f:(fun i -> i)

let fsamples_per_pixel = float_of_int samples_per_pixel

let resulting_colors =
  Array.map screen_coords ~f:(fun screen_coord ->
      let x, y = screen_coord in
      let pixel_samples =
        Array.map samples ~f:(fun _ ->
            let jitter_x = Random.float (1.0 /. float_of_int screen_width) in
            let jitter_y = Random.float (1.0 /. float_of_int screen_height) in
            trace
              (Inc.return
                 ( white,
                   get_ray_from_camera (x +. jitter_x) (y +. jitter_y)
                     scene_camera ))
              max_depth)
      in
      let result =
        Inc.sum pixel_samples
          ~zero:{ x = 0.0; y = 0.0; z = 0.0 }
          ~add:(fun old element -> add old element)
          ~sub:(fun old element -> sub old element)
      in
      observe result)

let () =
  printf "first render\n";
  print_endline "";
  let t = Unix.gettimeofday () in
  stabilize ();
  let elapsed = Unix.gettimeofday () -. t in
  printf "inital elasped %f seconds\n" elapsed;
  let n = Inc.State.num_nodes_changed Inc.State.t in
  printf "num nodes created %i" n;
  print_endline "";
  Gc.print_stat stdout

let () =
  let samples =
    Array.init num_spheres ~f:(fun i ->
        Var.set materials.(i)
          (M'
             {
               albedo =
                 {
                   x = Random.float 1.0;
                   y = Random.float 1.0;
                   z = Random.float 1.0;
                 };
               fuzziness = 0.0;
             });

        let t = Unix.gettimeofday () in
        stabilize ();
        let elapsed = Unix.gettimeofday () -. t in

        (*Update the screen if users is watching in interactive mode*)
        let packed_color_array =
          Array.map resulting_colors ~f:(fun color ->
              pack_color_to_int
                (scale (Observer.value_exn color) (1.0 /. fsamples_per_pixel)))
        in
        let color_matrix =
          Array.init screen_height ~f:(fun i ->
              Array.sub packed_color_array ~pos:(i * screen_width)
                ~len:screen_width)
        in
        let img = make_image color_matrix in
        draw_image img 0 0;

        elapsed)
  in
  printf "num rays %i\n" (samples_per_pixel * screen_width * screen_height);
  printf "50th percentile %f seconds\n" (Stats.percentile samples 50.0);
  printf "95th percentile %f seconds\n" (Stats.percentile samples 95.0)
