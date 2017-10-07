

type coords = float * float

(* NODES *)
type node_index = int
type light_state = Red | Green
type node_kind = Continue | Stop | Light of light_state
type node = {
  coords      : coords ;
  kind        : node_kind ;
  touching    : node_index list (* array ? *)
}

(* VEHICLES *)
type vehicle_kind = Car [@@deriving show]
type vehicle = {
  from_node         : node_index ;
  to_node           : node_index ;
  kind              : vehicle_kind ;
  position          : coords ;
  remaining_nodes   : node_index list ; (* array with current_node_index field ? *)
  momentum          : float * float (* speed vector, maybe slower acceleration when going uphill, and faster when downhill *)
}

let get_all_nodes () =
  [
    { coords = (4.5, 42.42); kind = Continue; touching = [1; 2] };
    { coords = (4.7, 42.57); kind = Continue; touching = [0; 2] };
    { coords = (4.4, 42.31); kind = Stop; touching = [0; 1] } ;
    { coords = (4.2, 42.31); kind = Light Green; touching = [] }
  ]

let make_step vehicle =
  match vehicle with
  | { kind; position; remaining_nodes; momentum } -> (
    (* vehicle with ({ momentum = ((fst momentum *. 1.1), (snd momentum *. 1.1)) }) *)
    { from_node = vehicle.from_node; to_node = vehicle.to_node; kind; position; remaining_nodes; momentum = ((fst momentum *. 1.000001), (snd momentum *. 1.000001)) }
  )

exception No_nodes

let find_node nodes maximize =
  let rec _find_node current = function
    | [] -> current
    | h :: tl -> _find_node (if maximize h > maximize current then h else current) tl
  in
  match nodes with
  | [] -> raise No_nodes
  | h :: tl -> _find_node h tl


(* ============= DISPLAY ============= *)

type display_options = {
  width         : int ;
  height        : int ;
  resolution    : float ;
  decal         : float * float ;
  zoom_factor   : float
}

let d_options = {
  width = 1300 ; (* 1300 *)
  height = 800 ; (* 500 *)
  resolution = 0.9 ; (* 0.9mx0.9m squares, a car is approximately 1.8 * 4.5, so 2 squares by 5 *)
  decal = (0.0, 0.0) ;
  zoom_factor = 0.8
}

let open_window { width ; height } =
  Graphics.open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height) ;
  Graphics.set_window_title "traffic-lights" ;
  Graphics.auto_synchronize false

let render_nodes nodes (ext_left, ext_right, ext_top, ext_bottom) { width ; height ; resolution ; decal ; zoom_factor } =
  let x_max_distance = ext_right -. ext_left in
  let y_max_distance = ext_top -. ext_bottom in
  let min_win_border = float_of_int (min width height) in
  let max_distance = max x_max_distance y_max_distance in
  let x_ratio = x_max_distance /. max_distance in
  let y_ratio = y_max_distance /. max_distance in
  let max_x_pixels, max_y_pixels = x_ratio *. min_win_border, y_ratio *. min_win_border in
  let remaining_pixels_x = width - int_of_float max_x_pixels in
  let remaining_pixels_y = height - int_of_float max_y_pixels in
  let render_node { coords ; kind } =
    let x = fst coords -. ext_left in
    let x = x /. max_distance in
    let x = x +. fst decal in

    (* move point to bottom left, to apply zoom_factor *)
    let x = x -. (x_ratio /. 2.0) in
    let x = x *. zoom_factor in
    let x = x +. (x_ratio /. 2.0) in

    let x = int_of_float (x *. min_win_border) in
    let x = x + (remaining_pixels_x / 2) in

    let y = snd coords -. ext_bottom in
    let y = y /. max_distance in
    let y = y +. snd decal in

    (* move point to bottom left, to apply zoom_factor *)
    let y = y -. (y_ratio /. 2.0) in
    let y = y *. zoom_factor in
    let y = y +. (y_ratio /. 2.0) in

    let y = int_of_float (y *. min_win_border) in
    let y = y + (remaining_pixels_y / 2) in

    let color = match kind with
      | Continue -> Graphics.cyan
      | Stop -> Graphics.magenta
      | Light Green -> Graphics.green
      | Light Red -> Graphics.red
    in
    Graphics.set_line_width 3 ;
    Graphics.set_color color ;
    Graphics.draw_circle x y (int_of_float (zoom_factor *. 10.0))
  in
  List.iter render_node nodes

let rec update_options_with_events options =
  match Graphics.key_pressed () with
  | false -> (false, options)
  | true -> (
    let stop, options = (
      let min_win_border = min options.width options.height in
      let zoom_factor = options.zoom_factor in
      let decal_x, decal_y = options.decal in
      let move_decal = 30.0 /. zoom_factor /. float_of_int min_win_border in
      match Graphics.read_key () with
      (* WASD *)
      | 'w' -> (false, { options with decal = (decal_x, decal_y -. move_decal) })
      | 's' -> (false, { options with decal = (decal_x, decal_y +. move_decal) })
      | 'a' -> (false, { options with decal = (decal_x +. move_decal, decal_y) })
      | 'd' -> (false, { options with decal = (decal_x -. move_decal, decal_y) })
      (* ZOOM *)
      | 'x' -> (false, { options with zoom_factor = zoom_factor *. 1.1 })
      | 'z' -> (false, { options with zoom_factor = zoom_factor /. 1.1 })
      (* ESC *)
      | c when int_of_char c = 27 -> (true, options)
      | c -> print_int (int_of_char c) ; print_endline "" ; (false, options)
      | _ -> (false, options)
    ) in
    match stop with
    | true -> (true, options)
    | false -> update_options_with_events options
  )

let rec main_loop nodes bounds options =
  let start = Unix.gettimeofday () in
  let stop, options = update_options_with_events options in
  match stop with
  | true -> ()
  | false -> (
    Graphics.clear_graph () ;
    render_nodes nodes bounds options ;
    Graphics.synchronize () ;
    (* 60 fps *)
    let ms_per_frame = 1.0 /. 60.0 in
    let elapsed = Unix.gettimeofday () -. start in
    Printf.printf "%d %%\n" (int_of_float (elapsed /. ms_per_frame *. 100.0)) ;
    if elapsed < ms_per_frame then Unix.sleepf (ms_per_frame -. elapsed) ;
    main_loop nodes bounds options
  )

(* ============= END DISPLAY ============= *)


let rec n_times car = function
  | 0 -> car
  | n -> n_times (make_step car) (n - 1)

let () =
  let all_nodes = get_all_nodes () in
  let left_most = find_node all_nodes (fun n -> -.(fst n.coords)) in
  let right_most = find_node all_nodes (fun n -> fst n.coords) in
  let top_most = find_node all_nodes (fun n -> snd n.coords) in
  let bottom_most = find_node all_nodes (fun n -> -.(snd n.coords)) in
  let bounds = (fst left_most.coords, fst right_most.coords, snd top_most.coords, snd bottom_most.coords) in
  let my_car = {
    from_node = 0;
    to_node = 1;
    kind = Car;
    position = (4.5, 42.42);
    remaining_nodes = [0; 2; 1];
    momentum = (1.0, 1.0)
  } in
  (* let my_car = n_times my_car 100000000 in *)
  (* print_endline (show_vehicle my_car) ; *)
  open_window d_options ;
  main_loop all_nodes bounds d_options
