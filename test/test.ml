

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
    { coords = (4.4, 42.31); kind = Stop; touching = [0; 1] }
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
  center        : float * float ;
  zoom_factor   : float
}

let d_options = {
  width = 1000 ;
  height = 700 ;
  resolution = 1.0 ; (* 1mx1m squares *)
  center = (0.0, 0.0) ; (* goes from -1.0 to 1.0 *)
  zoom_factor = 1.0
}

let open_window { width ; height } =
  Graphics.open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height) ;
  Graphics.set_window_title "traffic-lights"

let render_nodes nodes (ext_left, ext_right, ext_top, ext_bottom) { width ; height ; resolution ; zoom_factor } =
  let render_node { coords ; kind } =
    let x = ((fst coords -. ext_left) /. ext_right) *. (float_of_int width) in
    let x = int_of_float x in
    let y = ((snd coords -. ext_bottom) /. ext_top) *. (float_of_int height) in
    let y = int_of_float y in
    print_endline (string_of_int x) ;
    print_endline (string_of_int y) ;
    let color = match kind with
      | Continue -> Graphics.cyan
      | Stop -> Graphics.red
      | Light _ -> Graphics.green
    in
    Graphics.set_line_width 3 ;
    Graphics.set_color color ;
    Graphics.draw_circle x y (int_of_float (zoom_factor *. 10.0))
  in
  List.iter render_node nodes

let rec update_options_with_events options =
  match Graphics.key_pressed () with
  | false -> options
  | true -> (
    let options = (
      match Graphics.read_key () with
      | 'x' -> { width = options.width ; height = options.height ; resolution = options.resolution ; center = options.center ; zoom_factor = options.zoom_factor *. 1.1 } (* options with { zoom_factor = options.zoom_factor *. 1.1 } *)
      | 'z' -> { width = options.width ; height = options.height ; resolution = options.resolution ; center = options.center ; zoom_factor = options.zoom_factor /. 1.1 } (* options with { zoom_factor = options.zoom_factor /. 1.1 } *)
      | c -> print_char c ; print_endline "" ; options
      | _ -> options
    ) in
    update_options_with_events options
  )

let rec main_loop nodes bounds options =
  let start = Unix.gettimeofday () in
  let options = update_options_with_events options in
  render_nodes nodes bounds options ;
  (* 60 fps *)
  let ms_per_frame = 1.0 /. 60.0 in
  let elapsed = start -. Unix.gettimeofday () in
  if elapsed < ms_per_frame then Unix.sleepf (ms_per_frame -. elapsed) ;
  main_loop nodes bounds options

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
