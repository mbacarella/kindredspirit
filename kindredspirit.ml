open Core.Std
open Async.Std

let title = "Kindred Spirit Lighting Console"
  
let display_width = 1600.0
let display_height = 880.0

let num_display_calls = ref 0
let last_display_time = ref Time.epoch

let mouse_x = ref 0.
let mouse_y = ref 0.
let mouse_down_left = ref false
let mouse_down_right = ref false
  
let reshape ~w ~h =
  (* The actual "world" stays fixed to display_width and display_height
     even if the user resizes. *)
  GlDraw.viewport ~x:0 ~y:0 ~w ~h

let text ?size ~x ~y s =
  let font =
    match size with
    | None | Some `sm -> Glut.BITMAP_HELVETICA_10
    | Some `md -> Glut.BITMAP_HELVETICA_12
    | Some `lg -> Glut.BITMAP_HELVETICA_18
  in
  GlMat.push ();
  GlMat.load_identity ();
  GlDraw.color (1.0, 1.0, 1.0);
  GlPix.raster_pos ~x ~y ();
  String.iter ~f:(fun c ->
    let c = Char.to_int c in
    Glut.bitmapCharacter ~font ~c) s;
  GlMat.pop ()

module Fps = struct
  let fps = ref 0.
  let display () =
    let now = Time.now () in
    let span = Time.Span.to_sec (Time.diff now !last_display_time) in
    fps := 1.0 /. span;
    text ~x:(display_width -. 40.) ~y:(display_height -. 10.) (sprintf "fps: %.0f" !fps)
end
 
module List_pane = struct
  let height = 10.
  let width = 160.
  let mouse_over_animation () =
    let x = 0. in
    List.findi Animation.all ~f:(fun i _a ->
      let y = height *. (Float.of_int i) in
      !mouse_x >= x && !mouse_x < width && !mouse_y >= y && !mouse_y < y +. height)
  let display () =
    GlDraw.color (0.1, 0.1, 0.1);
    GlDraw.rect (0., 0.) (width, display_height);
    let x = 0. in
    let hovered_a = mouse_over_animation () in
    List.iteri Animation.all ~f:(fun i a ->
      let y = display_height -. (height *. (Float.of_int (i+1))) in
      let text () = text ~x ~y a.Animation.name in
      match hovered_a with
	| None -> text ()
	| Some (_, a') ->
	  if a'.Animation.name = a.Animation.name then begin
	    GlDraw.color (0.0, 1.0, 0.0);
	    GlDraw.rect (0.0, y) (width, y +. height)
	  end;
	  text ())
end

let rotating = ref true

let display_model =
  let angle = ref 0. in
  (fun ~center:(x, y) model ->
    GlMat.push ();
    GlMat.load_identity ();
    if !rotating then
      angle := Float.of_int ((succ (Float.to_int !angle)) mod 360);
    GlMat.translate ~x ~y ();
    GlMat.rotate ~angle:!angle ~x:0. ~y:1.0 ~z:0. ();
    GlMat.scale ~x:1.5 ~y:1.5 ~z:1.5 ();
    GlDraw.point_size 2.0;
    GlDraw.begins `points;
    List.iter model.Model.virtual_pixels ~f:(fun vp ->
      GlDraw.color (Color.to_gl vp.Virtual_pixel.color);
      let coord = vp.Virtual_pixel.coord in
      GlDraw.vertex ~x:coord.Coordinate.x ~y:coord.Coordinate.y
	~z:coord.Coordinate.z ());
    GlDraw.ends ();
    GlMat.pop ())
    
let load_colors_from_picker a cp =
  begin match a.Animation.primary_color, Color_picker.get_primary cp with
    | Some _, Some c -> a.Animation.primary_color <- Some c
    | None, None -> ()
    | Some _, None | None, Some _ -> failwithf "broken primary color picker for '%s'" a.Animation.name ()
  end;
  begin match a.Animation.secondary_color, Color_picker.get_secondary cp with
    | Some _, Some c -> a.Animation.secondary_color <- Some c
    | None, None -> ()
    | Some _, None | None, Some _ -> failwithf "broken secondary color picker for '%s'" a.Animation.name ()
  end

let display_animation ~x a tag color_picker =
  load_colors_from_picker a color_picker;
  let s = sprintf "%s: %s" tag a.Animation.name in
  text ~x ~y:(display_height -. 10.) s;
  a.Animation.update a;
  display_model ~center:(x +. 350., display_height -. 350.) (Option.value_exn a.Animation.model);
  Color_picker.display color_picker

module Preview_pane = struct
  let x = List_pane.width
  let y = 0.
  let width = (display_width -. x) /. 2.0
  let loaded_animation = ref Animation.off
  let color_picker = Color_picker.create ~x ~y ~width ~height:180.
  let load_animation a model =
    Color_picker.reset color_picker a;
    loaded_animation := Animation.init a model
  let display () =
    display_animation ~x !loaded_animation "preview" color_picker;
end

module Live_pane = struct
  let x = List_pane.width +. Preview_pane.width +. 10.
  let y = 0.
  let width = display_width -. x
  let loaded_animation = ref Animation.off
  let color_picker = Color_picker.create ~x ~y ~width ~height:180.
  let load_animation_from_preview () =
    let a = !Preview_pane.loaded_animation in
    Color_picker.reset color_picker a;
    loaded_animation := Animation.init a (Option.value_exn a.Animation.model)
  let display () =
    display_animation ~x !loaded_animation "live" color_picker;
end

let send_frame_to_pixel_pushers a =
  match a.Animation.model with
    | None -> failwithf "animation %s is not initiatilized" a.Animation.name ()
    | Some model ->
      let strip_map =
	List.fold_left (Pixel_pusher.get_strips ()) ~init:Map.Poly.empty ~f:(fun map strip ->
	  let controller_id = strip.Pixel_pusher.Strip.controller_id in
	  let strip_id = strip.Pixel_pusher.Strip.strip_number in
	  let key = (controller_id, strip_id) in
	  Map.add map ~key ~data:strip)
      in
      List.iter model.Model.virtual_pixels ~f:(fun vp ->
	let controller_id = vp.Virtual_pixel.controller_id in
	let strip_id = vp.Virtual_pixel.strip_id in
	let key = (controller_id, strip_id) in
	match Map.find strip_map key with
	  | None -> ()
	  | Some strip ->
	    let index = vp.Virtual_pixel.pixel_id in
	    let color = vp.Virtual_pixel.color in
	    Pixel_pusher.Strip.set_pixel strip ~color ~index)

let handle_mouse_events model =
  if !mouse_down_left then begin
    let x = !mouse_x in
    let y = display_height -. !mouse_y in
    Color_picker.maybe_set_primary Preview_pane.color_picker ~x ~y;
    Color_picker.maybe_set_primary Live_pane.color_picker ~x ~y;
    Option.iter (List_pane.mouse_over_animation ()) ~f:(fun (_, a) ->
      Preview_pane.load_animation a model)
  end;
  if !mouse_down_right then begin
    let x = !mouse_x in
    let y = display_height -. !mouse_y in
    Color_picker.maybe_set_secondary Preview_pane.color_picker ~x ~y;
    Color_picker.maybe_set_secondary Live_pane.color_picker ~x ~y
  end

let display ~model () =
  handle_mouse_events model;
  GlClear.clear [`color];
  List_pane.display ();
  Preview_pane.display ();
  Live_pane.display ();
  Fps.display ();
  Gl.flush ();
  Glut.swapBuffers ();
  send_frame_to_pixel_pushers !Live_pane.loaded_animation;
  last_display_time := Time.now ();
  incr num_display_calls

let key_input ~key ~x:_ ~y:_ =
  match Char.of_int key with
    | None -> printf "wat (key code: %d)\n" key
    | Some '\r' -> Live_pane.load_animation_from_preview ()
    | Some '\n' -> printf "received line feed?!\n"
    | Some ' ' -> rotating := not !rotating
    | Some 'Q' ->
      printf "*** Shutting down on 'Q' command\n";
      Shutdown.shutdown 0
    | Some char ->
      printf "*** key input: %c\n" char

let tick () =
  (* We time the refreshes ourselves via this idle func rather than
     having GLUT do it because this pause call has the important
     side-effect of surrendering time to the Async thread. 
     It also saves us from burning 100% CPU (if we don't have to). *)
  (* TODO: dynamically shorten or lengthen span if we're
     consistently failing to hit 60 fps. *)
  let span = sec 0.016666 in
  Core.Std.Time.pause span;
  Glut.postRedisplay ()

let mouse_motion ~x ~y =
  mouse_x := Float.of_int x;
  mouse_y := Float.of_int y

let mouse_click_event ~button ~state ~x ~y =
  mouse_motion ~x ~y;
  match button with
    | Glut.RIGHT_BUTTON ->
      begin match state with
	| Glut.DOWN -> mouse_down_right := true
	| Glut.UP -> mouse_down_right := false
      end
    | Glut.LEFT_BUTTON ->
      begin match state with
	| Glut.DOWN -> mouse_down_left := true
	| Glut.UP -> mouse_down_left := false
      end
    | _ -> ()

let gl_main model =
  let _ = Glut.init ~argv:Sys.argv in
  Glut.initDisplayMode ~depth:true ~double_buffer:true ();
  let _ = Glut.createWindow ~title in

  Glut.positionWindow ~x:0 ~y:0;
  GlMat.mode `projection;
  GlMat.load_identity ();
  GlMat.ortho ~x:(0.0, display_width) ~y:(0.0, display_height) ~z:(-300.0, 300.0);
  GlMat.mode `modelview;
  GlMat.load_identity ();

  Glut.reshapeFunc ~cb:reshape;
  Glut.displayFunc ~cb:(display ~model);
  Glut.idleFunc ~cb:(Some tick);
  Glut.keyboardFunc ~cb:key_input;
  Glut.mouseFunc ~cb:mouse_click_event;
  Glut.motionFunc ~cb:mouse_motion;
  Glut.passiveMotionFunc ~cb:mouse_motion;
  Glut.mainLoop ()

let main () =
  don't_wait_for begin
    Model.load "model.csv"
    >>= fun model ->
    Preview_pane.loaded_animation := (Animation.init Animation.off model);
    Live_pane.loaded_animation := (Animation.init Animation.off model);
    In_thread.run (fun () -> gl_main model)
  end;
  Pixel_pusher.start_discovery_listener ()

let () =
  let cmd =
    Command.async_basic ~summary:title
      Command.Spec.(empty)
      (fun () -> main ())
  in
  Command.run cmd
