open! Core.Std
open! Async.Std

let title = "Kindred Spirit Lighting Console"
  
let display_width = 1600.0
let display_height = 880.0

let target_fps = 50.
let display_interval = sec (1. /. target_fps)
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

let text ?(color=(1.0, 1.0, 1.0)) ?size ~x ~y s =
  let font =
    match size with
    | None | Some `sm -> Glut.BITMAP_HELVETICA_10
    | Some `md -> Glut.BITMAP_HELVETICA_12
    | Some `lg -> Glut.BITMAP_HELVETICA_18
  in
  GlMat.push ();
  GlMat.load_identity ();
  GlDraw.color color;
  GlPix.raster_pos ~x ~y ();
  String.iter ~f:(fun c ->
    let c = Char.to_int c in
    Glut.bitmapCharacter ~font ~c) s;
  GlMat.pop ()

 
module List_pane = struct
  let height = 10.
  let width = 150.
  let mouse_over_animation () =
    let x = 0. in
    List.findi (Animation.all ()) ~f:(fun i _a ->
      let y = height *. (Float.of_int i) in
      !mouse_x >= x && !mouse_x < width && !mouse_y >= y && !mouse_y < y +. height)
  let display () =
    GlDraw.color (0.1, 0.1, 0.1);
    GlDraw.rect (0., 0.) (width, display_height);
    let x = 0. in
    let hovered_a = mouse_over_animation () in
    List.iteri (Animation.all ()) ~f:(fun i a ->
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
      GlDraw.vertex ~x:coord.Coordinate.x ~y:(130. -. coord.Coordinate.y)
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
  text ~size:`md ~x ~y:(display_height -. 10.) s;
  a.Animation.update a;
  display_model ~center:(x +. 350., display_height -. 350.) (Option.value_exn a.Animation.model);
  Color_picker.display color_picker

module Preview_pane = struct
  let x = List_pane.width
  let y = 0.
  let width = (display_width -. x) /. 2.0 -. 5.
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

module Pixel_pusher_status = struct
  let display model =
    let module Controller_report = Pixel_pusher.Controller_report in
    let expected_controllers = model.Model.controller_ids in    
    let seen_controllers =
      List.fold_left (Pixel_pusher.get_controllers ()) ~init:Int.Map.empty ~f:(fun map c ->
        Map.add map ~key:c.Controller_report.controller_id ~data:c)
    in
    let box_width = 15. in
    let box_height = 15. in
    let now = Time.now () in
    List.iteri (Set.to_list expected_controllers) ~f:(fun index controller_id ->
      let x = display_width -. box_width in
      let y = display_height -. (30. +. ((Float.of_int index) *. box_height)) in
      let rect_color =
        match Map.find seen_controllers controller_id with
        | None -> (1.0, 0.0, 0.0)    
        | Some c ->
          let span = Time.diff now c.Controller_report.last_beacon in
          if Time.Span.(<) span (sec 1.0) then (0.0, 1.0, 0.0)
          else (0.0, 1.0, 0.0)
      in
      GlDraw.color rect_color;
      GlDraw.rect (x, (y-.2.0)) (x +. 10., y +. 10.);
      text ~color:(0.0, 0.0, 0.0) ~x ~y (sprintf " %d" controller_id))
end
  
let send_frame_to_pixel_pushers a send_updates_t =
  match a.Animation.model with
    | None -> failwithf "animation %s is not initiatilized" a.Animation.name ()
    | Some model ->
      let strip_map = Pixel_pusher.get_strips_as_map () in
      List.iter model.Model.virtual_pixels ~f:(fun vp ->
        let controller_id = vp.Virtual_pixel.controller_id in
        let strip_id = vp.Virtual_pixel.strip_id in
        let key = (controller_id, strip_id) in
	match Map.find strip_map key with
	  | None -> ()
	  | Some strip ->
	    let index = Virtual_pixel.pixel_id vp in
	    let color = Virtual_pixel.color vp in
	    Pixel_pusher.Strip.set_pixel strip ~color ~index);
      Pixel_pusher.send_updates_from_non_async_thread send_updates_t

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

let next_display_time = ref (Time.now ())
let display ~model ~send_updates_t () =
  if Time.( < ) (Time.now ()) !next_display_time then ()
  else begin
    let start_display_time = Time.now () in
    next_display_time := Time.add start_display_time display_interval;
    handle_mouse_events model;
    GlClear.clear [`color];
    List_pane.display ();
    Preview_pane.display ();
    Live_pane.display ();
    Pixel_pusher_status.display model;
    let now = Time.now () in
    let fps = 1.0 /. Time.Span.to_sec (Time.diff now !last_display_time) in
    last_display_time := now;
    text ~x:(display_width -. 40.) ~y:(display_height -. 10.) (sprintf "fps: %.0f" fps);
    Gl.flush ();
    Glut.swapBuffers ();
    send_frame_to_pixel_pushers !Live_pane.loaded_animation send_updates_t;
    begin
      let now = Time.now () in
      if Time.( > ) now !next_display_time then
	printf "!! took longer than %s to display %s !!\n%!"
	  (Time.Span.to_string display_interval)
	  (Time.diff now start_display_time |> Time.Span.to_string)
    end;
    incr num_display_calls
  end
    
let tick () =
  (* We time the refreshes ourselves via this idle func rather than
     having GLUT do it because this pause call has the important
     side-effect of surrendering time to the Async thread. 
     It also saves us from burning 100% CPU (if we don't have to). *)
  let span = Time.diff !next_display_time (Time.now ()) in
  if Time.Span.(>) span Time.Span.zero then
    Time.pause span;
  Glut.postRedisplay ()

let key_input model ~key ~x:_ ~y:_ =
  match Char.of_int key with
    | None -> printf "wat (key code: %d)\n" key
    | Some '\r' -> Live_pane.load_animation_from_preview ()
    | Some '\n' -> printf "received line feed?!\n"
    | Some ' ' -> rotating := not !rotating
    | Some 'D' -> Model.print model
    | Some 'Q' ->
      printf "*** Shutting down on 'Q' command\n";
      Shutdown.shutdown 0
    | Some char ->
      printf "*** key input: %c\n" char

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

let gl_main model send_updates_t =
  let _ = Glut.init ~argv:Sys.argv in
  Glut.initDisplayMode ~depth:true ~double_buffer:true ();
  let _ = Glut.createWindow ~title in

  Glut.setCursor Glut.CURSOR_LEFT_ARROW;
  
  Glut.positionWindow ~x:0 ~y:0;
  GlMat.mode `projection;
  GlMat.load_identity ();
  GlMat.ortho ~x:(0.0, display_width) ~y:(0.0, display_height) ~z:(-300.0, 300.0);
  GlMat.mode `modelview;
  GlMat.load_identity ();

  Color_picker.gl_init ();

  Glut.reshapeFunc ~cb:reshape;
  Glut.displayFunc ~cb:(display ~model ~send_updates_t);
  Glut.idleFunc ~cb:(Some tick);
  Glut.keyboardFunc ~cb:(key_input model);
  Glut.mouseFunc ~cb:mouse_click_event;
  Glut.motionFunc ~cb:mouse_motion;
  Glut.passiveMotionFunc ~cb:mouse_motion;
  Glut.mainLoop ()

let main () =
  Pixel_pusher.start ()
  >>= fun send_updates_t ->
  Model.load "model.csv"
  >>= fun model ->
  Preview_pane.loaded_animation := (Animation.init Animation.off model);
  Live_pane.loaded_animation := (Animation.init Animation.off model);
  In_thread.run (fun () -> gl_main model send_updates_t)

let () =
  let cmd =
    Command.async ~summary:title
      Command.Spec.(empty +> flag "-test-animations" no_arg ~doc:"test individual strips for signal/power")
      (fun test_animations () ->
	if test_animations then
	  Animation.mode := `test;
	main ())
  in
  Command.run cmd
