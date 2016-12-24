open! Core.Std
open! Async.Std

let display_width = 1600.0
let display_height = 834.0
  
(* Pixel Pushers "guarantee" 60 hz updates.  Set our target FPS to something lower
   so we don't drop packets/clip.  *)
let target_fps = 50.0
let display_interval = Time.Span.( / ) (sec 1.0) target_fps
let num_display_calls = ref 0
let last_display_time = ref Time.epoch

let mouse_x = ref 0
let mouse_y = ref 0

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
  let last_update_time = ref Time.epoch
  let last_update_frames = ref 0.
  let fps = ref 0.
  let display () =
    let now = Time.now () in
    let span = Time.diff now !last_update_time |> Time.Span.to_sec in
    if span >= 1.0 then begin
      let num_display_calls = Float.of_int !num_display_calls in
      fps := (num_display_calls -. !last_update_frames) /. span;
      last_update_time := now;
      last_update_frames := num_display_calls
    end;
    text ~x:(display_width -. 40.) ~y:(display_height -. 10.) (sprintf "fps: %.0f" !fps)
end
 
module List_pane = struct
  let height = 10.
  let width = 160.
  let mouse_over_animation () =
    let x = 0. in
    let mouse_x = Float.of_int !mouse_x in
    let mouse_y = Float.of_int !mouse_y in
    List.findi Animation.all ~f:(fun i _a ->
      let y = height *. (Float.of_int i) in
      mouse_x >= x && mouse_x < width && mouse_y >= y && mouse_y < y +. height)
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

let display_model ~center:(x, y) model =
  GlMat.push ();
  GlMat.load_identity ();
  let angle = Float.of_int (!num_display_calls mod 360) in
  GlMat.translate ~x ~y ();
  GlMat.rotate ~angle ~x:0. ~y:1.0 ~z:0. ();
  GlMat.scale ~x:1.5 ~y:1.5 ~z:1.5 ();
  GlDraw.point_size 2.0;
  GlDraw.begins `points;
  List.iter model.Model.virtual_pixels ~f:(fun vp ->
    GlDraw.color (Color.to_gl vp.Model.Virtual_pixel.color);
    let coord = vp.Model.Virtual_pixel.coord in
    GlDraw.vertex ~x:coord.Model.Coordinate.x ~y:coord.Model.Coordinate.y
      ~z:coord.Model.Coordinate.z ());
  GlDraw.ends ();
  GlMat.pop ()

let display_animation ~x a tag =
  let s = sprintf "%s: %s" tag a.Animation.name in
  text ~x ~y:(display_height -. 10.) s;
  a.Animation.update a;
  display_model ~center:(x +. 350., display_height -. 350.) (Option.value_exn a.Animation.model)

let color_picker_kind a =
  match a.Animation.primary_color, a.Animation.secondary_color with
    | None, None -> `NA
    | Some _, None -> `Primary (0, 0)
    | Some _, Some _ -> `Primary_and_secondary ((0, 0), (0, 0))
    | None, Some _ ->
      failwithf "animation '%s' has a secondary color but no primary" a.Animation.name ()

module Preview_pane = struct
  let x = List_pane.width
  let y = 0.
  let width = (display_width -. x) /. 2.0
  let loaded_animation = ref Animation.off
  let color_picker = { Color_picker.x = x; y; width; height=180.; kind=color_picker_kind !loaded_animation }
  let load_animation a model =
    color_picker.Color_picker.kind <- color_picker_kind a;
    loaded_animation := Animation.init a model
  let display () =
    display_animation ~x !loaded_animation "preview";
    Color_picker.display color_picker
end

module Live_pane = struct
  let x = List_pane.width +. Preview_pane.width
  let y = 0.
  let loaded_animation = ref Animation.off
  let width = display_width -. x
  let color_picker = { Color_picker.x = x; y; width; height=180.; kind=color_picker_kind !loaded_animation }
  let load_animation_from_preview () =
    let a = !Preview_pane.loaded_animation in
    color_picker.Color_picker.kind <- Preview_pane.color_picker.Color_picker.kind; 
    loaded_animation := Animation.init a (Option.value_exn a.Animation.model)
  let display () =
    display_animation ~x !loaded_animation "live";
    Color_picker.display color_picker
end

let send_frame_to_pixel_pushers a =
  match a.Animation.model with
    | None -> failwithf "animation %s is not initiatilized" a.Animation.name ()
    | Some _model ->
      ()

let display () =
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
    | Some 'Q' ->
      printf "*** Shutting down on 'Q' command\n";
      Shutdown.shutdown 0
    | Some char ->
      printf "*** key input: %c\n" char

let num_ticks = ref 0
let last_ticks_print_time = ref (Time.now ())
let last_ticks_print_num = ref 0

let set_random_pixels () =
  List.iter (Pixel_pusher.get_strips ()) ~f:(fun strip ->
    List.iter (List.range 0 strip.Pixel_pusher.Strip.strip_length) ~f:(fun index ->
      Pixel_pusher.Strip.set_pixel strip ~color:(Color.rand ())
	~index))

let tick () =
  incr num_ticks;
  if Time.diff (Time.now ()) !last_ticks_print_time > (sec 10.0) then begin
    printf "%s idle calls: %d ticks/sec, frames: %d\n%!"
      (Time.now () |> Time.to_string) (!num_ticks - !last_ticks_print_num)
      !num_display_calls;
    last_ticks_print_time := Time.now ();
    last_ticks_print_num := !num_ticks
  end;
  (* This no-op sleep is here to make sure GLUT doesn't starve Async. *)
  (* TODO: we're currently burning 100% cpu by hooking into the glut idle func
     to give time to async.  Find a way to use GLUT in polling mode, or make
     the GL/GLU/X11 calls to set up the environment directly. *)
  Core.Std.Unix.sleep 0;
  if Time.Span.(>) (Time.diff (Time.now ()) !last_display_time) display_interval then begin
    set_random_pixels ();
    Pixel_pusher.send_updates ();
    Glut.postRedisplay ()
  end

let mouse_clicked ~model ~button ~state ~x ~y =
  mouse_x := x; mouse_y := y;
  (*
    printf "*** mouse clicked: button:%s, state:%s, %d %d\n"
    (match button with
      | Glut.LEFT_BUTTON -> "left"
      | Glut.MIDDLE_BUTTON -> "middle"
      | Glut.RIGHT_BUTTON -> "right"
      | Glut.OTHER_BUTTON i -> sprintf "other:%d" i)
    (match state with
      | Glut.UP -> "up"
      | Glut.DOWN -> "down")
    x y
  *)
  match button, state with
  | Glut.LEFT_BUTTON, Glut.DOWN ->
    begin match List_pane.mouse_over_animation () with
      | None -> ()
      | Some (_, a) ->
	Preview_pane.load_animation a model
    end
  | _, _ -> ()

let gl_main model =
  let _ = Glut.init ~argv:Sys.argv in
  Glut.initDisplayMode ~depth:true ~double_buffer:true ();
  let _ = Glut.createWindow ~title:"Kindred Spirit Lighting Console" in

  Glut.positionWindow ~x:0 ~y:0;
  GlMat.mode `projection;
  GlMat.load_identity ();
  GlMat.ortho ~x:(0.0, display_width) ~y:(0.0, display_height) ~z:(-300.0, 300.0);
  GlMat.mode `modelview;
  GlMat.load_identity ();

  Glut.reshapeFunc ~cb:reshape;
  Glut.displayFunc ~cb:display;
  Glut.idleFunc ~cb:(Some tick);
  Glut.keyboardFunc ~cb:key_input;
  Glut.passiveMotionFunc ~cb:(fun ~x ~y -> mouse_x := x; mouse_y := y);
  Glut.mouseFunc ~cb:(mouse_clicked ~model);
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
    Command.async_basic ~summary:"Pixel Pusher driver"
      Command.Spec.(empty)
      (fun () -> main ())
  in
  Command.run cmd
