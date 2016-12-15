open! Core.Std
open! Async.Std

let display_width = 1600.0
let display_height = 834.0
  
(* Pixel Pushers "guarantee" 60 hz updates.  Set our target FPS to something lower
   so we don't drop packets/clip.  *)
let target_fps = 45.0
let display_interval = Time.Span.( / ) (sec 1.0) target_fps
let num_display_calls = ref 0
let last_display_time = ref Time.epoch

let reshape ~w ~h =
  (* The actual "world" stays fixed to display_width and display_height
     even if the user resizes. *)
  GlDraw.viewport ~x:0 ~y:0 ~w ~h

let display () =
  GlClear.clear [`color];
  GlDraw.color (1.0, 1.0, 0.0);
  GlDraw.rect (0.0, 100.0) (display_width, 200.0);
  GlDraw.rect (100.0, 0.0) (200.0, display_height); 
  Gl.flush ();
  Glut.swapBuffers ();
  last_display_time := Time.now ();
  num_display_calls := !num_display_calls + 1
    
let key_input ~key ~x:_ ~y:_ =
  match Char.of_int key with
  | None -> printf "wat (key code: %d)\n" key
  | Some char ->
    printf "*** key input: %c\n" char

let num_ticks = ref 0
let last_ticks_print_time = ref (Time.now ())
let last_ticks_print_num = ref 0

let set_random_pixels () =
  List.iter (Pixel_pusher.get_strips ()) ~f:(fun strip ->
    List.iter (List.range 0 strip.Pixel_pusher.Strip.strip_length) ~f:(fun index ->
      Pixel_pusher.Strip.set_pixel strip ~color:(Pixel_pusher.Color.rand ())
	~index))

let tick () =
  num_ticks := !num_ticks + 1;
  if Time.diff (Time.now ()) !last_ticks_print_time > (sec 10.0) then begin
    printf "%s idle calls: %d ticks/sec, frames: %d\n%!"
      (Time.now () |> Time.to_string) (!num_ticks - !last_ticks_print_num)
      !num_display_calls;
    last_ticks_print_time := Time.now ();
    last_ticks_print_num := !num_ticks
  end;
  (* This no-op sleep is here to make sure GLUT doesn't starve Async. *)
  Core.Std.Unix.sleep 0;
  if Time.Span.(>) (Time.diff (Time.now ()) !last_display_time) display_interval then begin
    set_random_pixels ();
    Pixel_pusher.send_updates ();
    Glut.postRedisplay ()
  end
    
let gl_main () =
  let _ = Glut.init ~argv:Sys.argv in
  Glut.initDisplayMode ~depth:true ~double_buffer:true ();
  let _ = Glut.createWindow ~title:"Kindred Spirit Lighting Console" in

  Glut.positionWindow ~x:0 ~y:0;
  GlMat.mode `projection;
  GlMat.load_identity ();
  GlMat.ortho ~x:(0.0, display_width) ~y:(0.0, display_height) ~z:(-100.0, 100.0);
  GlMat.mode `modelview;
  GlMat.load_identity ();

  Glut.reshapeFunc ~cb:reshape;
  Glut.displayFunc ~cb:display;
  Glut.idleFunc ~cb:(Some tick);
  Glut.keyboardFunc ~cb:key_input;
  Glut.fullScreen ();
  Glut.mainLoop ()
  
let main () =
  don't_wait_for (In_thread.run gl_main);
  Pixel_pusher.start_discovery_listener ()

let () =
  let cmd =
    Command.async_basic ~summary:"Pixel Pusher driver"
      Command.Spec.(empty)
      (fun () -> main ())
  in
  Command.run cmd
