open! Core.Std
open! Async.Std

let reshape ~w ~h =
  printf "*** resize window to %dx%d\n" w h;
  GlDraw.viewport ~x:0 ~y:0 ~w ~h

let display () =
  GlClear.clear [`color];
  GlDraw.color (1.0, 1.0, 0.0);
  GlDraw.rect (100.0, 100.0) (300.0, 200.0);
  Gl.flush ();
  Glut.swapBuffers ()

let key_input ~key ~x:_ ~y:_ =
  match Char.of_int key with
  | None -> printf "wat (key code: %d)\n" key
  | Some char ->
    printf "*** key input: %c\n" char

let gl_main () =
  let _ = Glut.init ~argv:Sys.argv in
  Glut.initDisplayMode ~depth:true ~double_buffer:true ();
  let _ = Glut.createWindow ~title:"Kindred Spirit Lighting Console" in

  Glut.positionWindow ~x:200 ~y:100;
  GlMat.mode `projection;
  GlMat.load_identity ();
  GlMat.ortho ~x:(0.0, 500.0) ~y:(0.0, 600.0) ~z:(-100.0, 100.0);
  GlMat.mode `modelview;
  GlMat.load_identity ();

  Glut.reshapeFunc ~cb:reshape;
  Glut.displayFunc ~cb:display;
  Glut.idleFunc ~cb:(Some Glut.postRedisplay);
  Glut.keyboardFunc ~cb:key_input;
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
