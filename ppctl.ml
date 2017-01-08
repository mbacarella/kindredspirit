open Core.Std
open Async.Std

let summary = "Pixel Pusher control util"

let main color () =
  don't_wait_for (Pixel_pusher.start_discovery_listener ());
  let rec loop () =
    List.iter (Pixel_pusher.get_strips ()) ~f:(fun strip ->
      List.iter (List.range 0 strip.Pixel_pusher.Strip.strip_length) ~f:(fun index ->
	Pixel_pusher.Strip.set_pixel strip ~color:(Color.of_string color) ~index));
    Pixel_pusher.send_updates ();
    Clock.after (sec 0.016) >>= fun () ->
    loop ()
  in
  loop ()
    
let () =
  let cmd =
    Command.async_basic ~summary
      Command.Spec.(
	empty +> flag "-color" (required string) ~doc:" hex or color name")
      (fun color () -> main color ())
  in
  Command.run cmd
