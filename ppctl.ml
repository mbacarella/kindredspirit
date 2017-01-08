open Core.Std
open Async.Std

let main () =
  don't_wait_for (Pixel_pusher.start_discovery_listener ());
  let rec loop () =
    List.iter (Pixel_pusher.get_strips ()) ~f:(fun strip ->
      List.iter (List.range 0 strip.Pixel_pusher.Strip.strip_length) ~f:(fun index ->
	Pixel_pusher.Strip.set_pixel strip ~color:Color.green ~index));
    Pixel_pusher.send_updates ();
    Clock.after (sec 1.0) >>= fun () ->
    loop ()
  in
  loop ()
    
let () =
  let cmd =
    Command.async_basic ~summary:"ppctl"
      Command.Spec.(empty)
      (fun () -> main ())
  in
  Command.run cmd
