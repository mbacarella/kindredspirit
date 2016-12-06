open! Core.Std
open! Async.Std

let main () =
  don't_wait_for (Pixel_pusher.update_loop 128);
  Pixel_pusher.start_discovery_listener ()

let () =
  let cmd =
    Command.async_basic ~summary:"Pixel Pusher driver"
      Command.Spec.(empty)
      (fun () -> main ())
  in
  Command.run cmd
