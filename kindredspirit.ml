open! Core.Std
open! Async.Std

let main () =
  Pixel_pusher.start_discovery_listener ()

let () =
  let cmd =
    Command.async_basic ~summary:"Pixel Pusher driver"
      Command.Spec.(empty)
      (fun () -> main ())
  in
  Command.run cmd
