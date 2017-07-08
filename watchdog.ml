open Core
open Async

let watchdog_port = 9901
let summary = "sets a safe color if the VJ disappears"

let iter_pixels f =
  List.iter (Pixel_pusher.get_strips ()) ~f:(fun strip ->
    List.iter (List.range 0 strip.Pixel_pusher.Strip.strip_length) ~f:(fun index ->
      f strip index))

let iter_pixels_set_color f =
  iter_pixels (fun strip index ->
    Pixel_pusher.Strip.set_pixel strip ~color:(f ()) ~index)

let muted_until = ref Time.epoch

let start_watchdog_listener () =
  let addr = `Inet (Unix.Inet_addr.bind_any, watchdog_port) in
  Async_extra.Udp.bind addr >>= fun socket ->
  printf "*** Watchdog listener started on port %d\n%!" watchdog_port;
  let fd = Async_extra.Import.Socket.fd socket in
  Async_extra.Udp.recvfrom_loop fd (fun _buf addr ->
    muted_until := Time.add (Time.now ()) (sec 5.);
    printf "*** Received reset from %s; muted until %s\n%!"
      (Async_extra.Import.Socket.Address.Inet.to_string addr)
      (Time.to_string !muted_until))
  
let watchdog () =
  don't_wait_for (start_watchdog_listener ());
  Pixel_pusher.start () >>= fun uph ->
  Clock.every (sec 1.) (fun () ->
    if Time.(<) (Time.now ()) !muted_until then ()
    else begin
      iter_pixels_set_color (fun () -> Color.green);
      Pixel_pusher.send_updates uph
    end);
  Deferred.never ()

let () =
  let command =
    Command.async ~summary Command.Spec.empty (fun () -> watchdog ())
  in
  Command.run command
