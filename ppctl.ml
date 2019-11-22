open Core
open Async

let summary = "Pixel Pusher control util"

let iter_pixels f =
  List.iter (Pixel_pusher.get_strips ()) ~f:(fun strip ->
    List.iter (List.range 0 strip.Pixel_pusher.Strip.strip_length) ~f:(fun index ->
      f strip index))

let iter_pixels_set_color f =
  iter_pixels (fun strip index ->
    Pixel_pusher.Strip.set_pixel strip ~color:(f ()) ~index)

let pp_basic f =
  Pixel_pusher.start ()
  >>= fun _uph ->
  let rec loop () =
    f ();
    Pixel_pusher.send_updates ()
    >>= fun () ->
    Clock.after (sec 0.02) >>= loop
  in
  loop ()

module Set = struct
  module All = struct
    let set_all ~color =
      let _ : Color.t = Color.of_string color in (* fail early in case this color is invalid *)
      pp_basic (fun () -> iter_pixels_set_color (fun () -> Color.of_string color))

    let cmd =
      let open Command.Let_syntax in
      Command.async ~summary:"set all LEDs on all strips"
        [%map_open
          let color = flag "color" (required string) ~doc:"color to set" in
          fun () -> set_all ~color]
  end
  let cmd = Command.group ~summary:"set strip(s) to a color" [ "all", All.cmd ]
end

let () =
  let cmd = Command.group ~summary:"Pixel Pusher control util" [ "set", Set.cmd ] in
  Command.run cmd
