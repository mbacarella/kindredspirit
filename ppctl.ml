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
  Beat_detection.start () >>= fun () ->
  Pixel_pusher.start () >>= fun uph ->
  let rec loop () =
    f ();
    Pixel_pusher.send_updates uph;
    printf "beat: %f\n%!" !Beat_detection.beat;
    Clock.after (sec 0.02) >>= loop
  in
  loop ()

module Set = struct
  module All = struct
    let set_all ~color =
      ignore (Color.of_string color); (* fail early in case this color is invalid *)
      pp_basic (fun () -> iter_pixels_set_color (fun () -> Color.of_string color))

    let cmd =
      Command.async ~summary:"set all LEDs on all strips"
	Command.Spec.(empty +> anon ("color" %: string))
	(fun color () -> set_all ~color)
  end
  let cmd =
    Command.group ~summary:"set strip(s) to a color"
      [ "all", All.cmd ]
end

let () =
  let cmd =
    Command.group ~summary:"Pixel Pusher control util"
      [ "set", Set.cmd ]
  in
  Command.run cmd
