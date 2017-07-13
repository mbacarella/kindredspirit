open! Core
open! Async

let title = "Kindred Spirit Lighting Console"

module Config = struct
  type t =
      { display_width : float
      ; display_height : float
      ; target_fps : float
      ; beat_detection : bool
      ; waveform_detection : bool
      ; sound_dev : string }
        [@@deriving sexp, fields]
  let display_interval t =
    sec (1. /. t.target_fps)
end

let num_display_calls = ref 0
let last_display_time = ref Time.epoch

let mouse_x = ref 0.
let mouse_y = ref 0.
let mouse_down_left = ref false
let mouse_down_right = ref false

let reshape ~w ~h =
  (* The actual "world" stays fixed to display_width and display_height
     even if the user resizes. *)
  GlDraw.viewport ~x:0 ~y:0 ~w ~h

let text ?(color=(1.0, 1.0, 1.0)) ?size ~x ~y s =
  let font =
    match size with
    | None | Some `sm -> Glut.BITMAP_HELVETICA_10
    | Some `md -> Glut.BITMAP_HELVETICA_12
    | Some `lg -> Glut.BITMAP_HELVETICA_18
  in
  GlMat.push ();
  GlMat.load_identity ();
  GlDraw.color color;
  GlPix.raster_pos ~x ~y ();
  String.iter ~f:(fun c ->
    let c = Char.to_int c in
    Glut.bitmapCharacter ~font ~c) s;
  GlMat.pop ()


module List_pane = struct
  let height = 10.
  let width = 150.
  let mouse_over_animation () =
    let x = 0. in
    List.findi (Animation.all ()) ~f:(fun i _a ->
      let y = height *. (Float.of_int i) in
      !mouse_x >= x && !mouse_x < width && !mouse_y >= y && !mouse_y < y +. height)
  let display config =
    let display_height = Config.display_height config in
    GlDraw.color (0.1, 0.1, 0.1);
    GlDraw.rect (0., 0.) (width, display_height);
    let x = 0. in
    let hovered_a = mouse_over_animation () in
    List.iteri (Animation.all ()) ~f:(fun i a ->
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

module Fps = struct
  let times = Array.init 1000 ~f:(fun _ -> Time.epoch)
  let index = ref 0
  let fps = ref 0
  let epsilon = Time.Span.of_ms 0.
  let max_delay = ref epsilon
  let add now =
    times.(!index) <- now;
    index := (succ !index) mod Array.length times
  let calc () =
    let cutoff = Time.sub (Time.now ()) (sec 1.) in
    let times = Array.to_list times |> List.filter ~f:(Time.(<) cutoff) |> List.sort ~cmp:Time.compare in
    max_delay :=
      begin match times with
      | [] | [_] -> epsilon
      | t1 :: lst ->
        let _, span =
          List.fold_left lst ~init:(t1, epsilon) ~f:(fun (prev, span) time ->
            let span' = Time.diff time prev in
            if Time.Span.(>) span' span then (time, span')
            else (time, span))
        in
        span
      end;
    fps := List.length times
end

let rotating = ref true

let display_model =
  let angle = ref 0. in
  (fun ~center:(x, y) model ->
    GlMat.push ();
    GlMat.load_identity ();
    if !rotating then
      angle := Float.of_int ((succ (Float.to_int !angle)) mod 360);
    GlMat.translate ~x ~y ();
    GlMat.rotate ~angle:!angle ~x:0. ~y:1.0 ~z:0. ();
    GlMat.scale ~x:1.5 ~y:1.5 ~z:1.5 ();
    GlDraw.point_size 2.0;
    GlDraw.begins `points;
    List.iter model.Model.virtual_pixels ~f:(fun vp ->
      GlDraw.color (Color.to_gl vp.Virtual_pixel.color);
      let coord = vp.Virtual_pixel.coord in
      GlDraw.vertex ~x:coord.Coordinate.x ~y:(130. -. coord.Coordinate.y)
	~z:coord.Coordinate.z ());
    GlDraw.ends ();
    GlMat.pop ())

let load_colors_from_picker a cp =
  begin match a.Animation.primary_color, Color_picker.get_primary cp with
    | Some _, Some c -> a.Animation.primary_color <- Some c
    | None, None -> ()
    | Some _, None | None, Some _ -> failwithf "broken primary color picker for '%s'" a.Animation.name ()
  end;
  begin match a.Animation.secondary_color, Color_picker.get_secondary cp with
    | Some _, Some c -> a.Animation.secondary_color <- Some c
    | None, None -> ()
    | Some _, None | None, Some _ -> failwithf "broken secondary color picker for '%s'" a.Animation.name ()
  end

let display_animation ~config ~x a tag color_picker =
  load_colors_from_picker a color_picker;
  let s = sprintf "%s: %s" tag a.Animation.name in
  let display_height = Config.display_height config in
  text ~size:`md ~x ~y:(display_height -. 10.) s;
  a.Animation.update a;
  display_model ~center:(x +. 350., display_height -. 350.) (Option.value_exn a.Animation.model);
  Color_picker.display color_picker

module Preview_pane = struct
  let x = List_pane.width
  let y = 0.
  let width config = ((Config.display_width config) -. x) /. 2.0 -. 5.
  let loaded_animation = ref Animation.off
  let color_picker =
    Memo.general (fun config -> Color_picker.create ~x ~y ~width:(width config) ~height:180.)
  let load_animation config a model =
    Color_picker.reset (color_picker config) a;
    loaded_animation := Animation.init a model
  let display config =
    display_animation ~config ~x !loaded_animation "preview" (color_picker config);
end

module Live_pane = struct
  let x config = List_pane.width +. (Preview_pane.width config) +. 10.
  let y = 0.
  let width config = (Config.display_width config) -. (x config)
  let loaded_animation = ref Animation.off
  let color_picker =
    Memo.general (fun config -> Color_picker.create ~x:(x config) ~y ~width:(width config) ~height:180.)
  let load_animation_from_preview config =
    let a = !Preview_pane.loaded_animation in
    Color_picker.reset (color_picker config) a;
    loaded_animation := Animation.init a (Option.value_exn a.Animation.model)
  let display config =
    display_animation ~config ~x:(x config) !loaded_animation "live" (color_picker config);
end

module Pixel_pusher_status = struct
  let display config model =
    let display_height = Config.display_height config in
    let display_width = Config.display_width config in
    let module Controller_report = Pixel_pusher.Controller_report in
    let expected_controllers = model.Model.controller_ids in
    let seen_controllers =
      List.fold_left (Pixel_pusher.get_controllers ()) ~init:Int.Map.empty ~f:(fun map c ->
        Map.add map ~key:c.Controller_report.controller_id ~data:c)
    in
    let box_width = 15. in
    let box_height = 15. in
    let now = Time.now () in
    List.iteri (Set.to_list expected_controllers) ~f:(fun index controller_id ->
      let x = display_width -. box_width in
      let y = display_height -. (40. +. ((Float.of_int index) *. box_height)) in
      let rect_color =
        match Map.find seen_controllers controller_id with
        | None -> (1.0, 0.0, 0.0)
        | Some c ->
          let span = Time.diff now c.Controller_report.last_beacon in
          if Time.Span.(<) span (sec 1.0) then (0.0, 1.0, 0.0)
          else (0.0, 1.0, 0.0)
      in
      GlDraw.color rect_color;
      GlDraw.rect (x, (y-.2.0)) (x +. 10., y +. 10.);
      text ~color:(0.0, 0.0, 0.0) ~x ~y (sprintf " %d" controller_id))
end

let send_frame_to_pixel_pushers a send_updates_t =
  match a.Animation.model with
    | None -> failwithf "animation %s is not initiatilized" a.Animation.name ()
    | Some model ->
      let strip_map = Pixel_pusher.get_strips_as_map () in
      List.iter model.Model.virtual_pixels ~f:(fun vp ->
        let controller_id = vp.Virtual_pixel.controller_id in
        let strip_id = vp.Virtual_pixel.strip_id in
        let key = (controller_id, strip_id) in
	match Map.find strip_map key with
	  | None -> ()
	  | Some strip ->
	    let index = Virtual_pixel.pixel_id vp in
	    let color = Virtual_pixel.color vp in
	    Pixel_pusher.Strip.set_pixel strip ~color ~index);
      Pixel_pusher.send_updates_from_non_async_thread send_updates_t

let handle_mouse_events config model =
  if !mouse_down_left then begin
    let x = !mouse_x in
    let y = (Config.display_height config) -. !mouse_y in
    Color_picker.maybe_set_primary (Preview_pane.color_picker config) ~x ~y;
    Color_picker.maybe_set_primary (Live_pane.color_picker config) ~x ~y;
    Option.iter (List_pane.mouse_over_animation ()) ~f:(fun (_, a) ->
      Preview_pane.load_animation config a model)
  end;
  if !mouse_down_right then begin
    let x = !mouse_x in
    let y = (Config.display_height config) -. !mouse_y in
    Color_picker.maybe_set_secondary (Preview_pane.color_picker config) ~x ~y;
    Color_picker.maybe_set_secondary (Live_pane.color_picker config) ~x ~y
  end

let next_display_time = ref (Time.now ())
let display ~config ~model ~send_updates_t () =
  if Time.( < ) (Time.now ()) !next_display_time then ()
  else begin
    let start_display_time = Time.now () in
    let display_interval = Config.display_interval config in
    next_display_time := Time.add start_display_time display_interval;
    handle_mouse_events config model;
    GlClear.clear [`color];
    List_pane.display config;
    Preview_pane.display config;
    Live_pane.display config;
    Pixel_pusher_status.display config model;
    let now = Time.now () in
    Fps.add now;
    last_display_time := now;
    let display_width = Config.display_width config in
    let display_height = Config.display_height config in
    text ~x:(display_width -. 90.) ~y:(display_height -. 10.) (sprintf "fps: %d max: %.0fms" !Fps.fps (Time.Span.to_ms !Fps.max_delay));
    text ~x:(display_width -. 40.) ~y:(display_height -. 20.) (sprintf "beat: %.4f" !Beat_detection.beat);
    Gl.flush ();
    Glut.swapBuffers ();
    send_frame_to_pixel_pushers !Live_pane.loaded_animation send_updates_t;
    begin
      let now = Time.now () in
      if Time.( > ) now !next_display_time then
	printf "!! took longer than %s to display %s !!\n%!"
	  (Time.Span.to_string display_interval)
	  (Time.diff now start_display_time |> Time.Span.to_string)
    end;
    incr num_display_calls
  end

let tick () =
  (* We time the refreshes ourselves via this idle func rather than
     having GLUT do it because this pause call has the important
     side-effect of surrendering time to the Async thread.
     It also saves us from burning 100% CPU (if we don't have to). *)
  let span = Time.diff !next_display_time (Time.now ()) in
  if Time.Span.(>) span Time.Span.zero then
    Time.pause span;
  Glut.postRedisplay ()

let key_input config model ~key ~x:_ ~y:_ =
  match Char.of_int key with
    | None -> printf "wat (key code: %d)\n" key
    | Some '\r' -> Live_pane.load_animation_from_preview config
    | Some '\n' -> printf "received line feed?!\n"
    | Some ' ' -> rotating := not !rotating
    | Some 'D' -> Model.dump_sexp model
    | Some 'C' -> Model.dump_csv model
    | Some 'Q' ->
      printf "*** Shutting down on 'Q' command\n";
      Shutdown.shutdown 0
    | Some char ->
      printf "*** key input: %c\n" char

let mouse_motion ~x ~y =
  mouse_x := Float.of_int x;
  mouse_y := Float.of_int y

let mouse_click_event ~button ~state ~x ~y =
  mouse_motion ~x ~y;
  match button with
    | Glut.RIGHT_BUTTON ->
      begin match state with
	| Glut.DOWN -> mouse_down_right := true
	| Glut.UP -> mouse_down_right := false
      end
    | Glut.LEFT_BUTTON ->
      begin match state with
	| Glut.DOWN -> mouse_down_left := true
	| Glut.UP -> mouse_down_left := false
      end
    | _ -> ()

let gl_main config model send_updates_t =
  let display_width = Config.display_width config in
  let display_height = Config.display_height config in
  let _ = Glut.init ~argv:Sys.argv in
  Glut.initDisplayMode ~depth:true ~double_buffer:true ();
  let _ = Glut.createWindow ~title in

  Glut.setCursor Glut.CURSOR_LEFT_ARROW;

  Glut.positionWindow ~x:0 ~y:0;
  GlMat.mode `projection;
  GlMat.load_identity ();
  GlMat.ortho ~x:(0.0, display_width) ~y:(0.0, display_height) ~z:(-360.0, 360.0);
  GlMat.mode `modelview;
  GlMat.load_identity ();

  Color_picker.gl_init ();

  Glut.reshapeFunc ~cb:reshape;
  Glut.displayFunc ~cb:(display ~config ~model ~send_updates_t);
  Glut.idleFunc ~cb:(Some tick);
  Glut.keyboardFunc ~cb:(key_input config model);
  Glut.mouseFunc ~cb:mouse_click_event;
  Glut.motionFunc ~cb:mouse_motion;
  Glut.passiveMotionFunc ~cb:mouse_motion;
  Glut.mainLoop ()

let start_waveform_listener ~sound_dev =
  don't_wait_for (In_thread.run (fun () ->
    Waveform.start ~sound_dev))

let start_watchdog_muter () =
  let ip = "10.1.1.200" in
  let watchdog_ip = Unix.Inet_addr.of_string ip in
  let watchdog_port = 9901 in
  let addr = Unix.ADDR_INET (watchdog_ip, watchdog_port) in
  let socket =    Core.Unix.socket ~domain:Core.Unix.PF_INET
      ~kind:Core.Unix.SOCK_DGRAM ~protocol:0
  in
  let buf = "STFU" in
  let len = String.length buf in
  Clock.every (sec 1.) (fun () ->
    let bytes_sent =
      Core.Unix.sendto socket ~buf ~pos:0
        ~len:(String.length buf) ~mode:[] ~addr
    in
    if bytes_sent <> len then
      failwithf "Failed to send %d bytes to %s (%d bytes short)"
        bytes_sent ip (len - bytes_sent) ());
  printf "*** Watchdog muter initialized\n"
    
let main ~config =
  start_watchdog_muter ();
  Clock.every (sec 1.) (fun () -> Fps.calc ());
  let sound_dev = Config.sound_dev config in
  (if (Config.beat_detection config) then Beat_detection.start ~sound_dev
   else return ()) >>= fun () ->
  if (Config.waveform_detection config) then start_waveform_listener ~sound_dev;
  Pixel_pusher.start () >>= fun send_updates_t ->
  Model.load "model.csv" >>= fun model ->
  Preview_pane.loaded_animation := (Animation.init Animation.off model);
  Live_pane.loaded_animation := (Animation.init Animation.off model);
  In_thread.run (fun () -> gl_main config model send_updates_t)

let () =
  let cmd =
    Command.async ~summary:title
      Command.Spec.(empty
                    +> flag "-test-animations" no_arg ~doc:"test individual strips for signal/power"
                    +> anon ("config-path" %: string))
      (fun test_animations config_path () ->
	if test_animations then Animation.mode := `test;
        Reader.file_contents config_path >>= fun s ->
        let config = Sexp.of_string s |> Config.t_of_sexp in
        main ~config)
  in
  Command.run cmd
