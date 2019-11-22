open! Core

type t =
    { name : string
    ; update : (t -> unit)
    ; mutable model : Model.t option
    ; mutable primary_color : Color.t option
    ; mutable secondary_color : Color.t option }

let dj = { Coordinate.x=140.; y=30.; z=(-40.) }
(*let subs = { Coordinate.x=140.; y=150.; z=(-40.) }*)
let nose = { Coordinate.x=45.; y=130.; z=0. }

let init t model =
  { t with model = Some (Model.dup model) }

let with_model t ~f =
  match t.model with
    | None -> failwithf "model not initialized: %s" t.name ()
    | Some model ->
      f t model

let iter_pixels ~f =
  with_model ~f:(fun t model -> List.iter model.Model.virtual_pixels ~f:(fun vp -> f t vp))

let empty = { name="empty"; update=ignore; model=None; primary_color=None; secondary_color=None }
let off_animation = { empty with name="off" }
let off = off_animation

let solid_animation =
  { empty with
    name = "solid"
  ; update = iter_pixels ~f:(fun t vp -> vp.Virtual_pixel.color <- Option.value_exn t.primary_color)
  ; primary_color = Some Color.green }

let noise_animation =
  { empty with
    name = "noise"
  ; update = iter_pixels ~f:(fun _t vp -> vp.Virtual_pixel.color <- Color.rand ()) }

let twinkle_animation =
  { empty with
    name = "twinkle"
  ; update = iter_pixels ~f:(fun _t vp ->
    vp.Virtual_pixel.color <-
      begin
        if Random.int 10 = 0 then Color.rand ()
        else Color.black
      end) }
(*
module Sticks_rnd = struct
  let ticks = ref 0
  let gen () =
    (* TODO: too basic? : / *)
    Array.init 100 ~f:(fun _ ->
      match Random.int 6 with
      | 0 -> Color.red
      | 1 -> Color.green
      | 2 -> Color.blue
      | 3 -> Color.white
      | 4 -> Color.purple
      | 5 -> Color.black
      | _ -> assert false)
  let colors = ref (gen ())
  let update t =
    if !ticks = 0 then colors := gen ();
    iter_pixels t ~f:(fun _ vp ->
	vp.Virtual_pixel.color <- (!colors).(vp.Virtual_pixel.controller_id * 8 + vp.Virtual_pixel.strip_id));
    ticks := (succ !ticks) mod 100

  let animation =
    { empty with
      name = "sticks-rnd"
    ; update }
end
*)

module Rain = struct
  let ticks = ref 0
  let height = 140.
  let rand_color = ref (Color.rand ())
  let update ~rnd t =
    if rnd && !ticks mod 300 = 0 then rand_color := Color.rand ()
    else ();
    let pos = Float.of_int (!ticks mod (Float.to_int (height *. 1.5))) in
    iter_pixels t ~f:(fun _ vp ->
      let color = if rnd then !rand_color else Option.value_exn t.primary_color in
      vp.Virtual_pixel.color <-
        begin
	  let coord = vp.Virtual_pixel.coord in
	  let dist = coord.Coordinate.y -. pos in
    let (<) = Float.(<) in
	  if dist < 0. then Color.shade vp.Virtual_pixel.color ~factor:0.05
	  else if dist < 1. then color
	  else Color.shade ~factor:((dist /. height) *. 10.) color
        end);
    incr ticks

  let anim = { empty with name = "rain"; update = update ~rnd:false; primary_color = Some (Color.of_hex_int 0x660E6F) }
  let anim_rnd = { empty with name = "rain-rnd"; update = update ~rnd:true }
end

module Split = struct
  let shades = Array.init 50 ~f:(fun i -> Float.of_int i /. 50.)
  let ticks = ref 0
  let update t =
    iter_pixels t ~f:(fun _ vp ->
      let d = 50 - (Float.abs vp.Virtual_pixel.coord.Coordinate.z |> Float.to_int) in
      let pcolor = Option.value_exn t.primary_color in
      let index = (d + !ticks) mod (Array.length shades) in
      vp.Virtual_pixel.color <- Color.shade ~factor:(shades.(index)) pcolor);
    incr ticks
  let animation = { empty with name = "split"; update; primary_color = Some Color.green }
end

module Solid_glow = struct
  let ticks = ref 0
  let color = ref (Color.rand ())
  let update ~rnd t =
    let phase =
      let phase = Float.of_int (!ticks mod 200) in
      if Float.(>) phase 100. then 100. -. (phase -. 100.)
      else phase
    in
    if rnd then begin
      if !ticks mod 200 = 0 then
        color := Color.rand ()
    end else begin
      color := Option.value_exn t.primary_color
    end;
    (* TODO: drops out too fast near the end *)
    iter_pixels t ~f:(fun _ vp ->
      let c = Color.shade !color ~factor:(1.0 -. (phase /. 100.)) in
      vp.Virtual_pixel.color <- c);
    incr ticks
  let anim =
    { empty with
      name = "solidglow"
    ; update=update ~rnd:false
    ; primary_color = Some Color.green }
  let anim_rnd =
    { empty with
      name = "solidglow-rnd"
    ; update=update ~rnd:true }
end

module Solid_beat = struct
  let update t =
    let beat =
      let beat = !Beat_detection.beat in
      if Float.(<) beat 0.3 then 0.
      else beat
    in
    let color = Option.value_exn t.primary_color in
    let intensity = beat /. 1.0 in
    iter_pixels t ~f:(fun _ vp ->
      vp.Virtual_pixel.color <- Color.shade ~factor:(1.0 -. intensity) color)

  let animation = { empty with name = "solid-beat"; update; primary_color = Some Color.red  }
end

module Waveform = struct
  let update t f =
    let pcm_data = Waveform.pcm_data in
    let pcm_data_len = Array.length pcm_data in
    let pcm_index = !Waveform.index in
    let spectro_data = Beat_detection.spectro in
    let spectro_data_len = Array.length spectro_data in
    let spectro_index = !Beat_detection.index in
    iter_pixels t ~f:(fun _ vp ->
      let power_dist = min ((Coordinate.dist dj vp.Virtual_pixel.coord |> Float.to_int) / 3 ) pcm_data_len in
      let power () =
        let circ_index =
          if power_dist > pcm_index
          then pcm_data_len - (power_dist-pcm_index)
          else pcm_index - power_dist
        in
        pcm_data.(circ_index)
      in
      let spectro_dist = min ((Coordinate.dist dj vp.Virtual_pixel.coord |> Float.to_int) / 3 ) spectro_data_len in
      let spectro () =
        let circ_index =
          if spectro_dist > spectro_index
          then spectro_data_len - (spectro_dist-spectro_index)
          else spectro_index - spectro_dist
        in
        spectro_data.(circ_index)
      in
      vp.Virtual_pixel.color <- f ~power ~spectro)

  let update_rgb t =
    update t (fun ~power ~spectro:_ ->
      let color =
        let power = power () in
        if power > 5000 then Color.blue
        else if power > 2500 then Color.purple
        else if power > 1000 then Color.green
        else if power > 500 then Color.red
        else Color.black
      in
      color)

  let avg_of_indices a ii =
    (List.map ii ~f:(fun i -> snd a.(i)) |> List.fold_left ~init:0. ~f:(+.))
    /. (List.length ii |> Float.of_int)

  let update_eq t =
    update t (fun ~power ~spectro ->
      let shade =
        let power = Float.min 32768. (Float.of_int (power ())) in
        assert (Float.(>=) power 0.);
        Color.shade ~factor:(1. -. (power /. 32768.))
      in
      let color =
        (*let bands = [| 60; 320; 640; 1280; 2560; 5120; 10240; 22000 |] *)
        let spectro = spectro () in
        let lows = avg_of_indices spectro [0; 1] in
        let mids = avg_of_indices spectro [2; 3; 4] in
        let highs = avg_of_indices spectro [5; 6; 7] in
        { Color.r = Float.to_int (lows  *. 255.)
        ;       g = Float.to_int (mids  *. 255.)
        ;       b = Float.to_int (highs *. 255.) }
      in
      shade color)

  let anim_rgb = { empty with name = "waveform-rgb"; update=update_rgb }
  let anim_eq = { empty with name = "waveform-eq"; update=update_eq }
end

(*
module Subwoofer = struct
  let max_beat = ref 0.
  let update t =
    let beat =
      let b = !Beat_detection.beat in
      if b > 3.0 then b else 0.
    in
    if beat > !max_beat then max_beat := beat;
    let intensity = beat /. !max_beat in
    let magnitude = intensity *. 320. in
    iter_pixels t ~f:(fun _ vp ->
      let dist = Coordinate.dist subs vp.Virtual_pixel.coord in
      vp.Virtual_pixel.color <-
        if dist < magnitude then Option.value_exn t.primary_color
        else Color.shade ~factor:0.9 vp.Virtual_pixel.color);
    max_beat := !max_beat *. 0.99

  let animation =
    { empty with name = "subwoofer"
    ; update
    ; primary_color = Some Color.green }
end
*)

module Strobe = struct
  let ticks = ref 0
  let color = ref Color.white
  let update ~rnd t =
    if !ticks = 0 then
      color := if rnd then Color.rand () else Option.value_exn t.primary_color;
    iter_pixels t ~f:(fun _ vp ->
      vp.Virtual_pixel.color <-
        if !ticks < 24 then !color
        else Color.black);
    ticks := (succ !ticks) mod 50
  let reg = { empty with name = "strobe"; update = update ~rnd:false; primary_color = Some Color.purple }
  let rnd = { empty with name = "strobe-rnd"; update = update ~rnd:true }
end

let rainbow_colors =
  Memo.unit (fun () ->
    Array.init 768 ~f:(fun i ->
      { Color.
        r =
          if i < 256 then 255-i
          else if i >= 512 then i-512
          else 0
      ; g =
          if i < 256 then i
          else if i >= 256 && i < 512 then 511-i
          else 0
      ; b =
          if i >= 512 then 767-i
          else if i >= 256 then i-256
          else 0 }))

module Rainbow_solid = struct
  let i = ref 0
  let update t =
    let cols = rainbow_colors () in
    let c = cols.(!i) in
    iter_pixels t ~f:(fun _ vp ->
      vp.Virtual_pixel.color <- c);
    i := (succ !i) mod (Array.length cols)

  let animation =
    { empty with name = "rainbow-solid"; update }
end

module Rainbow_dj = struct
  let i = ref 0
  let update t =
    let cols = rainbow_colors () in
    let colsl = Array.length cols in
    iter_pixels t ~f:(fun _ vp ->
      let d = Coordinate.dist dj (Virtual_pixel.coord vp) |> Float.to_int in
      let index = (!i*2 + d*3) mod colsl in
      vp.Virtual_pixel.color <- cols.(index)
    );
    incr i

  let animation =
    { empty with name = "rainbow-dj"; update }
end

module Rainbow_nose = struct
  let i = ref 0
  let update t =
    let cols = rainbow_colors () in
    let colsl = Array.length cols in
    iter_pixels t ~f:(fun _ vp ->
      let d = Coordinate.dist nose (Virtual_pixel.coord vp) |> Float.to_int in
      let index = (!i*2 + d*3) mod colsl in
      vp.Virtual_pixel.color <- cols.(index)
    );
    incr i

  let animation =
    { empty with name = "rainbow-nose"; update }
end

module Scan_dj = struct
  let ticks = ref 0
  let color = ref (Color.rand ())
  let size = 625
  let update ~rnd t =
    let pos = Float.of_int ((!ticks/2) mod size) in
    let c = if rnd then Some !color else t.primary_color in
    iter_pixels t ~f:(fun _ vp ->
      vp.Virtual_pixel.color <- Option.value_exn
        (let dist = Coordinate.dist vp.Virtual_pixel.coord dj in
         if Float.(>=) dist pos && Float.(<) dist (pos +. 40.) then c
         else Some (Color.shade ~factor:0.05 vp.Virtual_pixel.color)));
    ticks := (succ !ticks) mod size;
    if rnd && !ticks = 0 then color := Color.rand ()

  let reg_animation =
    { empty with name="scan-dj"
    ; update = update ~rnd:false
    ; primary_color = Some Color.{r=0x99; g=0; b=0 } }

  let rnd_animation =
    { empty with name="scan-dj-rnd"
    ; update = update ~rnd:true }
end

module Strip_walk = struct
  let ticks = ref 0
  let update t =
    let id = !ticks / 10 in
    iter_pixels t ~f:(fun _ vp ->
      vp.Virtual_pixel.color <-
        if vp.Virtual_pixel.pixel_id = id then Option.value_exn t.primary_color
        else Option.value_exn t.secondary_color);
    ticks := (succ !ticks) mod 1000

  let animation =
    { empty with name="strip-walk"
    ; primary_color = Some Color.green
    ; secondary_color = Some Color.{r=0x66; g=0x66; b=0x66 }
    ; update }
end

module Slugs = struct
  let ticks = ref 0
  let color = ref (Color.rand ())
  let update ~rnd t =
    let id = !ticks / 10 in
    if rnd then begin
      if !ticks = 0 then color := Color.rand ()
    end else begin
      color := Option.value_exn t.primary_color
    end;
    iter_pixels t ~f:(fun _ vp ->
      vp.Virtual_pixel.color <-
        if vp.Virtual_pixel.pixel_id = id then !color
        else Color.shade ~factor:0.025 vp.Virtual_pixel.color);
    ticks := (succ !ticks) mod 1000


  let anim =
    { empty with name="slugs"; primary_color = Some Color.green; update=update ~rnd:false }
  let anim_rnd =
    { empty with name="slugs-rnd"; update=update ~rnd:true }
end

module Flame = struct
  let ticks = ref 0
  let rand_color = ref (Color.rand ())

  let update ~rnd t =
    let model = Option.value_exn t.model in
    let y_map = Model.y_map model in
    let y_range = Model.y_range model in
    let max_y = Model.max_y model in
    let row y = Map.find_exn y_map y in

    if rnd && !ticks mod 10 = 0 then rand_color := Color.rand ();
    let color = if rnd then !rand_color else Option.value_exn t.primary_color in

    Array.iter (row max_y) ~f:(fun vp -> vp.Virtual_pixel.color <- color);
    List.iter (List.range 0 (Array.length y_range-1)) ~f:(fun i ->
      let y = y_range.(i) in
      let lower_y = y_range.(succ i) in
      let cur_row = row y in
      let prev_row = row lower_y in
      Array.iteri cur_row ~f:(fun j vp ->
        let pct = (Float.of_int j) /. Float.of_int (Array.length cur_row) in
        let prev_index =
          let index = (Float.of_int (Array.length prev_row)) *. pct |> Float.to_int in
          min (pred (Array.length prev_row)) index
        in
        let prev_color = Virtual_pixel.color (prev_row.(prev_index)) in
        let color =
          Color.shade ~factor:(Random.float 0.025) prev_color
        in
        vp.Virtual_pixel.color <- color));
    incr ticks

  let anim = { empty with name="flame"; update = update ~rnd:false; primary_color = Some Color.purple }
  let anim_rnd = { empty with name="flame-rnd"; update = update ~rnd:true }
end

module Pixelate = struct
  let ticks = ref 0
  let colors = Array.init 100 ~f:(fun _ -> Color.rand ())
  let cluster_size = ref 1
  let update t =
    iter_pixels t ~f:(fun _ vp ->
      let index = vp.Virtual_pixel.controller_id * 800 + vp.Virtual_pixel.strip_id * 100 + vp.Virtual_pixel.pixel_id in
      vp.Virtual_pixel.color <- colors.(((!ticks/10 + index/(!cluster_size))) mod (Array.length colors)));
    incr ticks;
    if !ticks mod 650 = 0 then incr cluster_size;
    if !cluster_size > 10 then cluster_size := 1

  let animation =
    { empty with name="pixelate"
     ; update }
end

let live_all =
  [ off_animation
  ; solid_animation
  ; twinkle_animation
  ; noise_animation
  ; Strip_walk.animation
  ; Slugs.anim
  ; Slugs.anim_rnd
  ; Split.animation
  ; Strobe.reg
  ; Strobe.rnd
  ; Rain.anim
  ; Rain.anim_rnd
  ; Scan_dj.reg_animation
  ; Scan_dj.rnd_animation
  ; Solid_glow.anim
  ; Solid_glow.anim_rnd
  ; Solid_beat.animation
  ; Waveform.anim_rgb
  ; Waveform.anim_eq
  ; Rainbow_solid.animation
  ; Rainbow_dj.animation
  ; Rainbow_nose.animation
  ; Flame.anim
  ; Flame.anim_rnd
  ; Pixelate.animation ]

let test_all () =
  off_animation :: List.concat_map (List.range 0 8) ~f:(fun controller_id ->
    List.map (List.range 0 8) ~f:(fun strip_id ->
      let name = sprintf "test-%d:%d" controller_id strip_id in
      let update t =
	iter_pixels t ~f:(fun _ vp ->
	  vp.Virtual_pixel.color <-
	    if vp.Virtual_pixel.controller_id = controller_id
            && vp.Virtual_pixel.strip_id = strip_id
	    then Option.value_exn t.primary_color
	    else Option.value_exn t.secondary_color)
      in
      { empty with name; update
      ; primary_color = Some Color.white
      ; secondary_color = Some Color.black }))

let mode = ref `live

let all = Memo.unit (fun () ->
  match !mode with
    | `live -> live_all
    | `test -> test_all ())
