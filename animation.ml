open! Core.Std
  
type t =
    { name : string
    ; update : (t -> unit)
    ; mutable model : Model.t option
    ; mutable primary_color : Color.t option
    ; mutable secondary_color : Color.t option }

let dj = { Coordinate.x=0.; y=100.; z=3. }
          
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

module Rain = struct
  let ticks = ref 0
  let height = 140.
  let update t =
    let pos = height -. (Float.of_int (!ticks mod (Float.to_int height))) in
    assert (pos > 0.);
    iter_pixels t ~f:(fun _ vp ->
      vp.Virtual_pixel.color <- Option.value_exn
	(let coord = vp.Virtual_pixel.coord in
	 let dist = coord.Coordinate.y -. pos in
	 if dist < 0. then t.secondary_color
	 else if dist < 1. then t.primary_color
	 else
	   Option.map t.primary_color ~f:(Color.shade ~factor:((dist /. height) *. 10.))));
    incr ticks
  let animation =
    { empty with
      name = "rain"
    ; update
    ; primary_color = Some (Color.of_hex_int 0x660E6F)
    ; secondary_color = Some Color.black }
end

module Solid_glow = struct
  let ticks = ref 0
  let update t =
    let phase =
      let phase = Float.of_int (!ticks mod 200) in
      if phase > 100. then 100. -. (phase -. 100.)
      else phase
    in
    (* TODO: drops out too fast near the end *)
    iter_pixels t ~f:(fun _ vp ->
      let c =
	Option.value_exn t.primary_color
        |> Color.shade ~factor:(1.0 -. (phase /. 100.))
      in
      vp.Virtual_pixel.color <- c);
    incr ticks
  let animation =
    { empty with
      name = "solidglow"
    ; update
    ; primary_color = Some Color.green }
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
      let index = (!i + d*3) mod colsl in 
      vp.Virtual_pixel.color <- cols.(index)
    );
    incr i
      
  let animation =
    { empty with name = "rainbow-dj"; update }
end

module Radiate_dj = struct
  let ticks = ref 0
  let update t =
    let i = Float.of_int !ticks in
    iter_pixels t ~f:(fun _ vp ->
      let d = Coordinate.dist dj vp.Virtual_pixel.coord in
      vp.Virtual_pixel.color <-
	if d >= i && d < (i +. 40.) then Option.value_exn t.primary_color
	else if d >= (i +. 40.) && d < (i +. 80.) then Option.value_exn t.secondary_color
	else Color.black);
    ticks := (succ !ticks) mod 200
  let animation =
    { empty with name="radiate-dj"
    ; update
    ; primary_color = Some Color.{r=0x99; g=0; b=0 }
    ; secondary_color = Some Color.black
    }
end
  
let live_all =
  [ off_animation
  ; solid_animation
  ; noise_animation
  ; Rain.animation
  ; Solid_glow.animation
  ; Rainbow_solid.animation
  ; Rainbow_dj.animation
  ; Radiate_dj.animation ]

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
