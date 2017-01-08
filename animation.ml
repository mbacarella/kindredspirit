open Core.Std
  
type t =
    { name : string
    ; update : (t -> unit)
    ; mutable model : Model.t option
    ; mutable primary_color : Color.t option
    ; mutable secondary_color : Color.t option }

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
    (* XXX: drops out too fast near the end *)
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

let all =
  [ off_animation
  ; solid_animation
  ; noise_animation
  ; Rain.animation
  ; Solid_glow.animation ]
