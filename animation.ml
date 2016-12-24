open! Core.Std
  
type t =
    { name : string
    ; update : (t -> unit)
    ; mutable model : Model.t option
    ; mutable primary_color : Color.t option
    ; mutable secondary_color : Color.t option }

let init t model =
  { t with model = Some model }

let with_model t ~f =
  match t.model with
    | None -> failwithf "model not initialized: %s" t.name ()
    | Some model ->
      f t model
	
let off_animation =
  { name = "off"
  ; update = (fun _t -> ())
  ; model = None
  ; primary_color = None
  ; secondary_color = None }

let solid_animation =
  { name = "solid"
  ; update =
    with_model ~f:(fun t model ->
      List.iter model.Model.virtual_pixels ~f:(fun vp ->
	vp.Model.Virtual_pixel.color <- Option.value_exn t.primary_color))
  ; model = None
  ; primary_color = Some Color.white
  ; secondary_color = None }
  
let off = off_animation
let all =
  [ off_animation
  ; solid_animation ]
