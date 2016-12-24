type t =
    { name : string
    ; update : (unit -> unit)
    ; mutable model : Model.t option
    ; mutable primary_color : Color.t option
    ; mutable secondary_color : Color.t option }

let init t model =
  { t with model = Some model }
    
let off_animation =
  { name = "off"
  ; update = (fun () -> ())
  ; model = None
  ; primary_color = None
  ; secondary_color = None }

let solid_animation =
  { name = "solid"
  ; update = (fun () -> ())
  ; model = None
  ; primary_color = Some Color.white
  ; secondary_color = None }

let off = off_animation
let all =
  [ off_animation
  ; solid_animation ]
