type t =
    { name : string
    ; update : (unit -> unit)
    ; mutable primary_color : Color.t option
    ; mutable secondary_color : Color.t option }

let off_animation =
  { name = "off"
  ; update = (fun () -> ())
  ; primary_color = None
  ; secondary_color = None }

let solid_animation =
  { name = "solid"
  ; update = (fun () -> ())
  ; primary_color = Some Color.white
  ; secondary_color = None }
    
let all =
  [ off_animation
  ; solid_animation ]
