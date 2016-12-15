type t =
    { name : string
    ; update : (unit -> unit)
    ; mutable primary_color : Pixel_pusher.Color.t option
    ; mutable secondary_color : Pixel_pusher.Color.t option }

(* All animations available. *)
      
val all = t list
  
