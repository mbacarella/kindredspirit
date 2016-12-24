type t =
    { name : string
    ; update : (unit -> unit)
    ; mutable primary_color : Color.t option
    ; mutable secondary_color : Color.t option }

(* Exposed because it's the null animation. *)
val off : t
  
(* All animations available. *)
val all : t list
  
