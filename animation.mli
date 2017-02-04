type t =
    { name : string
    ; update : (t -> unit)
    ; mutable model : Model.t option
    ; mutable primary_color : Color.t option
    ; mutable secondary_color : Color.t option }

val init : t -> Model.t -> t
  
(* Exposed because it's the null animation. *)
val off : t

(* set to `test for test animations; default: `live *)    
val mode : [`live | `test] ref

(* All animations available. *)
val all : unit -> t list
  
