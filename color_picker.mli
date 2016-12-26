type t

val gl_init : unit -> unit
val create : x:float -> y:float -> width:float -> height:float -> t
val display : t -> unit
val get_primary : t -> Color.t option
val get_secondary : t -> Color.t option
val maybe_set_primary : t -> x:float -> y:float -> unit
val maybe_set_secondary : t -> x:float -> y:float -> unit
val reset : t -> Animation.t -> unit
