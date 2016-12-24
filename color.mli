type t = { r: int; g: int; b: int } with sexp
val rand : unit -> t
val black : t
val white : t
val green : t
val to_gl : t -> (float * float * float)
