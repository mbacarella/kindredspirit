open Core.Std

type t = { x:float; y:float; z:float } with sexp
val dist : t -> t -> float
