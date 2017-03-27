open Core.Std

type t = { x:float; y:float; z:float } [@@deriving sexp, fields]
val dist : t -> t -> float
