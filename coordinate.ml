open Core.Std
  
type t = { x : float; y : float; z : float } with sexp
let dist a b =
  let dx = b.x -. a.x in
  let dy = b.y -. a.y in
  let dz = b.z -. a.z in
  sqrt ((dx ** 2.) +. (dy ** 2.) +. (dz ** 2.))
