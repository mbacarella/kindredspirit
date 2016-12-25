open Core.Std
  
type t = { r: int; g: int; b: int } with sexp
let black = { r=0; g=0; b=0 }
let white = { r=255; g=255; b=255 }
let green = { r=0; g=255; b=0 }
let ri () = Random.int 256
let rand () = { r=ri (); g=ri (); b=ri () }

let f = Float.of_int
let i = Float.to_int
let to_gl t =
  (f t.r /. 255.), (f t.g /. 255.), (f t.b /. 255.)
let of_gl (r, g, b) =
  { r=i (r*.255.); g=i (g*.255.); b=i (b*.255.) }
let to_string t =
  sexp_of_t t |> Sexp.to_string
