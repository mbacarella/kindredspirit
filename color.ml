open Core.Std
  
type t = { r: int; g: int; b: int } with sexp
let black = { r=0; g=0; b=0 }
let white = { r=255; g=255; b=255 }
let green = { r=0; g=255; b=0 }
let of_hex_int i =
  { r=(i lsr 16) land 0xFF
  ; g=(i lsr 8) land 0xFF
  ; b=i land 0xFF }
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

let shade t ~factor =
  let r, g, b =
    let f = Float.of_int in
    f t.r, f t.g, f t.b
  in
  let i = Float.to_int in
  let m x = max (i x) 0 in
  let shade = 1.0 -. factor in
  { r = m (r *. shade)
  ; g = m (g *. shade)
  ; b = m (b *. shade) }
