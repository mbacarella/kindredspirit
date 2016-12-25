open! Core.Std

type pos = float * float
type t =
    { x : float
    ; y : float
    ; width : float
    ; height : float
    ; mutable kind : [ `NA | `Primary of pos | `Primary_and_secondary of pos * pos ] }

let create ~x ~y ~width ~height =
  { x; y; width; height; kind=`NA }
    
(* Adapted from https://gist.github.com/mjackson/5311256 *)
let hue_to_rgb ~p ~q ~t =
  let t = if t < 0. then t +. 1. else t in
  let t = if t > 1. then t -. 1. else t in
  if t < 1. /. 6. then p +. (q -. p) *. 6. *. t
  else if t < 1. /. 2. then q
  else if t < 2. /. 3. then p +. (q -. p) *. (2. /. 3. -. t) *. 6.
  else p

let hsl_to_rgb ~h ~l ~s =
  if s = 0. then (l, l, l) (* achromatic *)
  else
    let q = if l < 0.5 then l *. (1. +. s) else l +. s -. l *. s in
    let p = 2. *. l -. q in
    hue_to_rgb ~p ~q ~t:(h +. 1. /. 3.),
    hue_to_rgb ~p ~q ~t:h,
    hue_to_rgb ~p ~q ~t:(h -. 1. /. 3.)

let color_at_xy_gl t ~x ~y =
  if x < t.x || x >= t.x +. t.width || y < t.y || y >= t.y +. t.height then None
  else
    let h = (x -. t.x) /. t.width in
    let l = (y -. t.y) /. t.height in
    let s = 1.0 in
    Some (hsl_to_rgb ~h ~l ~s)

let color_at_xy t ~x ~y =
  match color_at_xy_gl t ~x ~y with
    | None -> None
    | Some c -> Some (Color.of_gl c)

let hsl_iter f =
  for h = 0 to 359; do
    let h = Float.of_int h /. 360. in
    for l=0 to 100; do
      let l = Float.of_int l /. 100. in
      f ~h ~s:1.0 ~l
    done
  done

let draw_circle ~x ~y =
  GlDraw.color (Color.black |> Color.to_gl);
  GlDraw.line_width 4.;
  GlDraw.begins `line_loop;
  GlDraw.vertex ~x:(x -. 5.) ~y:(y -. 5.) ();
  GlDraw.vertex ~x:(x +. 5.) ~y:(y -. 5.) ();
  GlDraw.vertex ~x:(x +. 5.) ~y:(y +. 5.) ();
  GlDraw.vertex ~x:(x -. 5.) ~y:(y +. 5.) ();
  GlDraw.ends ()
    
let display_selections t =
  match t.kind with
    | `NA -> ()
    | `Primary_and_secondary ((px, py), (sx, sy)) ->
      draw_circle ~x:px ~y:py;
      draw_circle ~x:sx ~y:sy
    | `Primary (x, y) ->
      draw_circle ~x ~y
	
let display t =
  begin match t.kind with
    | `NA ->
      GlDraw.point_size 1.0;
      GlDraw.color (0.9, 0.9, 0.9);
      GlDraw.begins `lines;
      GlDraw.vertex ~x:t.x ~y:t.y ();
      GlDraw.vertex ~x:(t.x +. t.width) ~y:(t.y +. t.height) ();
      GlDraw.vertex ~x:t.x ~y:(t.y +. t.height) ();
      GlDraw.vertex ~x:(t.x +. t.width) ~y:t.y ();
      GlDraw.ends ()
    | `Primary _ | `Primary_and_secondary _ ->
      GlDraw.begins `points;
      hsl_iter (fun ~h ~s ~l ->
	GlDraw.color (hsl_to_rgb ~h ~l ~s);
	let x = t.x +. t.width *. h in
	let y = t.y +. t.height *. l in
	GlDraw.vertex ~x ~y ()
      );
      GlDraw.ends ()
  end;
  display_selections t
  
let get_primary t =
  match t.kind with
    | `NA -> None
    | `Primary (x, y) | `Primary_and_secondary ((x, y), _) ->
      color_at_xy t ~x ~y

let get_secondary t =
  match t.kind with
    | `NA | `Primary _ -> None
    | `Primary_and_secondary (_, (x, y)) ->
      color_at_xy t ~x ~y

let maybe_set_primary t ~x ~y =
  match color_at_xy t ~x ~y with
    | None -> ()
    | Some _c ->
      match t.kind with
	| `NA -> ()
	| `Primary _ ->
	  t.kind <- `Primary (x, y)
	| `Primary_and_secondary (_, b) ->
	  t.kind <- `Primary_and_secondary ((x, y), b)

let maybe_set_secondary t ~x ~y =
  match color_at_xy t ~x ~y with
    | None -> ()
    | Some _c -> 
      match t.kind with
	| `NA | `Primary _ -> ()
	| `Primary_and_secondary (a, _) ->
	  t.kind <- `Primary_and_secondary (a, (x, y))

let rgb_to_coord t c =
  let (r, g, b) = Color.to_gl c in
  let dist (r', g', b') =
    sqrt ((r' -. r) ** 2. +. (g' -. g) ** 2. +. (b' -. b) ** 2.)
  in
  let best =
    let c' = Option.value_exn (color_at_xy_gl t ~x:t.x ~y:t.y) in
    ref (t.x, t.y, dist c')
  in
  hsl_iter (fun ~h ~s ~l ->
    let dist' = dist (hsl_to_rgb ~h ~s ~l) in
    let (_, _, dist) = !best in
    if dist' < dist then
      let x = t.x +. t.width *. h in
      let y = t.y +. t.height *. l in
      best := (x, y, dist'));
  let (x, y, _) = !best in
  (x, y)

let reset t a =
  match a.Animation.primary_color, a.Animation.secondary_color with
    | None, None ->
      t.kind <- `NA
    | Some pc, None ->
      t.kind <- `Primary (rgb_to_coord t pc)
    | Some pc, Some sc ->
      t.kind <- `Primary_and_secondary (rgb_to_coord t pc, rgb_to_coord t sc)
    | None, Some _ ->
      failwithf "animation '%s' has a secondary color but no primary"
	a.Animation.name ()
	
