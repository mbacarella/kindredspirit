open! Core.Std
  
type t =
    { name : string
    ; update : (t -> unit)
    ; mutable model : Model.t option
    ; mutable primary_color : Color.t option
    ; mutable secondary_color : Color.t option }

let dj = { Coordinate.x=120.; y=30.; z=30. }
          
let init t model =
  { t with model = Some (Model.dup model) }

let with_model t ~f =
  match t.model with
    | None -> failwithf "model not initialized: %s" t.name ()
    | Some model ->
      f t model

let iter_pixels ~f =
  with_model ~f:(fun t model -> List.iter model.Model.virtual_pixels ~f:(fun vp -> f t vp))

let empty = { name="empty"; update=ignore; model=None; primary_color=None; secondary_color=None }
let off_animation = { empty with name="off" }
let off = off_animation
  
let solid_animation =
  { empty with
    name = "solid"
  ; update = iter_pixels ~f:(fun t vp -> vp.Virtual_pixel.color <- Option.value_exn t.primary_color)
  ; primary_color = Some Color.green }

let noise_animation =
  { empty with
    name = "noise"
  ; update = iter_pixels ~f:(fun _t vp -> vp.Virtual_pixel.color <- Color.rand ()) }

let twinkle_animation =
  { empty with
    name = "twinkle"
  ; update = iter_pixels ~f:(fun _t vp ->
    vp.Virtual_pixel.color <-
      begin
        if Random.int 10 = 0 then Color.rand ()
        else Color.black
      end) }

module Sticks_rnd = struct
  let ticks = ref 0
  let gen () = Array.init 100 ~f:(fun _ -> Color.rand ())
  let colors = ref (gen ())
  let update t =
    if !ticks = 0 then colors := gen ();
    iter_pixels t ~f:(fun _ vp ->
	vp.Virtual_pixel.color <- (!colors).(vp.Virtual_pixel.controller_id * 8 + vp.Virtual_pixel.strip_id));
    ticks := (succ !ticks) mod 100
 
  let animation =
    { empty with
      name = "sticks-rnd"
    ; update }
end
  
module Rain = struct
  let ticks = ref 0
  let height = 140.
  let update t =
    let pos = Float.of_int (!ticks mod (Float.to_int height)) in
    iter_pixels t ~f:(fun _ vp ->
      vp.Virtual_pixel.color <- Option.value_exn
	(let coord = vp.Virtual_pixel.coord in
	 let dist = coord.Coordinate.y -. pos in
	 if dist < 0. then t.secondary_color
	 else if dist < 1. then t.primary_color
	 else
	   Option.map t.primary_color ~f:(Color.shade ~factor:((dist /. height) *. 10.))));
    incr ticks
      
  let animation =
    { empty with
      name = "rain"
    ; update
    ; primary_color = Some (Color.of_hex_int 0x660E6F)
    ; secondary_color = Some Color.black }
end

module Rain_rnd = struct
  let ticks = ref 0
  let height = 140.
  let color = ref (Color.rand ())
  let update t =
    let pos = Float.of_int (!ticks mod (Float.to_int height)) in
    iter_pixels t ~f:(fun _ vp ->
      vp.Virtual_pixel.color <-
	(let coord = vp.Virtual_pixel.coord in
	 let dist = coord.Coordinate.y -. pos in
	 if dist < 0. then Color.black
         else if dist < 1. then !color
	 else Color.shade ~factor:((dist /. height) *. 10.) !color));
    ticks := (succ !ticks) mod (Float.to_int height);
    if !ticks = 0 then color := Color.rand ()
    
  let animation =
    { empty with
      name = "rain-rnd"
    ; update
    ; primary_color = Some (Color.of_hex_int 0x660E6F) }
end

module Solid_glow = struct
  let ticks = ref 0
  let update t =
    let phase =
      let phase = Float.of_int (!ticks mod 200) in
      if phase > 100. then 100. -. (phase -. 100.)
      else phase
    in
    (* TODO: drops out too fast near the end *)
    iter_pixels t ~f:(fun _ vp ->
      let c =
	Option.value_exn t.primary_color
        |> Color.shade ~factor:(1.0 -. (phase /. 100.))
      in
      vp.Virtual_pixel.color <- c);
    incr ticks
  let animation =
    { empty with
      name = "solidglow"
    ; update
    ; primary_color = Some Color.green }
end 

let rainbow_colors =
  Memo.unit (fun () ->
    Array.init 768 ~f:(fun i ->
      { Color.
        r =
          if i < 256 then 255-i
          else if i >= 512 then i-512
          else 0
      ; g =
          if i < 256 then i
          else if i >= 256 && i < 512 then 511-i
          else 0
      ; b =
          if i >= 512 then 767-i
          else if i >= 256 then i-256
          else 0 }))

module Rainbow_solid = struct
  let i = ref 0
  let update t =
    let cols = rainbow_colors () in
    let c = cols.(!i) in
    iter_pixels t ~f:(fun _ vp ->
      vp.Virtual_pixel.color <- c);
    i := (succ !i) mod (Array.length cols)

  let animation =
    { empty with name = "rainbow-solid"; update }
end

module Rainbow_dj = struct
  let i = ref 0
  let update t =
    let cols = rainbow_colors () in
    let colsl = Array.length cols in
    iter_pixels t ~f:(fun _ vp ->
      let d = Coordinate.dist dj (Virtual_pixel.coord vp) |> Float.to_int in
      let index = (!i*2 + d*3) mod colsl in 
      vp.Virtual_pixel.color <- cols.(index)
    );
    incr i
      
  let animation =
    { empty with name = "rainbow-dj"; update }
end

module Radiate_dj = struct
  let ticks = ref 0
    
  let update t =
    let i = Float.of_int !ticks in
    iter_pixels t ~f:(fun _ vp ->
      let d = Coordinate.dist dj vp.Virtual_pixel.coord in
      vp.Virtual_pixel.color <-
	if d >= i && d < (i +. 40.) then Option.value_exn t.primary_color
	else if d >= (i +. 40.) && d < (i +. 80.) then Option.value_exn t.secondary_color
	else Color.black);
    ticks := (succ !ticks) mod 200
      
  let animation =
    { empty with name="radiate-dj"
    ; update
    ; primary_color = Some Color.{r=0x99; g=0; b=0 }
    ; secondary_color = Some Color.black
    }
end

module Scan_dj = struct
  let ticks = ref 0
  let color = ref (Color.rand ())

  let update ~rnd t =
    let pos = Float.of_int (!ticks mod 300) in
    let c = if rnd then Some !color else t.primary_color in
    iter_pixels t ~f:(fun _ vp ->
      vp.Virtual_pixel.color <- Option.value_exn
        (let dist = Coordinate.dist vp.Virtual_pixel.coord dj in
         if dist >= pos && dist < pos +. 40. then c
         else Some Color.black));
    ticks := (succ !ticks) mod 190;
    if rnd && !ticks = 0 then color := Color.rand ()

  let reg_animation =
    { empty with name="scan-dj"
    ; update = update ~rnd:false
    ; primary_color = Some Color.{r=0x99; g=0; b=0 } }

  let rnd_animation =
    { empty with name="scan-dj-rnd"
    ; update = update ~rnd:true }
end
  
module Strip_walk = struct
  let ticks = ref 0
  let update t =
    let id = !ticks / 10 in
    iter_pixels t ~f:(fun _ vp ->
      vp.Virtual_pixel.color <-
        if vp.Virtual_pixel.pixel_id = id then Option.value_exn t.primary_color
        else Option.value_exn t.secondary_color);
    ticks := (succ !ticks) mod 1000
      
  let animation =
    { empty with name="strip-walk"
    ; primary_color = Some Color.green
    ; secondary_color = Some Color.{r=0x66; g=0x66; b=0x66 } 
    ; update }
end 

  
(* let y_buckets = *)
(*   (\* Bucket pixels along y axis *\) *)
(*   Memo.general (fun t -> *)
(*     let map = *)
(*       with_model t ~f:(fun _t model -> *)
(*         List.fold_left (Model.virtual_pixels model) ~init:Map.Poly.empty ~f:(fun map vp -> *)
(*           let key = Virtual_pixel.coord vp |> Coordinate.y |> Float.to_int in *)
(*           Map.add_multi map ~key ~data:vp)) *)
(*     in *)
(*     Map.to_alist map |> List.fold_left ~init:Map.Poly.empty ~f:(fun map (key, vps) -> *)
(*       let data = *)
(*         Array.of_list (List.sort vps ~cmp:(fun a b -> *)
(*           Float.compare *)
(*             (Virtual_pixel.coord a |> Coordinate.x) *)
(*             (Virtual_pixel.coord b |> Coordinate.x))) *)
(*       in *)
(*       Map.add map ~key ~data)) *)

module Flame = struct
  let ticks = ref 0
    
  let update t =
    (* memo some of this *)
    let map =
      let map =
        with_model t ~f:(fun _t model ->
          List.fold_left (Model.virtual_pixels model) ~init:Map.Poly.empty ~f:(fun map vp ->
            let key = Virtual_pixel.coord vp |> Coordinate.y |> Float.to_int in
            Map.add_multi map ~key ~data:vp))
      in
      Map.to_alist map |> List.fold_left ~init:Map.Poly.empty ~f:(fun map (key, vps) ->
        let data =
          Array.of_list (List.sort vps ~cmp:(fun a b ->
            Float.compare
              (Virtual_pixel.coord a |> Coordinate.x)
              (Virtual_pixel.coord b |> Coordinate.x)))
        in
        Map.add map ~key ~data)
    in
    let y_range = Map.keys map |> List.sort ~cmp:Int.compare |> Array.of_list in
    let min_y, max_y = y_range.(0), y_range.(Array.length y_range-1) in
    assert (min_y < (max_y+1));
    let row y = Map.find_exn map y in
    Array.iter (row max_y) ~f:(fun vp ->
      vp.Virtual_pixel.color <- Option.value_exn t.primary_color);
    List.iter (List.range 0 (Array.length y_range-1)) ~f:(fun i ->
      let y = y_range.(i) in
      let lower_y = y_range.(succ i) in
      let cur_row = row y in
      let prev_row = row lower_y in
      Array.iteri cur_row ~f:(fun j vp ->
        let pct = (Float.of_int j) /. Float.of_int (Array.length cur_row) in
        let prev_index =
          let index = (Float.of_int (Array.length prev_row)) *. pct |> Float.to_int in
          min (pred (Array.length prev_row)) index
        in
        let prev_color = Virtual_pixel.color (prev_row.(prev_index)) in
        let color =
          Color.shade ~factor:(Random.float 0.025) prev_color
        in
        vp.Virtual_pixel.color <- color));
    incr ticks

  let animation =
    { empty with name="flame"
    ; primary_color = Some (Color.rand ())
    ; update } 
end
  
let live_all =
  [ off_animation
  ; solid_animation
  ; twinkle_animation
  ; noise_animation
  ; Sticks_rnd.animation
  ; Strip_walk.animation 
  ; Rain.animation
  ; Rain_rnd.animation
  ; Scan_dj.reg_animation
  ; Scan_dj.rnd_animation
  ; Solid_glow.animation
  ; Rainbow_solid.animation
  ; Rainbow_dj.animation
  ; Radiate_dj.animation
  ; Flame.animation ]

let test_all () =
  off_animation :: List.concat_map (List.range 0 8) ~f:(fun controller_id ->
    List.map (List.range 0 8) ~f:(fun strip_id ->
      let name = sprintf "test-%d:%d" controller_id strip_id in
      let update t =
	iter_pixels t ~f:(fun _ vp ->
	  vp.Virtual_pixel.color <-
	    if vp.Virtual_pixel.controller_id = controller_id
            && vp.Virtual_pixel.strip_id = strip_id
	    then Option.value_exn t.primary_color
	    else Option.value_exn t.secondary_color)
      in
      { empty with name; update
      ; primary_color = Some Color.white
      ; secondary_color = Some Color.black }))

let mode = ref `live
  
let all = Memo.unit (fun () ->
  match !mode with
    | `live -> live_all
    | `test -> test_all ())
