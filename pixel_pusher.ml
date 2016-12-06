open! Core.Std
open! Async.Std

let discovery_port = 7331
let command_port = 9897

let reset_command = [| 0x40; 0x09; 0x2d; 0xa6; 0x15; 0xa5; 0xdd; 0xe5; 0x6a; 0x9d; 0x4d; 0x5a; 0xcf; 0x09; 0xaf; 0x50; 0x01 |]

module Beacon = struct
  module Strip_info = struct
    type t =
	{ rgbow         : bool (* high CRI strip *)
	; widepixels    : bool (* 48 bit/pixel RGBrgb *)
	; logarithmic   : bool (* LED has logarithmic response *)
	; motion        : bool (* a motion controller *)
	; notidempotent : bool (* a motion controller with side-effects *)
	}
    with sexp
    let of_wire bs =
      bitmatch bs with
	| { rgbow : 1
	  ; widepixels : 1
	  ; logarithmic : 1
	  ; motion : 1
	  ; notidempotent : 1
	  ; _unused : 3 } ->
	  { rgbow; widepixels; logarithmic; motion; notidempotent }
  end
  type t =
      { mac_address : string
      ; ip_address  : string
      ; device_type : int
      ; protocol_version : int
      ; vendor_id : int
      ; product_id : int
      ; hw_revision : int
      ; sw_revision : int
      ; link_speed : int (* in bits per second *)
      ; strips_attached : int
      ; max_strips_per_packet : int
      ; pixels_per_strip : int
      ; update_period : int (* in microseconds *)
      ; power_total : int (* in PWM units *)
      ; delta_sequence : int (* diff between received and expected sequence numbers *)
      ; controller_ordinal : int
      ; group_ordinal : int
      ; my_port : int
      ; strip_info : Strip_info.t Array.t
      ; protected : bool (* require qualified registry.getStrips() call (???) *)
      ; fixed_size : bool (* requires every datagram same size *)
      (* last host and port to drive this PP *)
      ; last_driven_ip : string
      ; last_driven_port : int 
      }
  with sexp

  let of_wire s =
    bitmatch Bitstring.bitstring_of_string s with
      | { mac_address : 48 : bitstring
	; ip_address  : 32
	; device_type : 8
	; protocol_version : 8
	; vendor_id : 16 : littleendian
	; product_id : 16 : littleendian 
	; hw_revision : 16 : littleendian
	; sw_revision : 16 : littleendian
	; link_speed : 32 : littleendian
	; strips_attached : 8
	; max_strips_per_packet : 8
	; pixels_per_strip : 16 : littleendian
	; update_period : 32 : littleendian
	; power_total : 32 : littleendian
	; delta_sequence : 32 : littleendian
	; controller_ordinal : 32 : littleendian
	; group_ordinal : 32 : littleendian
	; _artnet_universe : 16 : littleendian
	; _artnet_channel : 16 : littleendian
	; my_port : 16 : littleendian
	; strip_info : 64 : bitstring
	; _padding : 16
	; protected : 1
	; fixed_size : 1
	; _unused_flags : 30 : bitstring (* flags for the whole pusher *)
	; _segments : 32 : littleendian (* number of segments in each strip *)
	; _power_domain : 32 : littleendian
	; last_driven_ip : 32 : littleendian
	; last_driven_port : 16 : littleendian } ->

	if device_type <> 2 then failwithf "Unsupported device type: %d" device_type ();
	let mac_address =
	  bitmatch mac_address with
	    | { a : 8; b : 8; c : 8; d : 8; e : 8; f : 8 } ->
	      sprintf "%02x:%02x:%02x:%02x:%02x:%02x" a b c d e f
	in
	let strip_info =
	  bitmatch strip_info with
	    | { a : 8 : bitstring; b : 8 : bitstring
	      ; c : 8 : bitstring; d : 8 : bitstring
	      ; e : 8 : bitstring; f : 8 : bitstring
	      ; g : 8 : bitstring; h : 8 : bitstring} ->
	      let z = Strip_info.of_wire in
	      [| z a; z b; z c; z d; z e; z f; z g; z h |]
	in
	let to_int a = Option.value_exn (Int32.to_int a) in
	let ip_address = Unix.Inet_addr.inet4_addr_of_int32 ip_address |> Unix.Inet_addr.to_string in
	let last_driven_ip = Unix.Inet_addr.inet4_addr_of_int32 last_driven_ip |> Unix.Inet_addr.to_string in
	{ mac_address ; ip_address ; device_type ; protocol_version ; vendor_id ; product_id
	; hw_revision ; sw_revision ; link_speed = to_int link_speed ; strips_attached ; max_strips_per_packet
	; pixels_per_strip ; update_period = to_int update_period ; power_total = to_int power_total
	; delta_sequence = to_int delta_sequence ; controller_ordinal = to_int controller_ordinal 
	; group_ordinal = to_int group_ordinal; my_port; strip_info; protected; fixed_size
	; last_driven_ip; last_driven_port }
end

module Pixel = struct
  type t = { red : int; green: int; blue: int }
  let black = { red = 0; green = 0; blue = 0 }
  let white = { red = 255; green = 255; blue = 255 }
end

module Pusher_state = struct
  type t =
      { beacon_time : Time.t
      ; beacon      : Beacon.t
      ; mutable seq : int 
      ; matrix      : Pixel.t Array.t }
end
  
let known_pushers = String.Table.create ()

let start_discovery_listener () =
  printf "*** Starting Pixel Pusher listener on port %d...\n%!" discovery_port;
  let addr = `Inet (Unix.Inet_addr.bind_any, discovery_port) in
  Async_extra.Udp.bind addr
  >>= fun socket ->
  let fd = Async_extra.Import.Socket.fd socket in
  Async_extra.Udp.recvfrom_loop fd
    (fun buf addr ->
      let addr_s = Async_extra.Import.Socket.Address.Inet.to_string addr in
      let beacon = Iobuf.to_string buf |> Beacon.of_wire in
      let key = beacon.Beacon.ip_address in
      let my_port = beacon.Beacon.my_port in
      if my_port <> command_port then
	failwithf "*** PP %s's command port is %d, not %d" key my_port command_port ();
      let num_pixels = Beacon.(beacon.pixels_per_strip * beacon.strips_attached) in
      match Hashtbl.find known_pushers key with
	| Some state ->
	  let mlen = Array.length state.Pusher_state.matrix in
	  if mlen <> num_pixels then
	    failwithf "*** PP %s's dimensions changed from %d to %d pixels" key mlen num_pixels ();
	  let data = { state with Pusher_state.beacon_time = Time.now (); beacon } in
	  Hashtbl.replace known_pushers ~key ~data
	| None ->
	  printf "*** Discovered new Pixel Pusher: %s\n" addr_s;
	  printf "%s\n" (Beacon.sexp_of_t beacon |> Sexp.to_string_hum ~indent:2);
	  let matrix = Array.init num_pixels ~f:(fun _ -> Pixel.black) in
	  Hashtbl.add_exn known_pushers ~key ~data:{
	      Pusher_state.beacon_time = Time.now ()
	    ; beacon; seq = 0; matrix })

let send_pixels_to_pushers socket =
  Hashtbl.iter known_pushers ~f:(fun ~key:ip ~data:pusher ->
    let beacon = pusher.Pusher_state.beacon in
    let addr = Unix.ADDR_INET (Unix.Inet_addr.of_string ip, command_port) in
    let pixels_per_strip = beacon.Beacon.pixels_per_strip in
    let packet_size num_strips =
        4 (* 32-bit sequence *)
      + 1*num_strips (* 8-bit strip indices *)
      + 3*num_strips*pixels_per_strip (* 24 bit rgb data *)
    in
    let strips_attached = beacon.Beacon.strips_attached in
    let max_strips_per_packet = beacon.Beacon.max_strips_per_packet in
    let buf = String.create (packet_size max_strips_per_packet) in
    let stripss =
      List.groupi (List.range 0 strips_attached)
	~break:(fun i _x _y -> i mod max_strips_per_packet = 0)
    in
    let matrix = pusher.Pusher_state.matrix in
    List.iteri stripss ~f:(fun seq_index strips ->
      let seq = pusher.Pusher_state.seq + seq_index in
      let char = Char.of_int_exn in
      buf.[0] <- char ((seq lsr 24) land 0xFF);
      buf.[1] <- char ((seq lsr 16) land 0xFF);
      buf.[2] <- char ((seq lsr  8) land 0xFF);
      buf.[3] <- char (seq land 0xFF);
      List.iteri strips ~f:(fun strip_index strip_num ->
	let strip_base = 4 + strip_index*(1+pixels_per_strip*3) in
	buf.[strip_base] <- char strip_num;
	let pixels_base = strip_base+1 in
	for pixel_num=0 to pixels_per_strip-1; do
	  let pixel = matrix.(strip_num*pixels_per_strip + pixel_num) in
	  buf.[pixels_base + pixel_num*3    ] <- char pixel.red;
	  buf.[pixels_base + pixel_num*3 + 1] <- char pixel.green;
	  buf.[pixels_base + pixel_num*3 + 2] <- char pixel.blue
	done);
      assert ((List.length strips) <= max_strips_per_packet);
      let bytes_to_send = packet_size (List.length strips) in
      let bytes_sent =
	printf "*** Sending %d byte packet to %s:%d\n" bytes_to_send ip command_port;
	Core.Std.Unix.sendto socket ~buf ~pos:0 ~len:bytes_to_send ~mode:[] ~addr
      in
      if bytes_sent < bytes_to_send then
	failwithf "Failed to send %d bytes to %s (%d bytes short)"
	  bytes_sent ip (String.length buf - bytes_sent) ());
    pusher.Pusher_state.seq <- pusher.Pusher_state.seq + (List.length stripss))

let rec update_loop i =
  let ri () = Random.int 256 in
  let pixel = { Pixel.red = (ri ()); green = (ri ()); blue = (ri ()) } in
  Hashtbl.iter known_pushers ~f:(fun ~key:_ ~data:pusher ->
    let matrix = pusher.Pusher_state.matrix in
    for i=0 to Array.length matrix - 1; do
      matrix.(i) <- pixel
    done);
  Exn.protectx Core.Std.Unix.(socket ~domain:PF_INET ~kind:SOCK_DGRAM ~protocol:0)
    ~finally:(Core.Std.Unix.close ~restart:true)
    ~f:send_pixels_to_pushers;
  Clock.after (Time.Span.of_ms 100.)
  >>= fun () -> update_loop ((i+10) mod 256)

module Strip = struct
  type t =
      { strip_number: int
      ; strip_length : int
      ; controller_id : int
      ; group_id : int }
  let set_pixel t ~color ~index =
    ignore color;
    ignore index;
    ignore t
end
  
let get_strips () =
  (* XXX: cache this *)
  Hashtbl.fold known_pushers ~init:[] ~f:(fun ~key:_ ~data acc ->
    let beacon = data.Pusher_state.beacon in
    List.fold_left (List.range 0 beacon.Beacon.strips_attached) ~init:acc ~f:(fun acc i ->
      { Strip.strip_number = i
      ; strip_length = beacon.Beacon.pixels_per_strip
      ; controller_id = beacon.Beacon.controller_ordinal
      ; group_id = beacon.Beacon.group_ordinal } :: acc))
	  
