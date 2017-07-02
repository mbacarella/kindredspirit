open Core
  
type ('a, 'b, 'c, 'd) t =
    { stream : ('a, 'b, 'c, 'd) Portaudio.stream
    ; num_samples_per_read : int }

let dump_available_devices () =
  let num = Portaudio.get_device_count () in
  List.iter (List.range 0 num) ~f:(fun i ->
    let info = Portaudio.get_device_info i in
    eprintf "%d: %s\n" i info.Portaudio.d_name)

let init ~hz ~num_samples_per_read ~sound_dev =
  Portaudio.init ();
  let pulse_device_no =
    let num_devices = Portaudio.get_device_count () in
    try
      List.find_exn (List.range 0 num_devices) ~f:(fun device_no ->
        let device_info = Portaudio.get_device_info device_no in
        device_info.Portaudio.d_name = sound_dev)
    with
      | Not_found ->
        eprintf "*** Couldn't find device.  List of available devices:\n%!";
        dump_available_devices ();
        exit 1
  in
  let stream =
    let instream =
      { Portaudio.channels = 1
      ; device = pulse_device_no
      ; sample_format = Portaudio.format_int16
      ; latency = 0.00001 }
    in
    Portaudio.open_stream (Some instream) None ~interleaved:true (Float.of_int hz) num_samples_per_read []
  in
  Portaudio.start_stream stream;
  let t = { stream; num_samples_per_read } in
  t

let read t bbuf =
  Portaudio.read_stream t.stream bbuf 0 t.num_samples_per_read
