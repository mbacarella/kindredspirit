open Core.Std

(* Fftw3 segfaults when linked against Async, so we isolate it in this subprocess *)

module FFT = Fftw3.S

let hz = 44100
let sample_rate = 20
let num_samples_per_tick = hz / sample_rate

module Spectrogram = struct
  let f2i = Float.to_int
  let i2f = Float.of_int
  let bands = [| 60; 320; 640; 1280; 2560; 5120; 10240; 22000 |] ;;
  let nbands = Array.length bands
  let max_freq = bands.(nbands - 1)
  let freq_to_band =
    Array.init max_freq ~f:(fun freq ->
      let band_index_opt = ref None in
      Array.iteri bands ~f:(fun band_index band_limit ->
	match !band_index_opt with
	| Some _ -> () (* found already *)
        | None ->
	  if freq <= band_limit then
	    band_index_opt := Some band_index);
      Option.value_exn !band_index_opt)
  let get_array =
    Memo.unit (fun () -> Array.create Float.Set.empty ~len:(Array.length bands))
  let max_y = ref 0.
  let update dft =
    let dft_length = Bigarray.Array1.dim dft in
    for i = 0 to dft_length-1; do
      if !max_y < dft.{i} then max_y := dft.{i}
    done;
    let hist = get_array () in
    let freq_scale = 
      (Float.of_int max_freq) 
      /. (Float.of_int num_samples_per_tick)
    in
    Array.fill hist ~pos:0 ~len:(Array.length hist) (Float.Set.empty);
    for i = 0 to dft_length-1; do
      let freq = min max_freq (f2i (freq_scale *. (i2f i))) in
      let power = (max dft.{i} 0.) /. !max_y in
      let index = freq_to_band.(freq) in
      hist.(index) <- Set.add hist.(index) power
    done
end

let setup () =
  Portaudio.init ();
  let pulse_device_no =
    let num_devices = Portaudio.get_device_count () in
    List.find_exn (List.range 0 num_devices) ~f:(fun device_no ->
      let device_info = Portaudio.get_device_info device_no in
      device_info.Portaudio.d_name = "pulse")    
  in
  let stream =
    let instream =
      { Portaudio.channels = 1
      ; device = pulse_device_no
      ; sample_format = Portaudio.format_int16
      ; latency = 0.00001 }
    in
    Portaudio.open_stream (Some instream) None ~interleaved:true (Float.of_int hz) num_samples_per_tick []
  in
  Portaudio.start_stream stream;
  stream
    
let main () =
  let bbuf = [| Array.create ~len:num_samples_per_tick 0 |] in
  let stream = setup () in
  let rec loop () =
    Portaudio.read_stream stream bbuf 0 num_samples_per_tick;
    let dft =
      let input, output =
        let create n = FFT.Array1.create FFT.float Bigarray.c_layout n in
        create num_samples_per_tick,
        create num_samples_per_tick
      in
      let plan = FFT.Array1.r2r FFT.RODFT10 input output ~meas:FFT.Estimate in
      let buf = bbuf.(0) in
      assert (num_samples_per_tick = Bigarray.Array1.dim input);
      for i=0 to num_samples_per_tick-1; do
        input.{i} <- Float.of_int buf.(i)
      done;
      FFT.exec plan;
      output
    in
    Spectrogram.update dft;
    let hist = Spectrogram.get_array () in
    let power =
      let a = Set.fold hist.(0) ~init:0. ~f:max in
      let b = Set.fold hist.(1) ~init:0. ~f:max in
      a +. b /. 2.0
    in
    printf "%s\n%!"
      ({ Beat_detection.beat_magnitude = power } |> Beat_detection.sexp_of_t |> Sexp.to_string);
    loop ()
  in
  loop ()
    
let () =
  main ()
