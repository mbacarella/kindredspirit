open Core.Std

(* Fftw3 segfaults when linked against Async, so we isolate it in this subprocess *)

module FFT = Fftw3.S

let hz = 44100

let sample_size = 2
let sample_rate = 100
let num_samples_per_tick = hz / sample_rate

module Spectrogram = struct
  let f2i = Float.to_int
  let i2f = Float.of_int
  let bands = [| 80; 160; 320; 640; 1280; 2560; 5120; 10240; 22000 |] ;;
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
    Memo.unit (fun () -> Array.create (Float.Set.empty) ~len:(Array.length bands))
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
  
let main () =
  let buffer_size = num_samples_per_tick * sample_size in
  let cmd = "arecord -f S16_LE -t raw -c1 -r44100" in
  let in_channel = Core.Std.Unix.open_process_in cmd in
  let buf = String.create buffer_size in
  let rec loop () =
    match In_channel.really_input in_channel ~pos:0 ~len:buffer_size ~buf with
      | None -> failwithf "In_channel.really_input: error" ()
      | Some () ->
        let beat_magnitude =
          let dft =
            let input, output =
              let create n = FFT.Array1.create FFT.float Bigarray.c_layout n in
              create num_samples_per_tick,
              create num_samples_per_tick
            in
            let plan = FFT.Array1.r2r FFT.RODFT10 input output ~meas:FFT.Estimate in
            let float_samples = Array.create 0. ~len:num_samples_per_tick in
            Bits.s16_le_into_floats buf float_samples;
            for i = 0 to (Bigarray.Array1.dim input)-1; do
              input.{i} <- float_samples.(i);
            done;
            FFT.exec plan;
            output
          in
          Spectrogram.update dft;
          let hist = Spectrogram.get_array () in
          let lowest_band = hist.(0) in
          let sum = Set.fold lowest_band ~init:0. ~f:( +. ) in
          let mean = sum /. (Set.length lowest_band |> Float.of_int) in
          mean *. (Float.of_int sample_rate)
        in
        let update = { Beat_detection.beat_magnitude = beat_magnitude } in
        printf "%s\n%!" (Beat_detection.sexp_of_t update |> Sexp.to_string);
        loop ()
  in
  loop ()

let () =
  main ()
    
