open Core.Std
open Async.Std

module FFT = Fftw3.S
  
let hz = 44100
let sample_size = 2
x(* TODO: should this be 100? *)
let num_samples_per_tick = hz / 50

module Spectrogram = struct
  let bands = [| 160; 320; 640; 1280; 2560; 5120; 10240; 22000 |] ;;
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

let beat = ref 0.
  
let start () =
  Process.create ~prog:"arecord" ~args:["-f"; "S16_LE"; "-t"; "raw"; "-c1"; "-r44100"] ()
  >>= fun subprocess_opt ->
  let subprocess = Or_error.ok_exn subprocess_opt in
  let reader = Process.stdout subprocess in
  let buffer_size = num_samples_per_tick * sample_size in
  let buf = String.create buffer_size in
  let create n = FFT.Array1.create FFT.float Bigarray.c_layout n in
  let input = create num_samples_per_tick in
  let output = create num_samples_per_tick in
  let float_samples = Array.create 0. ~len:num_samples_per_tick in
  let rec loop () =
    Reader.read reader ~pos:0 ~len:buffer_size buf
    >>= function
      | `Eof -> printf "arecord exited!!!"; return ()
      | `Ok bytes_read ->
        if bytes_read <> buffer_size then
          failwithf "Reader.read returned %d on %d" bytes_read buffer_size ();
        let dft =
          Bits.s16_le_into_floats buf float_samples;
          for i = 0 to (Bigarray.Array1.dim input)-1; do
            input.(i) <- float_samples.(i);
          done;
          let ffst = FFT.Array1.r2r FFT.RODFT10 input output ~meas:FFT.Estimate in
          FFT.exec ffst;
          output
        in
        Spectrogram.update dft;
        let hist = Spectrogram.get_array () in
        let lowest_band = hist.(0) in
        let sum = Set.fold lowest_band ~init:0. ~f:( +. ) in
        let mean = sum /. (Set.length lowest_band |> Float.of_int) in
        beat := mean;
        loop ()
  in
  loop ()
