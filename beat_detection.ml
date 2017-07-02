open Core
open Async

type t = { beat_magnitude : float } [@@deriving sexp] ;;

let beat = ref 0.
let exe = "./beat_detection_helper.native"

let rec reader_loop reader =
  Reader.read_line reader >>= function
    | `Eof -> failwithf "%s exited unexpectedly" exe ()
    | `Ok line ->
      let t = Sexp.of_string line |> t_of_sexp in
      beat := t.beat_magnitude;
      reader_loop reader

let start () =
  Process.create ~prog:exe ~args:[] ()
  >>| fun result ->
  let subprocess = Or_error.ok_exn result in
  let reader = Process.stdout subprocess in
  don't_wait_for (reader_loop reader)

(*
let beat_updater_loop reader =

  let input, output =
    let create n = FFT.Array1.create FFT.float Bigarray.c_layout n in
    create num_samples_per_tick,
    create num_samples_per_tick
  in
  let plan = FFT.Array1.r2r FFT.RODFT10 input output ~meas:FFT.Estimate in
  let float_samples = Array.create 0. ~len:num_samples_per_tick in
  let rec loop () =
    let buf = String.create buffer_size in
    Reader.really_read reader ~pos:0 ~len:buffer_size buf
    >>= function
      | `Eof n -> printf "EOF on mic input (read %d)" n; return ()
      | `Ok ->
        let beat_magnitude =
          let dft =
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
        beat := beat_magnitude;
        Clock.after (sec 1.0) >>= fun () ->
        loop ()
  in
  loop ()

let start () =
  Process.create ~prog:"arecord"
    ~args:["-f"; "S16_LE"; "-t"; "raw"; "-c1"; sprintf "-r%d" hz] ()
  >>| fun subprocess_opt ->
  let subprocess = Or_error.ok_exn subprocess_opt in
  let reader = Process.stdout subprocess in
  don't_wait_for (beat_updater_loop reader)
*)
