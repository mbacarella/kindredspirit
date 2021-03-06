open Core
open Async

let beat = ref 0.
let exe = "./beat_detection_helper.exe"

let spectro = Array.create ~len:20 [||]
let index = ref 0

let rec reader_loop ~reader ~err_reader =
  Reader.read_line reader >>= function
    | `Eof ->
      begin
        Reader.contents err_reader >>= fun err_contents ->
        eprintf "*** Beat detection helper closed stdout.  Contents of stderr:\n%s\n" err_contents;
        return ()
      end
    | `Ok line ->
      let freq_hist =
        String.split ~on:',' line |> List.map ~f:(fun fh ->
          match String.split ~on:':' fh with
          | band :: power :: [] -> Int.of_string band, Float.of_string power
          | _lst -> failwithf "badly formatted beat_detection_helper line: %s" line ())
      in
      (* extract "beat" *)
      beat :=
        begin match freq_hist with
        | (60, pow1) :: (320, pow2) :: _lst -> (pow1 +. pow2) /. 2.0
        | _ -> failwithf "didn't find expected frequencies in beat_detection_helper line: %s" line ()
        end;
      (* extract the entire histogram *)
      spectro.(!index) <- Array.of_list freq_hist;
      index := (succ !index) mod Array.length spectro;
      reader_loop ~reader ~err_reader

let start ~sound_dev =
  Process.create ~prog:exe ~args:[sound_dev] ()
  >>| fun result ->
  let subprocess = Or_error.ok_exn result in
  let reader = Process.stdout subprocess in
  let err_reader = Process.stderr subprocess in
  don't_wait_for (reader_loop ~reader ~err_reader)
