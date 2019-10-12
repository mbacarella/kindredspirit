open Core
  
let hz = 44100
let num_samples_per_read = hz / 100

let pcm_data = Array.create ~len:100 0
let index = ref 0
  
let start ~sound_dev =
  let audio = Audio.init ~hz:44100 ~sound_dev ~num_samples_per_read in
  let bbuf = [| Array.create ~len:num_samples_per_read 0 |] in
  let rec loop () =
    Audio.read audio bbuf;
    let mean =
      (Array.fold_right bbuf.(0) ~init:0 ~f:(fun a b ->
        (abs a) + b))  / num_samples_per_read
    in
    pcm_data.(!index) <- mean;
    index := (succ !index) mod Array.length pcm_data;
    loop ()
  in
  loop ()
(*
let%test_unit _ =
  failwithf "unit test failed!" ()
;;
*)
