open Core.Std

let s16_le_into_floats s floats =
  let slen = String.length s in
  let flen = Array.length floats in
  if slen / 2 <> flen then
    failwithf "Float buffer size %d is not half of %d?!?"
      flen slen ();
  for i = 0 to (Array.length floats-1); do
    let sample =
      let a = Char.to_int s.[i*2] in
      let b = Char.to_int s.[i*2+1] in
      let sample = a lor (b lsl 8) in
      (* two's complement? are you kidding? *)
      if sample land 0x8000 = 0x8000 then
	Float.of_int ((((lnot sample) land 0xFFFF) + 1) * -1)
      else
	Float.of_int sample
    in
    if sample > 32768. then failwithf "%d: 16-bit signed? %f" i sample ();
    if sample < -32768. then failwithf "%d: 16-bit signed? %f" i sample ();
    floats.(i) <- sample
  done
