(** Verify lossless JPEG by decoding back to PPM *)

let write_ppm filename width height pixels =
  let oc = open_out_bin filename in
  Printf.fprintf oc "P6\n%d %d\n255\n" width height;
  for i = 0 to (width * height * 3) - 1 do
    output_byte oc (Bigarray.Array1.get pixels i)
  done;
  close_out oc

let () =
  let input = "color_wheel_lossless.jpg" in
  let output = "color_wheel_lossless_decoded.ppm" in
  let original = "color_wheel.ppm" in

  Printf.printf "Decoding %s...\n" input;
  let image = Jpeg.read input in
  Printf.printf "  Size: %dx%d\n" image.width image.height;

  Printf.printf "Writing %s...\n" output;
  write_ppm output image.width image.height image.pixels;

  (* Compare with original *)
  Printf.printf "\nComparing with original %s...\n" original;

  let ic = open_in_bin original in
  ignore (input_line ic);  (* P6 *)
  ignore (input_line ic);  (* dimensions *)
  ignore (input_line ic);  (* maxval *)

  let diff_count = ref 0 in
  let max_diff = ref 0 in
  for i = 0 to (image.width * image.height * 3) - 1 do
    let orig = input_byte ic in
    let decoded = Bigarray.Array1.get image.pixels i in
    let d = abs (orig - decoded) in
    if d > 0 then incr diff_count;
    if d > !max_diff then max_diff := d
  done;
  close_in ic;

  Printf.printf "  Pixel differences: %d\n" !diff_count;
  Printf.printf "  Max difference: %d\n" !max_diff;

  if !diff_count = 0 then
    Printf.printf "\nSUCCESS: Files are identical!\n"
  else
    Printf.printf "\nFAILED: Files differ\n";

  Printf.printf "\nYou can now compare visually:\n";
  Printf.printf "  Original: %s\n" original;
  Printf.printf "  Decoded:  %s\n" output;
  Printf.printf "\nOr use: diff <(xxd %s) <(xxd %s)\n" original output
