(** Convert color wheel PPM to lossless JPEG *)

(* Simple PPM reader for P6 format *)
let read_ppm filename =
  let ic = open_in_bin filename in
  (* Read header *)
  let magic = input_line ic in
  if magic <> "P6" then failwith "Not a P6 PPM file";
  (* Skip comments *)
  let rec skip_comments () =
    let line = input_line ic in
    if String.length line > 0 && line.[0] = '#' then skip_comments ()
    else line
  in
  let dims = skip_comments () in
  let parts = String.split_on_char ' ' dims in
  let width, height = match parts with
    | [w; h] -> (int_of_string w, int_of_string h)
    | _ -> failwith "Invalid dimensions"
  in
  let maxval = int_of_string (input_line ic) in
  if maxval <> 255 then failwith "Only 8-bit PPM supported";

  (* Read pixel data *)
  let pixels = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (width * height * 3) in
  for i = 0 to (width * height * 3) - 1 do
    Bigarray.Array1.set pixels i (input_byte ic)
  done;
  close_in ic;
  (width, height, pixels)

let () =
  let input_file = "color_wheel.ppm" in
  let output_file = "color_wheel_lossless.jpg" in

  Printf.printf "Reading %s...\n" input_file;
  let width, height, pixels = read_ppm input_file in
  Printf.printf "Image size: %dx%d\n" width height;

  let image = Jpeg.create_image width height pixels in

  (* Try different predictors and compare sizes *)
  Printf.printf "\nEncoding with different predictors:\n";
  for pred = 1 to 7 do
    let options = {
      Jpeg.quality = 100;  (* ignored for lossless *)
      subsampling = Jpeg.Sub_444;  (* ignored for lossless *)
      color_mode = Jpeg.Color;
      encoding_mode = Jpeg.Lossless;
      restart_interval = 0;
      precision = Jpeg.Precision_8;
      entropy_coding = Jpeg.Huffman;
      predictor = pred;
      point_transform = 0;
    } in
    let encoded = Jpeg.write_bytes_with_options options image in
    Printf.printf "  Predictor %d: %d bytes\n" pred (Bytes.length encoded)
  done;

  (* Use predictor 4 (Ra+Rb-Rc) which often gives best compression *)
  let best_predictor = 4 in
  Printf.printf "\nUsing predictor %d for output...\n" best_predictor;

  let options = {
    Jpeg.quality = 100;
    subsampling = Jpeg.Sub_444;
    color_mode = Jpeg.Color;
    encoding_mode = Jpeg.Lossless;
    restart_interval = 0;
    precision = Jpeg.Precision_8;
    entropy_coding = Jpeg.Huffman;
    predictor = best_predictor;
    point_transform = 0;
  } in

  Jpeg.write_with_options options output_file image;

  (* Get file size *)
  let stat = Unix.stat output_file in
  Printf.printf "Written %s (%d bytes)\n" output_file (stat.Unix.st_size);

  (* Verify roundtrip *)
  Printf.printf "\nVerifying lossless roundtrip...\n";
  let decoded = Jpeg.read output_file in

  let diff_count = ref 0 in
  for i = 0 to (width * height * 3) - 1 do
    if Bigarray.Array1.get pixels i <> Bigarray.Array1.get decoded.pixels i then
      incr diff_count
  done;

  if !diff_count = 0 then
    Printf.printf "SUCCESS: Perfect lossless roundtrip (0 pixel differences)\n"
  else
    Printf.printf "ERROR: %d pixel differences found\n" !diff_count;

  (* Compare with lossy JPEG *)
  Printf.printf "\nFor comparison:\n";
  Printf.printf "  Original PPM: %d bytes\n" (width * height * 3 + 50);
  let lossy_options = { Jpeg.default_encode_options with quality = 95 } in
  let lossy = Jpeg.write_bytes_with_options lossy_options image in
  Printf.printf "  Lossy JPEG (q95): %d bytes\n" (Bytes.length lossy);
  Printf.printf "  Lossless JPEG: %d bytes\n" (stat.Unix.st_size)
