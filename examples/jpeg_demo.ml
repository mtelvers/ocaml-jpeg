(** JPEG library demonstration *)

let () =
  (* Create a 64x64 gradient test image *)
  let width = 64 in
  let height = 64 in
  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in

  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let idx = ((y * width) + x) * 3 in
      (* Create a colorful gradient pattern *)
      Bigarray.Array1.set pixels idx (x * 4);
      (* R: horizontal gradient *)
      Bigarray.Array1.set pixels (idx + 1) (y * 4);
      (* G: vertical gradient *)
      Bigarray.Array1.set pixels (idx + 2) ((x + y) * 2)
      (* B: diagonal gradient *)
    done
  done;

  let image = Jpeg.create_image width height pixels in

  Printf.printf "Created test image: %dx%d\n" width height;

  (* Encode to JPEG with quality 85 *)
  let jpeg_data = Jpeg.write_bytes ~quality:85 image in
  Printf.printf "Encoded to JPEG: %d bytes\n" (Bytes.length jpeg_data);

  (* Decode the JPEG back *)
  let decoded = Jpeg.read_bytes jpeg_data in
  Printf.printf "Decoded image: %dx%d\n" decoded.Jpeg.width decoded.Jpeg.height;

  (* Compare pixels *)
  let total_error = ref 0 in
  let max_error = ref 0 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let r1, g1, b1 = Jpeg.get_pixel image x y in
      let r2, g2, b2 = Jpeg.get_pixel decoded x y in
      let err = abs (r1 - r2) + abs (g1 - g2) + abs (b1 - b2) in
      total_error := !total_error + err;
      if err > !max_error then max_error := err
    done
  done;

  let avg_error =
    float_of_int !total_error /. float_of_int (width * height * 3)
  in
  Printf.printf "Compression statistics:\n";
  Printf.printf "  Average pixel error: %.2f\n" avg_error;
  Printf.printf "  Max pixel error: %d\n" !max_error;
  Printf.printf "  Compression ratio: %.1fx\n"
    (float_of_int (width * height * 3) /. float_of_int (Bytes.length jpeg_data));

  (* Test different quality levels *)
  Printf.printf "\nQuality comparison:\n";
  List.iter
    (fun quality ->
      let data = Jpeg.write_bytes ~quality image in
      Printf.printf "  Q%d: %d bytes (%.1fx compression)\n" quality
        (Bytes.length data)
        (float_of_int (width * height * 3) /. float_of_int (Bytes.length data)))
    [ 25; 50; 75; 90; 100 ];

  Printf.printf "\nDemo completed successfully!\n"
