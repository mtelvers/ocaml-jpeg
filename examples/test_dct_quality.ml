(** Test DCT arithmetic coding quality across encoding modes and quality levels.
    Creates a 64x64 gradient image and measures max pixel error for each
    combination of entropy coding (Huffman/Arithmetic) and encoding mode
    (Baseline/Progressive) at quality=95 and quality=100. *)

let () =
  let width = 64 in
  let height = 64 in

  (* Create a gradient test image: R increases left-to-right,
     G increases top-to-bottom, B is a diagonal gradient *)
  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let idx = ((y * width) + x) * 3 in
      let r = (x * 255) / (width - 1) in
      let g = (y * 255) / (height - 1) in
      let b = ((x + y) * 255) / (width + height - 2) in
      Bigarray.Array1.set pixels idx r;
      Bigarray.Array1.set pixels (idx + 1) g;
      Bigarray.Array1.set pixels (idx + 2) b
    done
  done;
  let original = Jpeg.create_image width height pixels in

  (* Measure max pixel error between original and decoded images *)
  let max_pixel_error (orig : Jpeg.image) (decoded : Jpeg.image) =
    let max_err = ref 0 in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let (r1, g1, b1) = Jpeg.get_pixel orig x y in
        let (r2, g2, b2) = Jpeg.get_pixel decoded x y in
        let dr = abs (r1 - r2) in
        let dg = abs (g1 - g2) in
        let db = abs (b1 - b2) in
        let err = max dr (max dg db) in
        if err > !max_err then max_err := err
      done
    done;
    !max_err
  in

  (* Also compute mean absolute error *)
  let mean_abs_error (orig : Jpeg.image) (decoded : Jpeg.image) =
    let total_err = ref 0.0 in
    let count = ref 0 in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let (r1, g1, b1) = Jpeg.get_pixel orig x y in
        let (r2, g2, b2) = Jpeg.get_pixel decoded x y in
        let dr = abs (r1 - r2) in
        let dg = abs (g1 - g2) in
        let db = abs (b1 - b2) in
        total_err := !total_err +. float_of_int (dr + dg + db);
        count := !count + 3
      done
    done;
    !total_err /. float_of_int !count
  in

  let test_config name entropy_coding encoding_mode quality =
    let opts =
      {
        Jpeg.default_encode_options with
        entropy_coding;
        encoding_mode;
        quality;
        subsampling = Jpeg.Sub_444;
      }
    in
    let jpeg_bytes = Jpeg.write_bytes_with_options opts original in
    let decoded = Jpeg.read_bytes jpeg_bytes in
    let max_err = max_pixel_error original decoded in
    let mean_err = mean_abs_error original decoded in
    let file_size = Bytes.length jpeg_bytes in
    Printf.printf "  %-30s | quality=%3d | size=%6d bytes | max_err=%3d | mean_err=%.3f\n%!"
      name quality file_size max_err mean_err
  in

  Printf.printf "=============================================================================\n";
  Printf.printf "DCT Arithmetic Coding Quality Test\n";
  Printf.printf "Image: 64x64 gradient, 4:4:4 subsampling\n";
  Printf.printf "=============================================================================\n\n";

  let configs = [
    ("Huffman Baseline",     Jpeg.Huffman,    Jpeg.Baseline);
    ("Arithmetic Baseline",  Jpeg.Arithmetic, Jpeg.Baseline);
    ("Huffman Progressive",  Jpeg.Huffman,    Jpeg.Progressive);
    ("Arithmetic Progressive", Jpeg.Arithmetic, Jpeg.Progressive);
  ] in

  List.iter (fun quality ->
    Printf.printf "--- Quality = %d ---\n" quality;
    List.iter (fun (name, ec, em) ->
      test_config name ec em quality
    ) configs;
    Printf.printf "\n"
  ) [95; 100];

  Printf.printf "=============================================================================\n";
  Printf.printf "Notes:\n";
  Printf.printf "  - max_err: maximum absolute difference in any single R/G/B channel\n";
  Printf.printf "  - mean_err: mean absolute error across all R/G/B samples\n";
  Printf.printf "  - Huffman and Arithmetic should produce identical pixel errors\n";
  Printf.printf "    (they differ only in entropy coding, not in DCT/quantization)\n";
  Printf.printf "  - At quality=100, errors come only from DCT rounding\n";
  Printf.printf "=============================================================================\n"
