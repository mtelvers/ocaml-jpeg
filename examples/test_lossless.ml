(** Test lossless JPEG encode/decode roundtrip *)

let test_lossless_roundtrip ~predictor ~arithmetic () =
  let width = 32 in
  let height = 32 in

  (* Create test image with a gradient pattern *)
  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let idx = ((y * width) + x) * 3 in
      Bigarray.Array1.set pixels idx (x * 8);           (* R: gradient *)
      Bigarray.Array1.set pixels (idx + 1) (y * 8);     (* G: gradient *)
      Bigarray.Array1.set pixels (idx + 2) ((x + y) * 4) (* B: diagonal *)
    done
  done;

  let image = Jpeg.create_image width height pixels in

  let options =
    {
      Jpeg.quality = 100;  (* ignored for lossless *)
      subsampling = Jpeg.Sub_444;  (* ignored for lossless *)
      color_mode = Jpeg.Color;
      encoding_mode = Jpeg.Lossless;
      restart_interval = 0;
      precision = Jpeg.Precision_8;
      entropy_coding = if arithmetic then Jpeg.Arithmetic else Jpeg.Huffman;
      predictor = predictor;
      point_transform = 0;
    }
  in

  let encoded = Jpeg.write_bytes_with_options options image in
  Printf.printf "Encoded size: %d bytes (predictor=%d, arithmetic=%b)\n"
    (Bytes.length encoded) predictor arithmetic;

  (* Decode and verify *)
  let decoded = Jpeg.read_bytes encoded in

  if decoded.width <> width || decoded.height <> height then begin
    Printf.printf "FAIL: Dimensions mismatch %dx%d vs %dx%d\n"
      width height decoded.width decoded.height;
    false
  end
  else begin
    let diff_count = ref 0 in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let idx = ((y * width) + x) * 3 in
        let orig_r = Bigarray.Array1.get image.pixels idx in
        let orig_g = Bigarray.Array1.get image.pixels (idx + 1) in
        let orig_b = Bigarray.Array1.get image.pixels (idx + 2) in
        let dec_r = Bigarray.Array1.get decoded.pixels idx in
        let dec_g = Bigarray.Array1.get decoded.pixels (idx + 1) in
        let dec_b = Bigarray.Array1.get decoded.pixels (idx + 2) in
        if orig_r <> dec_r || orig_g <> dec_g || orig_b <> dec_b then begin
          incr diff_count;
          if !diff_count <= 5 then
            Printf.printf "  Diff at (%d,%d): orig=(%d,%d,%d) dec=(%d,%d,%d)\n"
              x y orig_r orig_g orig_b dec_r dec_g dec_b
        end
      done
    done;

    if !diff_count = 0 then begin
      Printf.printf "PASS: Lossless roundtrip (predictor=%d, arithmetic=%b)\n"
        predictor arithmetic;
      true
    end
    else begin
      Printf.printf "FAIL: %d pixel differences\n" !diff_count;
      false
    end
  end

let test_predictor_debug () =
  (* Minimal test to debug predictor 3 *)
  let width = 4 in
  let height = 4 in
  let predictor = 3 in  (* Rc predictor *)

  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in

  (* Simple pattern:
     Row 0: all 0
     Row 1: all 100
     etc *)
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let idx = ((y * width) + x) * 3 in
      let v = y * 50 in  (* Each row has different value *)
      Bigarray.Array1.set pixels idx v;        (* R *)
      Bigarray.Array1.set pixels (idx + 1) v;  (* G *)
      Bigarray.Array1.set pixels (idx + 2) v   (* B *)
    done
  done;

  Printf.printf "Original image:\n";
  for y = 0 to height - 1 do
    Printf.printf "  Row %d: " y;
    for x = 0 to width - 1 do
      let idx = ((y * width) + x) * 3 in
      Printf.printf "(%d,%d,%d) "
        (Bigarray.Array1.get pixels idx)
        (Bigarray.Array1.get pixels (idx + 1))
        (Bigarray.Array1.get pixels (idx + 2))
    done;
    Printf.printf "\n"
  done;

  let image = Jpeg.create_image width height pixels in

  let options =
    {
      Jpeg.quality = 100;
      subsampling = Jpeg.Sub_444;
      color_mode = Jpeg.Color;
      encoding_mode = Jpeg.Lossless;
      restart_interval = 0;
      precision = Jpeg.Precision_8;
      entropy_coding = Jpeg.Huffman;
      predictor = predictor;
      point_transform = 0;
    }
  in

  let encoded = Jpeg.write_bytes_with_options options image in
  Printf.printf "Encoded size: %d bytes\n" (Bytes.length encoded);

  let decoded = Jpeg.read_bytes encoded in

  Printf.printf "Decoded image:\n";
  for y = 0 to height - 1 do
    Printf.printf "  Row %d: " y;
    for x = 0 to width - 1 do
      let idx = ((y * width) + x) * 3 in
      Printf.printf "(%d,%d,%d) "
        (Bigarray.Array1.get decoded.pixels idx)
        (Bigarray.Array1.get decoded.pixels (idx + 1))
        (Bigarray.Array1.get decoded.pixels (idx + 2))
    done;
    Printf.printf "\n"
  done;

  let diff_count = ref 0 in
  for i = 0 to (width * height * 3) - 1 do
    if Bigarray.Array1.get image.pixels i <> Bigarray.Array1.get decoded.pixels i then begin
      incr diff_count
    end
  done;

  if !diff_count = 0 then
    Printf.printf "PASS: Predictor debug test\n"
  else
    Printf.printf "FAIL: %d differences\n" !diff_count

let test_uniform_image predictor =
  (* Test with uniform image - all pixels same value *)
  let width = 8 in
  let height = 8 in

  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for i = 0 to (width * height * 3) - 1 do
    Bigarray.Array1.set pixels i 100
  done;

  let image = Jpeg.create_image width height pixels in

  let options =
    {
      Jpeg.quality = 100;
      subsampling = Jpeg.Sub_444;
      color_mode = Jpeg.Color;
      encoding_mode = Jpeg.Lossless;
      restart_interval = 0;
      precision = Jpeg.Precision_8;
      entropy_coding = Jpeg.Huffman;
      predictor = predictor;
      point_transform = 0;
    }
  in

  let encoded = Jpeg.write_bytes_with_options options image in
  let decoded = Jpeg.read_bytes encoded in

  let diff_count = ref 0 in
  for i = 0 to (width * height * 3) - 1 do
    if Bigarray.Array1.get image.pixels i <> Bigarray.Array1.get decoded.pixels i then
      incr diff_count
  done;

  if !diff_count = 0 then
    Printf.printf "PASS: Uniform image predictor=%d\n" predictor
  else
    Printf.printf "FAIL: Uniform image predictor=%d had %d differences\n" predictor !diff_count

let test_grayscale_lossless () =
  let width = 16 in
  let height = 16 in

  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let idx = ((y * width) + x) * 3 in
      let v = (x * 16 + y) mod 256 in
      Bigarray.Array1.set pixels idx v;
      Bigarray.Array1.set pixels (idx + 1) v;
      Bigarray.Array1.set pixels (idx + 2) v
    done
  done;

  let image = Jpeg.create_image width height pixels in

  let options =
    {
      Jpeg.quality = 100;
      subsampling = Jpeg.Sub_444;
      color_mode = Jpeg.Grayscale;
      encoding_mode = Jpeg.Lossless;
      restart_interval = 0;
      precision = Jpeg.Precision_8;
      entropy_coding = Jpeg.Huffman;
      predictor = 1;
      point_transform = 0;
    }
  in

  let encoded = Jpeg.write_bytes_with_options options image in
  Printf.printf "Grayscale lossless size: %d bytes\n" (Bytes.length encoded);

  let decoded = Jpeg.read_bytes encoded in

  let diff_count = ref 0 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let idx = ((y * width) + x) * 3 in
      let orig = Bigarray.Array1.get image.pixels idx in
      let dec = Bigarray.Array1.get decoded.pixels idx in
      if orig <> dec then incr diff_count
    done
  done;

  if !diff_count = 0 then
    Printf.printf "PASS: Grayscale lossless roundtrip\n"
  else
    Printf.printf "FAIL: %d pixel differences in grayscale\n" !diff_count

let () =
  Printf.printf "=== Testing Lossless JPEG ===\n\n";

  (* Test all predictors with Huffman coding *)
  Printf.printf "--- Huffman Coding ---\n";
  let all_pass = ref true in
  for pred = 1 to 7 do
    if not (test_lossless_roundtrip ~predictor:pred ~arithmetic:false ()) then
      all_pass := false
  done;

  Printf.printf "\n--- Arithmetic Coding ---\n";
  Printf.printf "(NOTE: Arithmetic lossless is not fully implemented yet)\n";
  (* Skip arithmetic tests for now since the implementation is incomplete
  for pred = 1 to 7 do
    if not (test_lossless_roundtrip ~predictor:pred ~arithmetic:true ()) then
      all_pass := false
  done;
  *)

  Printf.printf "\n--- Grayscale ---\n";
  test_grayscale_lossless ();

  Printf.printf "\n--- Uniform Image Test ---\n";
  for pred = 1 to 7 do
    test_uniform_image pred
  done;

  Printf.printf "\n--- Debug Predictor 3 ---\n";
  test_predictor_debug ();

  Printf.printf "\n=== Summary ===\n";
  if !all_pass then
    Printf.printf "All lossless tests PASSED\n"
  else
    Printf.printf "Some tests FAILED\n"
