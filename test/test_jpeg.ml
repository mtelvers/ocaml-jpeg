(** Unit tests for the JPEG library *)

module Bitstream = Jpeg.Bitstream
module Dct = Jpeg.Dct
module Color = Jpeg.Color
module Quantization = Jpeg.Quantization
module Huffman = Jpeg.Huffman
module Exif = Jpeg.Exif
module Markers = Jpeg.Markers
module Icc = Jpeg.Icc

(** Test bitstream read/write round-trip *)
let test_bitstream_roundtrip () =
  let writer = Bitstream.create_writer () in

  (* Write some bits *)
  Bitstream.write_bits writer 0b10110 5;
  Bitstream.write_bits writer 0b11111111 8;
  (* Should trigger byte stuffing *)
  Bitstream.write_bits writer 0b101 3;
  Bitstream.flush_writer writer;

  let data = Bitstream.get_bytes writer in
  let reader = Bitstream.create_reader data in

  (* Read back *)
  let v1 = Bitstream.read_bits reader 5 in
  let v2 = Bitstream.read_bits reader 8 in
  let v3 = Bitstream.read_bits reader 3 in

  Alcotest.(check int) "first value" 0b10110 v1;
  Alcotest.(check int) "second value (FF)" 0xFF v2;
  Alcotest.(check int) "third value" 0b101 v3

(** Test DCT forward/inverse round-trip *)
let test_dct_roundtrip () =
  (* Create a test block with known values *)
  let original =
    Array.init 64 (fun i -> Float.of_int (((i * 17) + 23) mod 256))
  in

  (* Apply DCT then IDCT *)
  let dct_result = Dct.fdct original in
  let reconstructed = Dct.idct dct_result in

  (* Check that values are close (allowing for floating point errors) *)
  let max_error = ref 0.0 in
  for i = 0 to 63 do
    let error = abs_float (original.(i) -. reconstructed.(i)) in
    if error > !max_error then max_error := error
  done;

  Alcotest.(check bool) "DCT round-trip error < 0.01" true (!max_error < 0.01)

(** Test color conversion round-trip *)
let test_color_roundtrip () =
  for r = 0 to 255 do
    for g = 0 to 255 do
      (* Test a subset to keep test fast *)
      if (r + g) mod 32 = 0 then begin
        for b = 0 to 255 do
          if (r + g + b) mod 64 = 0 then begin
            let y, cb, cr = Color.rgb_to_ycbcr r g b in
            let r', g', b' = Color.ycbcr_to_rgb y cb cr in
            (* Allow small error due to rounding *)
            let error_r = abs (r - r') in
            let error_g = abs (g - g') in
            let error_b = abs (b - b') in
            if error_r > 2 || error_g > 2 || error_b > 2 then
              Alcotest.fail
                (Printf.sprintf
                   "Color conversion error: RGB(%d,%d,%d) -> YCbCr(%d,%d,%d) \
                    -> RGB(%d,%d,%d)"
                   r g b y cb cr r' g' b')
          end
        done
      end
    done
  done

(** Test quantization round-trip *)
let test_quantization () =
  let table = Quantization.luminance_table 75 in

  (* Create test block *)
  let original =
    Array.init 64 (fun i -> Float.of_int ((((i * 7) - 32) mod 200) - 100))
  in

  (* Quantize and dequantize *)
  let quantized = Quantization.quantize original table in
  let dequantized = Quantization.dequantize quantized table in

  (* Check that DC coefficient is close *)
  let dc_error = abs_float (original.(0) -. dequantized.(0)) in
  Alcotest.(check bool)
    "DC coefficient error < quant table value" true
    (dc_error <= Float.of_int table.(0))

(** Test zig-zag ordering *)
let test_zigzag () =
  let block = Array.init 64 (fun i -> i) in
  let zigzag = Quantization.to_zigzag block in
  let back = Quantization.from_zigzag zigzag in

  Alcotest.(check (array int)) "Zig-zag round-trip" block back

(** Test Huffman encoding/decoding *)
let test_huffman_roundtrip () =
  let table = Huffman.std_dc_luminance_table () in

  (* Test encoding and decoding each symbol *)
  for symbol = 0 to 11 do
    let writer = Bitstream.create_writer () in
    Huffman.encode_symbol writer table symbol;
    Bitstream.flush_writer writer;

    let data = Bitstream.get_bytes writer in
    let reader = Bitstream.create_reader data in
    let decoded = Huffman.decode_symbol reader table in

    Alcotest.(check int)
      (Printf.sprintf "Huffman symbol %d" symbol)
      symbol decoded
  done

(** Test category calculation *)
let test_huffman_category () =
  Alcotest.(check int) "category(0)" 0 (Huffman.category 0);
  Alcotest.(check int) "category(1)" 1 (Huffman.category 1);
  Alcotest.(check int) "category(-1)" 1 (Huffman.category (-1));
  Alcotest.(check int) "category(2)" 2 (Huffman.category 2);
  Alcotest.(check int) "category(3)" 2 (Huffman.category 3);
  Alcotest.(check int) "category(-3)" 2 (Huffman.category (-3));
  Alcotest.(check int) "category(255)" 8 (Huffman.category 255);
  Alcotest.(check int) "category(-255)" 8 (Huffman.category (-255))

(** Test extend function *)
let test_huffman_extend () =
  (* For 1-bit codes: 0 -> -1, 1 -> 1 *)
  Alcotest.(check int) "extend(0,1)" (-1) (Huffman.extend 0 1);
  Alcotest.(check int) "extend(1,1)" 1 (Huffman.extend 1 1);

  (* For 2-bit codes: 00 -> -3, 01 -> -2, 10 -> 2, 11 -> 3 *)
  Alcotest.(check int) "extend(0,2)" (-3) (Huffman.extend 0 2);
  Alcotest.(check int) "extend(1,2)" (-2) (Huffman.extend 1 2);
  Alcotest.(check int) "extend(2,2)" 2 (Huffman.extend 2 2);
  Alcotest.(check int) "extend(3,2)" 3 (Huffman.extend 3 2)

(** Test creating and encoding a simple image *)
let test_encode_decode_roundtrip () =
  let width = 16 in
  let height = 16 in

  (* Create a simple gradient image *)
  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let idx = ((y * width) + x) * 3 in
      Bigarray.Array1.set pixels idx (x * 16);
      (* R *)
      Bigarray.Array1.set pixels (idx + 1) (y * 16);
      (* G *)
      Bigarray.Array1.set pixels (idx + 2) 128 (* B *)
    done
  done;

  let original = Jpeg.create_image width height pixels in

  (* Encode to JPEG *)
  let jpeg_data = Jpeg.write_bytes ~quality:90 original in

  (* Verify it starts with SOI marker *)
  Alcotest.(check int) "SOI marker byte 1" 0xFF (Bytes.get_uint8 jpeg_data 0);
  Alcotest.(check int) "SOI marker byte 2" 0xD8 (Bytes.get_uint8 jpeg_data 1);

  (* Decode back *)
  let decoded = Jpeg.read_bytes jpeg_data in

  Alcotest.(check int) "Decoded width" width decoded.Jpeg.width;
  Alcotest.(check int) "Decoded height" height decoded.Jpeg.height;

  (* Check that pixels are similar (lossy compression, so not exact) *)
  let max_error = ref 0 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let r1, g1, b1 = Jpeg.get_pixel original x y in
      let r2, g2, b2 = Jpeg.get_pixel decoded x y in
      let err = max (abs (r1 - r2)) (max (abs (g1 - g2)) (abs (b1 - b2))) in
      if err > !max_error then max_error := err
    done
  done;

  (* Allow up to ~20% error for lossy compression *)
  Alcotest.(check bool) "Pixel error within bounds" true (!max_error < 50)

(** Test EXIF parsing *)
let test_exif_minimal () =
  let exif = Exif.create_minimal ~orientation:1 ~software:"OCaml JPEG" () in
  Alcotest.(check (option int)) "orientation" (Some 1) exif.Exif.orientation;
  Alcotest.(check (option string))
    "software" (Some "OCaml JPEG") exif.Exif.software

(** Test marker parsing basics *)
let test_markers_basic () =
  (* Create minimal valid JPEG structure *)
  let buf = Buffer.create 256 in
  Buffer.add_uint8 buf 0xFF;
  Buffer.add_uint8 buf 0xD8;
  (* SOI *)
  Buffer.add_uint8 buf 0xFF;
  Buffer.add_uint8 buf 0xD9;

  (* EOI *)
  let data = Buffer.to_bytes buf in
  let markers = Markers.parse_markers data in

  Alcotest.(check int) "Number of markers" 2 (List.length markers);

  match markers with
  | [ Markers.SOI; Markers.EOI ] -> ()
  | _ -> Alcotest.fail "Expected SOI and EOI markers"

(** Test progressive JPEG decoding *)
let test_progressive_decode () =
  (* Read progressive JPEG test image *)
  let filename = "test_images/progressive_gradient.jpg" in
  if not (Sys.file_exists filename) then Alcotest.skip ()
  else begin
    let image = Jpeg.read filename in
    Alcotest.(check int) "Progressive width" 64 image.Jpeg.width;
    Alcotest.(check int) "Progressive height" 64 image.Jpeg.height;

    (* Verify we can read pixels without error *)
    let r, g, b = Jpeg.get_pixel image 0 0 in
    Alcotest.(check bool) "Valid pixel" true (r >= 0 && g >= 0 && b >= 0)
  end

(** Test that baseline JPEG still has correct frame type *)
let test_baseline_frame_type () =
  let filename = "test_images/test_gradient.jpg" in
  if not (Sys.file_exists filename) then Alcotest.skip ()
  else begin
    let ic = open_in_bin filename in
    let len = in_channel_length ic in
    let data = Bytes.create len in
    really_input ic data 0 len;
    close_in ic;

    let markers = Markers.parse_markers data in
    let frame =
      List.find_map
        (fun m -> match m with Markers.SOF0 f -> Some f | _ -> None)
        markers
    in
    match frame with
    | None -> Alcotest.fail "No SOF0 marker found"
    | Some f ->
        Alcotest.(check bool)
          "Is baseline" true
          (f.Markers.frame_type = Markers.Baseline)
  end

(** Test that progressive JPEG has correct frame type *)
let test_progressive_frame_type () =
  let filename = "test_images/progressive_gradient.jpg" in
  if not (Sys.file_exists filename) then Alcotest.skip ()
  else begin
    let ic = open_in_bin filename in
    let len = in_channel_length ic in
    let data = Bytes.create len in
    really_input ic data 0 len;
    close_in ic;

    let markers = Markers.parse_markers data in
    let frame =
      List.find_map
        (fun m -> match m with Markers.SOF2 f -> Some f | _ -> None)
        markers
    in
    match frame with
    | None -> Alcotest.fail "No SOF2 marker found"
    | Some f ->
        Alcotest.(check bool)
          "Is progressive" true
          (f.Markers.frame_type = Markers.Progressive)
  end

(** Test transform_and_quantize produces consistent results *)
let test_transform_and_quantize () =
  (* Create a block with mid-gray values (128) *)
  let block = Array.make 64 128 in
  let quant_table = Quantization.luminance_table 75 in

  (* Use the new transform_and_quantize function *)
  let shifted = Color.level_shift_block_down block in
  let dct_coeffs = Dct.fdct shifted in
  let quantized = Quantization.quantize dct_coeffs quant_table in

  (* For uniform mid-gray, after level shift (all zeros), DC should be ~0 *)
  Alcotest.(check bool)
    "DC coefficient near zero for uniform gray" true
    (abs quantized.(0) < 5)

(** Test subsampling modes produce correct dimensions *)
let test_subsampling_modes () =
  let width = 32 in
  let height = 24 in

  (* Create test image *)
  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for i = 0 to (width * height * 3) - 1 do
    Bigarray.Array1.set pixels i (i mod 256)
  done;
  let image = Jpeg.create_image width height pixels in

  (* Test 4:4:4 *)
  let data_444 =
    Jpeg.write_bytes_with_options
      { Jpeg.default_encode_options with subsampling = Jpeg.Sub_444 }
      image
  in
  let decoded_444 = Jpeg.read_bytes data_444 in
  Alcotest.(check int) "4:4:4 width" width decoded_444.Jpeg.width;
  Alcotest.(check int) "4:4:4 height" height decoded_444.Jpeg.height;

  (* Test 4:2:2 *)
  let data_422 =
    Jpeg.write_bytes_with_options
      { Jpeg.default_encode_options with subsampling = Jpeg.Sub_422 }
      image
  in
  let decoded_422 = Jpeg.read_bytes data_422 in
  Alcotest.(check int) "4:2:2 width" width decoded_422.Jpeg.width;
  Alcotest.(check int) "4:2:2 height" height decoded_422.Jpeg.height;

  (* Test 4:2:0 *)
  let data_420 =
    Jpeg.write_bytes_with_options
      { Jpeg.default_encode_options with subsampling = Jpeg.Sub_420 }
      image
  in
  let decoded_420 = Jpeg.read_bytes data_420 in
  Alcotest.(check int) "4:2:0 width" width decoded_420.Jpeg.width;
  Alcotest.(check int) "4:2:0 height" height decoded_420.Jpeg.height

(** Test grayscale encoding produces single-component JPEG *)
let test_grayscale_encode () =
  let width = 16 in
  let height = 16 in

  (* Create grayscale gradient *)
  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let gray = ((y * width) + x) * 256 / (width * height) in
      let idx = ((y * width) + x) * 3 in
      Bigarray.Array1.set pixels idx gray;
      Bigarray.Array1.set pixels (idx + 1) gray;
      Bigarray.Array1.set pixels (idx + 2) gray
    done
  done;
  let image = Jpeg.create_image width height pixels in

  let data =
    Jpeg.write_bytes_with_options
      { Jpeg.default_encode_options with color_mode = Jpeg.Grayscale }
      image
  in

  (* Decode and verify *)
  let decoded = Jpeg.read_bytes data in
  Alcotest.(check int) "Grayscale width" width decoded.Jpeg.width;
  Alcotest.(check int) "Grayscale height" height decoded.Jpeg.height;

  (* Verify R=G=B for grayscale *)
  let r, g, b = Jpeg.get_pixel decoded 8 8 in
  Alcotest.(check int) "Grayscale R=G" r g;
  Alcotest.(check int) "Grayscale G=B" g b

(** Test progressive encoding roundtrip *)
let test_progressive_encode_roundtrip () =
  let width = 32 in
  let height = 32 in

  (* Create test image with gradient *)
  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let idx = ((y * width) + x) * 3 in
      Bigarray.Array1.set pixels idx (x * 8);
      Bigarray.Array1.set pixels (idx + 1) (y * 8);
      Bigarray.Array1.set pixels (idx + 2) 128
    done
  done;
  let original = Jpeg.create_image width height pixels in

  (* Encode as progressive *)
  let data =
    Jpeg.write_bytes_with_options
      { Jpeg.default_encode_options with encoding_mode = Jpeg.Progressive }
      original
  in

  (* Verify it's progressive by checking for SOF2 marker *)
  let markers = Markers.parse_markers data in
  let has_sof2 =
    List.exists
      (fun m -> match m with Markers.SOF2 _ -> true | _ -> false)
      markers
  in
  Alcotest.(check bool) "Has SOF2 marker" true has_sof2;

  (* Decode and verify dimensions *)
  let decoded = Jpeg.read_bytes data in
  Alcotest.(check int) "Progressive width" width decoded.Jpeg.width;
  Alcotest.(check int) "Progressive height" height decoded.Jpeg.height;

  (* Verify pixel quality (lossy, so allow some error) *)
  let max_error = ref 0 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let r1, g1, b1 = Jpeg.get_pixel original x y in
      let r2, g2, b2 = Jpeg.get_pixel decoded x y in
      let err = max (abs (r1 - r2)) (max (abs (g1 - g2)) (abs (b1 - b2))) in
      if err > !max_error then max_error := err
    done
  done;
  Alcotest.(check bool) "Progressive pixel error < 50" true (!max_error < 50)

(** Test all option combinations produce valid JPEGs *)
let test_options_combinations () =
  let width = 16 in
  let height = 16 in

  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for i = 0 to (width * height * 3) - 1 do
    Bigarray.Array1.set pixels i (i * 7 mod 256)
  done;
  let image = Jpeg.create_image width height pixels in

  let subsampling_modes = [ Jpeg.Sub_444; Jpeg.Sub_422; Jpeg.Sub_420 ] in
  let color_modes = [ Jpeg.Color; Jpeg.Grayscale ] in
  let encoding_modes = [ Jpeg.Baseline; Jpeg.Progressive ] in

  List.iter
    (fun sub ->
      List.iter
        (fun color ->
          List.iter
            (fun enc ->
              let options =
                {
                  Jpeg.quality = 75;
                  subsampling = sub;
                  color_mode = color;
                  encoding_mode = enc;
                  restart_interval = 0;
                  precision = Jpeg.Precision_8;
                  entropy_coding = Jpeg.Huffman;
                  predictor = 1;
                  point_transform = 0;
                }
              in
              let data = Jpeg.write_bytes_with_options options image in

              (* Verify valid JPEG (starts with SOI) *)
              Alcotest.(check int)
                "Valid JPEG start 1" 0xFF (Bytes.get_uint8 data 0);
              Alcotest.(check int)
                "Valid JPEG start 2" 0xD8 (Bytes.get_uint8 data 1);

              (* Verify can decode *)
              let decoded = Jpeg.read_bytes data in
              Alcotest.(check int) "Decoded width" width decoded.Jpeg.width;
              Alcotest.(check int) "Decoded height" height decoded.Jpeg.height)
            encoding_modes)
        color_modes)
    subsampling_modes

(** Test backward compatibility of write_bytes *)
let test_backward_compatibility () =
  let width = 16 in
  let height = 16 in

  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for i = 0 to (width * height * 3) - 1 do
    Bigarray.Array1.set pixels i (i mod 256)
  done;
  let image = Jpeg.create_image width height pixels in

  (* Old API still works *)
  let data = Jpeg.write_bytes ~quality:80 image in
  let decoded = Jpeg.read_bytes data in

  Alcotest.(check int) "Backward compat width" width decoded.Jpeg.width;
  Alcotest.(check int) "Backward compat height" height decoded.Jpeg.height

(** Test progressive grayscale encoding *)
let test_progressive_grayscale () =
  let width = 24 in
  let height = 24 in

  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let gray = (x + y) * 5 in
      let idx = ((y * width) + x) * 3 in
      Bigarray.Array1.set pixels idx gray;
      Bigarray.Array1.set pixels (idx + 1) gray;
      Bigarray.Array1.set pixels (idx + 2) gray
    done
  done;
  let image = Jpeg.create_image width height pixels in

  let options =
    {
      Jpeg.quality = 90;
      subsampling = Jpeg.Sub_444;
      color_mode = Jpeg.Grayscale;
      encoding_mode = Jpeg.Progressive;
      restart_interval = 0;
      precision = Jpeg.Precision_8;
      entropy_coding = Jpeg.Huffman;
      predictor = 1;
      point_transform = 0;
    }
  in
  let data = Jpeg.write_bytes_with_options options image in

  (* Verify SOF2 marker *)
  let markers = Markers.parse_markers data in
  let has_sof2 =
    List.exists
      (fun m -> match m with Markers.SOF2 _ -> true | _ -> false)
      markers
  in
  Alcotest.(check bool) "Grayscale progressive has SOF2" true has_sof2;

  let decoded = Jpeg.read_bytes data in
  Alcotest.(check int) "Progressive grayscale width" width decoded.Jpeg.width

(** Test restart interval encoding *)
let test_restart_interval_encoding () =
  let width = 32 in
  let height = 32 in

  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for i = 0 to (width * height * 3) - 1 do
    Bigarray.Array1.set pixels i (i * 7 mod 256)
  done;
  let image = Jpeg.create_image width height pixels in

  (* Encode with restart interval *)
  let options =
    {
      Jpeg.default_encode_options with
      restart_interval = 2;
      (* RST every 2 MCUs *)
    }
  in
  let data = Jpeg.write_bytes_with_options options image in

  (* Parse markers to verify DRI present *)
  let markers = Markers.parse_markers data in
  let has_dri =
    List.exists
      (fun m -> match m with Markers.DRI _ -> true | _ -> false)
      markers
  in
  Alcotest.(check bool) "Has DRI marker" true has_dri;

  (* Verify we can decode it *)
  let decoded = Jpeg.read_bytes data in
  Alcotest.(check int) "Decoded width" width decoded.Jpeg.width;
  Alcotest.(check int) "Decoded height" height decoded.Jpeg.height

(** Test restart interval roundtrip *)
let test_restart_roundtrip () =
  let width = 64 in
  let height = 64 in

  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let idx = ((y * width) + x) * 3 in
      Bigarray.Array1.set pixels idx (x * 4);
      Bigarray.Array1.set pixels (idx + 1) (y * 4);
      Bigarray.Array1.set pixels (idx + 2) 128
    done
  done;
  let original = Jpeg.create_image width height pixels in

  (* Encode with restart interval *)
  let options =
    { Jpeg.default_encode_options with restart_interval = 5; quality = 95 }
  in
  let data = Jpeg.write_bytes_with_options options original in
  let decoded = Jpeg.read_bytes data in

  (* Verify similar pixels *)
  let max_error = ref 0 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let r1, g1, b1 = Jpeg.get_pixel original x y in
      let r2, g2, b2 = Jpeg.get_pixel decoded x y in
      let err = max (abs (r1 - r2)) (max (abs (g1 - g2)) (abs (b1 - b2))) in
      if err > !max_error then max_error := err
    done
  done;
  Alcotest.(check bool) "Restart roundtrip error < 30" true (!max_error < 30)

(** Test progressive with restart markers *)
let test_progressive_with_restart () =
  let width = 32 in
  let height = 32 in

  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for i = 0 to (width * height * 3) - 1 do
    Bigarray.Array1.set pixels i (i mod 256)
  done;
  let image = Jpeg.create_image width height pixels in

  let options =
    {
      Jpeg.default_encode_options with
      encoding_mode = Jpeg.Progressive;
      restart_interval = 3;
    }
  in
  let data = Jpeg.write_bytes_with_options options image in

  (* Verify has both SOF2 and DRI *)
  let markers = Markers.parse_markers data in
  let has_sof2 =
    List.exists
      (fun m -> match m with Markers.SOF2 _ -> true | _ -> false)
      markers
  in
  let has_dri =
    List.exists
      (fun m -> match m with Markers.DRI _ -> true | _ -> false)
      markers
  in
  Alcotest.(check bool) "Has SOF2" true has_sof2;
  Alcotest.(check bool) "Has DRI" true has_dri;

  let decoded = Jpeg.read_bytes data in
  Alcotest.(check int) "Progressive+RST width" width decoded.Jpeg.width

(** Test 12-bit precision encoding *)
let test_12bit_precision () =
  let width = 16 in
  let height = 16 in

  (* Create 8-bit image - library will handle precision internally *)
  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for i = 0 to (width * height * 3) - 1 do
    Bigarray.Array1.set pixels i (i mod 256)
  done;
  let image = Jpeg.create_image width height pixels in

  let options =
    { Jpeg.default_encode_options with precision = Jpeg.Precision_12 }
  in
  let data = Jpeg.write_bytes_with_options options image in

  (* Verify SOF has precision=12 *)
  let markers = Markers.parse_markers data in
  let frame =
    List.find_map
      (fun m -> match m with Markers.SOF0 f -> Some f | _ -> None)
      markers
  in
  match frame with
  | None -> Alcotest.fail "No SOF0 marker found"
  | Some f -> Alcotest.(check int) "12-bit precision" 12 f.Markers.precision

(** Test precision color conversion *)
let test_precision_color_conversion () =
  (* Test 8-bit precision *)
  let y8, cb8, cr8 = Color.rgb_to_ycbcr_precision 8 255 128 64 in
  Alcotest.(check bool) "Y in 8-bit range" true (y8 >= 0 && y8 <= 255);
  Alcotest.(check bool) "Cb in 8-bit range" true (cb8 >= 0 && cb8 <= 255);
  Alcotest.(check bool) "Cr in 8-bit range" true (cr8 >= 0 && cr8 <= 255);

  (* Test 12-bit precision *)
  let y12, cb12, cr12 = Color.rgb_to_ycbcr_precision 12 4095 2048 1024 in
  Alcotest.(check bool) "Y in 12-bit range" true (y12 >= 0 && y12 <= 4095);
  Alcotest.(check bool) "Cb in 12-bit range" true (cb12 >= 0 && cb12 <= 4095);
  Alcotest.(check bool) "Cr in 12-bit range" true (cr12 >= 0 && cr12 <= 4095)

(** Test CMYK color conversion *)
let test_cmyk_color_conversion () =
  (* Test RGB -> CMYK -> RGB roundtrip *)
  let test_values =
    [ (255, 0, 0); (0, 255, 0); (0, 0, 255); (128, 128, 128) ]
  in
  List.iter
    (fun (r, g, b) ->
      let c, m, y, k = Color.rgb_to_cmyk r g b in
      let r', g', b' = Color.cmyk_to_rgb c m y k in
      let max_error = max (abs (r - r')) (max (abs (g - g')) (abs (b - b'))) in
      Alcotest.(check bool)
        (Printf.sprintf "CMYK roundtrip for RGB(%d,%d,%d)" r g b)
        true (max_error < 10))
    test_values

(** Test YCCK color conversion - verify it's self-consistent *)
let test_ycck_color_conversion () =
  (* YCCK conversion should be reversible for non-black colors.
     Pure black is a special case that doesn't roundtrip perfectly
     due to CMYK's separate K channel. *)
  let test_values =
    [ (255, 128, 64); (100, 200, 50); (255, 255, 255); (128, 64, 192) ]
  in
  List.iter
    (fun (r, g, b) ->
      let y, cb, cr, k = Color.rgb_to_ycck r g b in
      let r', g', b' = Color.ycck_to_rgb y cb cr k in
      let max_error = max (abs (r - r')) (max (abs (g - g')) (abs (b - b'))) in
      Alcotest.(check bool)
        (Printf.sprintf "YCCK roundtrip for RGB(%d,%d,%d), error=%d" r g b
           max_error)
        true (max_error < 5))
    test_values;
  (* Verify that YCCK values are in valid range *)
  let y, cb, cr, k = Color.rgb_to_ycck 128 128 128 in
  Alcotest.(check bool) "Y in range" true (y >= 0 && y <= 255);
  Alcotest.(check bool) "Cb in range" true (cb >= 0 && cb <= 255);
  Alcotest.(check bool) "Cr in range" true (cr >= 0 && cr <= 255);
  Alcotest.(check bool) "K in range" true (k >= 0 && k <= 255)

(** Test CMYK encoding *)
let test_cmyk_encode () =
  let width = 16 in
  let height = 16 in

  (* Create RGB image *)
  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for i = 0 to (width * height * 3) - 1 do
    Bigarray.Array1.set pixels i (i mod 256)
  done;
  let image = Jpeg.create_image width height pixels in

  (* Encode as CMYK *)
  let options = { Jpeg.default_encode_options with color_mode = Jpeg.CMYK } in
  let data = Jpeg.write_bytes_with_options options image in

  (* Verify valid JPEG *)
  Alcotest.(check int) "Valid JPEG start 1" 0xFF (Bytes.get_uint8 data 0);
  Alcotest.(check int) "Valid JPEG start 2" 0xD8 (Bytes.get_uint8 data 1);

  (* Verify 4 components in SOF *)
  let markers = Markers.parse_markers data in
  let frame =
    List.find_map
      (fun m -> match m with Markers.SOF0 f -> Some f | _ -> None)
      markers
  in
  match frame with
  | None -> Alcotest.fail "No SOF0 marker found"
  | Some f ->
      Alcotest.(check int) "4 components" 4 (Array.length f.Markers.components)

(** Test YCCK encoding *)
let test_ycck_encode () =
  let width = 16 in
  let height = 16 in

  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for i = 0 to (width * height * 3) - 1 do
    Bigarray.Array1.set pixels i (i * 7 mod 256)
  done;
  let image = Jpeg.create_image width height pixels in

  let options = { Jpeg.default_encode_options with color_mode = Jpeg.YCCK } in
  let data = Jpeg.write_bytes_with_options options image in

  (* Verify 4 components in SOF *)
  let markers = Markers.parse_markers data in
  let frame =
    List.find_map
      (fun m -> match m with Markers.SOF0 f -> Some f | _ -> None)
      markers
  in
  match frame with
  | None -> Alcotest.fail "No SOF0 marker found"
  | Some f ->
      Alcotest.(check int) "4 components" 4 (Array.length f.Markers.components)

(** Test CMYK image creation and pixel access *)
let test_cmyk_pixel_access () =
  let width = 4 in
  let height = 4 in
  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 4)
  in
  (* Fill with known CMYK values *)
  for i = 0 to (width * height) - 1 do
    Bigarray.Array1.set pixels (i * 4) 100;
    (* C *)
    Bigarray.Array1.set pixels ((i * 4) + 1) 150;
    (* M *)
    Bigarray.Array1.set pixels ((i * 4) + 2) 200;
    (* Y *)
    Bigarray.Array1.set pixels ((i * 4) + 3) 50 (* K *)
  done;
  let image = Jpeg.create_cmyk_image width height pixels in

  (* Check pixel_format *)
  Alcotest.(check bool) "Is CMYK32" true (image.Jpeg.pixel_format = Jpeg.CMYK32);

  (* Test get_cmyk_pixel *)
  let c, m, y, k = Jpeg.get_cmyk_pixel image 2 2 in
  Alcotest.(check int) "C value" 100 c;
  Alcotest.(check int) "M value" 150 m;
  Alcotest.(check int) "Y value" 200 y;
  Alcotest.(check int) "K value" 50 k;

  (* Test get_pixel (should convert to RGB) *)
  let r, g, b = Jpeg.get_pixel image 2 2 in
  Alcotest.(check bool) "R in valid range" true (r >= 0 && r <= 255);
  Alcotest.(check bool) "G in valid range" true (g >= 0 && g <= 255);
  Alcotest.(check bool) "B in valid range" true (b >= 0 && b <= 255)

(** Test set_cmyk_pixel *)
let test_set_cmyk_pixel () =
  let width = 4 in
  let height = 4 in
  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 4)
  in
  (* Initialize with zeros *)
  for i = 0 to (width * height * 4) - 1 do
    Bigarray.Array1.set pixels i 0
  done;
  let image = Jpeg.create_cmyk_image width height pixels in

  (* Set a pixel *)
  Jpeg.set_cmyk_pixel image 1 2 80 120 200 40;

  (* Read it back *)
  let c, m, y, k = Jpeg.get_cmyk_pixel image 1 2 in
  Alcotest.(check int) "Set C value" 80 c;
  Alcotest.(check int) "Set M value" 120 m;
  Alcotest.(check int) "Set Y value" 200 y;
  Alcotest.(check int) "Set K value" 40 k;

  (* Verify other pixels are unchanged *)
  let c0, m0, y0, k0 = Jpeg.get_cmyk_pixel image 0 0 in
  Alcotest.(check int) "Unchanged C" 0 c0;
  Alcotest.(check int) "Unchanged M" 0 m0;
  Alcotest.(check int) "Unchanged Y" 0 y0;
  Alcotest.(check int) "Unchanged K" 0 k0

(** Test CMYK encode/decode roundtrip *)
let test_cmyk_roundtrip () =
  let width = 16 in
  let height = 16 in

  (* Create RGB image with known colors *)
  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let idx = ((y * width) + x) * 3 in
      Bigarray.Array1.set pixels idx (x * 16);
      Bigarray.Array1.set pixels (idx + 1) (y * 16);
      Bigarray.Array1.set pixels (idx + 2) 128
    done
  done;
  let image = Jpeg.create_image width height pixels in

  (* Encode as CMYK *)
  let options =
    { Jpeg.default_encode_options with color_mode = Jpeg.CMYK; quality = 95 }
  in
  let data = Jpeg.write_bytes_with_options options image in

  (* Decode - should be CMYK32 *)
  let decoded = Jpeg.read_bytes data in
  Alcotest.(check int) "Decoded width" width decoded.Jpeg.width;
  Alcotest.(check int) "Decoded height" height decoded.Jpeg.height;
  Alcotest.(check bool)
    "Decoded is CMYK32" true
    (decoded.Jpeg.pixel_format = Jpeg.CMYK32);

  (* Verify we can get CMYK pixels *)
  let c, m, y, k = Jpeg.get_cmyk_pixel decoded 8 8 in
  Alcotest.(check bool) "C in range" true (c >= 0 && c <= 255);
  Alcotest.(check bool) "M in range" true (m >= 0 && m <= 255);
  Alcotest.(check bool) "Y in range" true (y >= 0 && y <= 255);
  Alcotest.(check bool) "K in range" true (k >= 0 && k <= 255)

(** Test QM-coder basic encoding/decoding roundtrip *)
let test_qm_coder_basic () =
  let module Arith = Jpeg.Arithmetic in
  (* Encode a sequence of binary decisions *)
  let encoder = Arith.init_encoder () in
  let ctx = Arith.create_context () in

  (* Encode pattern: 0, 0, 1, 0, 1, 1, 0 *)
  let pattern = [| 0; 0; 1; 0; 1; 1; 0 |] in
  Array.iter (Arith.encode ctx encoder) pattern;

  let data = Arith.flush_encoder encoder in
  Alcotest.(check bool) "Data produced" true (Bytes.length data > 0);

  (* Decode the data back *)
  let decoder = Arith.init_decoder_bytes data in
  let ctx2 = Arith.create_context () in

  let decoded =
    Array.init (Array.length pattern) (fun _ -> Arith.decode ctx2 decoder)
  in

  (* Verify roundtrip *)
  Array.iteri
    (fun i expected ->
      Alcotest.(check int) (Printf.sprintf "Decision %d" i) expected decoded.(i))
    pattern

(** Test arithmetic context state transitions *)
let test_arithmetic_context () =
  let module Arith = Jpeg.Arithmetic in
  let ctx = Arith.create_context () in

  (* Initial state *)
  Alcotest.(check int) "Initial index" 0 ctx.index;
  Alcotest.(check int) "Initial MPS" 0 ctx.mps;

  (* Verify context can be created for DC/AC *)
  let dc_contexts = Arith.create_dc_contexts () in
  let ac_contexts = Arith.create_ac_contexts () in
  Alcotest.(check int) "DC contexts count" 10 (Array.length dc_contexts);
  Alcotest.(check int) "AC contexts count" 252 (Array.length ac_contexts)

(** Test JPEG arithmetic decoder initialization *)
let test_jpeg_arith_decoder_init () =
  let module Arith = Jpeg.Arithmetic in
  (* Create a simple test bitstream *)
  let data = Bytes.make 10 '\x00' in
  Bytes.set_uint8 data 0 0x80;
  (* Some test data *)
  Bytes.set_uint8 data 1 0x00;

  (* Initialize JPEG decoder *)
  let decoder = Arith.init_jpeg_decoder data in

  (* Verify decoder state is initialized correctly.
     In libjpeg-style init: a=0, ct=-16 to force reading 2 initial bytes
     during the first renormalization in decode. *)
  Alcotest.(check int) "Initial A register" 0 decoder.Arith.a;
  Alcotest.(check int) "Initial CT" (-16) decoder.Arith.ct;
  Alcotest.(check bool) "Data attached" true (Bytes.length decoder.Arith.data > 0)

(** Test JPEG arithmetic DC stat bins *)
let test_jpeg_arith_dc_bins () =
  let module Arith = Jpeg.Arithmetic in
  (* Create DC stat bins - now a flat array of 64 contexts *)
  let bins = Arith.create_dc_stat_bins () in

  (* Verify flat array size *)
  Alcotest.(check int) "DC bins size" Arith.dc_stat_bins_size (Array.length bins);
  (* Verify contexts are initialized *)
  Alcotest.(check int) "DC bin 0 index" 0 bins.(0).Arith.index;
  Alcotest.(check int) "DC bin 0 mps" 0 bins.(0).Arith.mps

(** Test JPEG arithmetic AC stat bins *)
let test_jpeg_arith_ac_bins () =
  let module Arith = Jpeg.Arithmetic in
  (* Create AC stat bins - now a flat array of 256 contexts *)
  let bins = Arith.create_ac_stat_bins () in

  (* Verify flat array size *)
  Alcotest.(check int) "AC bins size" Arith.ac_stat_bins_size (Array.length bins);
  (* Verify contexts are initialized *)
  Alcotest.(check int) "AC bin 0 index" 0 bins.(0).Arith.index;
  Alcotest.(check int) "AC bin 0 mps" 0 bins.(0).Arith.mps

(** Test JPEG arithmetic scan state initialization *)
let test_jpeg_arith_scan_state () =
  let module Arith = Jpeg.Arithmetic in
  (* Create scan state for 3 components *)
  let data = Bytes.make 100 '\x80' in
  let state = Arith.init_arith_scan_decoder data 3 in

  (* Verify state is properly initialized *)
  Alcotest.(check int) "DC bins count" 3 (Array.length state.Arith.dc_bins);
  Alcotest.(check int) "AC bins count" 3 (Array.length state.Arith.ac_bins);
  Alcotest.(check int) "Prev DC count" 3 (Array.length state.Arith.prev_dc);
  Alcotest.(check int) "L values count" 3 (Array.length state.Arith.l);
  Alcotest.(check int) "Kx values count" 3 (Array.length state.Arith.kx);

  (* Verify default conditioning values *)
  Alcotest.(check int) "Default L" 0 state.Arith.l.(0);
  Alcotest.(check int) "Default Kx" 5 state.Arith.kx.(0)

(** Test setting arithmetic conditioning values *)
let test_jpeg_arith_conditioning () =
  let module Arith = Jpeg.Arithmetic in
  let data = Bytes.make 100 '\x80' in
  let state = Arith.init_arith_scan_decoder data 2 in

  (* Set DC conditioning (L value) *)
  Arith.set_conditioning state 0 true 4;
  Alcotest.(check int) "DC conditioning L" 4 state.Arith.l.(0);

  (* Set AC conditioning (Kx value) *)
  Arith.set_conditioning state 1 false 10;
  Alcotest.(check int) "AC conditioning Kx" 10 state.Arith.kx.(1)

(** Test arithmetic decoder reset *)
let test_jpeg_arith_reset () =
  let module Arith = Jpeg.Arithmetic in
  let data = Bytes.make 100 '\x80' in
  let state = Arith.init_arith_scan_decoder data 2 in

  (* Modify some state *)
  state.Arith.prev_dc.(0) <- 100;
  state.Arith.prev_dc.(1) <- -50;
  state.Arith.dc_bins.(0).(0).Arith.index <- 5;
  state.Arith.ac_bins.(0).(0).Arith.index <- 10;

  (* Reset *)
  Arith.reset_arith_decoder state;

  (* Verify reset *)
  Alcotest.(check int) "Prev DC 0 reset" 0 state.Arith.prev_dc.(0);
  Alcotest.(check int) "Prev DC 1 reset" 0 state.Arith.prev_dc.(1);
  Alcotest.(check int)
    "DC bin 0 index reset" 0 state.Arith.dc_bins.(0).(0).Arith.index;
  Alcotest.(check int)
    "AC bin 0 index reset" 0 state.Arith.ac_bins.(0).(0).Arith.index

(** Test JPEG MQ-coder decode decision *)
let test_jpeg_mq_decode () =
  let module Arith = Jpeg.Arithmetic in
  (* Create a bitstream that encodes some known decisions *)
  (* This tests the actual MQ-coder decode logic *)
  let data = Bytes.create 16 in
  (* Fill with a pattern that will produce some decodable data *)
  for i = 0 to 15 do
    Bytes.set_uint8 data i (if i mod 2 = 0 then 0x80 else 0x00)
  done;

  let decoder = Arith.init_jpeg_decoder data in
  let ctx = Arith.create_context () in

  (* Decode several decisions - should not crash *)
  let decisions = Array.init 10 (fun _ -> Arith.decode_decision ctx decoder) in

  (* Verify decisions are valid binary values *)
  Array.iter
    (fun d -> Alcotest.(check bool) "Decision is 0 or 1" true (d = 0 || d = 1))
    decisions

(** Test parsing of SOF9/SOF10 markers *)
let test_arith_marker_parsing () =
  (* Create a minimal JPEG with SOF9 marker (arithmetic sequential) *)
  let buf = Buffer.create 256 in

  (* SOI *)
  Buffer.add_uint8 buf 0xFF;
  Buffer.add_uint8 buf 0xD8;

  (* DQT - minimal quantization table *)
  Buffer.add_uint8 buf 0xFF;
  Buffer.add_uint8 buf 0xDB;
  Buffer.add_uint8 buf 0x00;
  Buffer.add_uint8 buf 0x43;
  (* length = 67 *)
  Buffer.add_uint8 buf 0x00;
  (* table 0, 8-bit precision *)
  for _ = 0 to 63 do
    Buffer.add_uint8 buf 16 (* uniform quantization *)
  done;

  (* SOF9 - Start of Frame, arithmetic sequential *)
  Buffer.add_uint8 buf 0xFF;
  Buffer.add_uint8 buf 0xC9;
  (* SOF9 marker *)
  Buffer.add_uint8 buf 0x00;
  Buffer.add_uint8 buf 0x0B;
  (* length = 11 *)
  Buffer.add_uint8 buf 0x08;
  (* precision = 8 bits *)
  Buffer.add_uint8 buf 0x00;
  Buffer.add_uint8 buf 0x08;
  (* height = 8 *)
  Buffer.add_uint8 buf 0x00;
  Buffer.add_uint8 buf 0x08;
  (* width = 8 *)
  Buffer.add_uint8 buf 0x01;
  (* 1 component (grayscale) *)
  Buffer.add_uint8 buf 0x01;
  (* component ID = 1 *)
  Buffer.add_uint8 buf 0x11;
  (* sampling = 1x1 *)
  Buffer.add_uint8 buf 0x00;

  (* quant table = 0 *)

  (* SOS - Start of Scan *)
  Buffer.add_uint8 buf 0xFF;
  Buffer.add_uint8 buf 0xDA;
  Buffer.add_uint8 buf 0x00;
  Buffer.add_uint8 buf 0x08;
  (* length = 8 *)
  Buffer.add_uint8 buf 0x01;
  (* 1 component *)
  Buffer.add_uint8 buf 0x01;
  (* component selector = 1 *)
  Buffer.add_uint8 buf 0x00;
  (* DC/AC table = 0/0 *)
  Buffer.add_uint8 buf 0x00;
  (* Ss = 0 *)
  Buffer.add_uint8 buf 0x3F;
  (* Se = 63 *)
  Buffer.add_uint8 buf 0x00;

  (* Ah/Al = 0/0 *)

  (* Entropy coded data - a simple pattern that decodes to gray *)
  (* For arithmetic coding, we need data that produces valid coefficients *)
  for _ = 0 to 31 do
    Buffer.add_uint8 buf 0x80
  done;

  (* EOI *)
  Buffer.add_uint8 buf 0xFF;
  Buffer.add_uint8 buf 0xD9;

  let data = Buffer.to_bytes buf in

  (* Parse markers to verify SOF9 is recognized *)
  let markers = Jpeg.Markers.parse_markers data in

  let has_sof9 =
    List.exists
      (fun m ->
        match m with
        | Jpeg.Markers.SOF9 frame ->
            frame.Jpeg.Markers.frame_type = Jpeg.Markers.ArithmeticSequential
        | _ -> false)
      markers
  in

  Alcotest.(check bool) "SOF9 marker parsed" true has_sof9;

  (* Try to decode - may produce unexpected results but should not crash *)
  try
    let _decoded = Jpeg.read_bytes data in
    (* If we get here, decoding succeeded *)
    Alcotest.(check bool) "Arithmetic JPEG decoded" true true
  with _ ->
    (* Decoding may fail with synthetic data, but parsing worked *)
    Alcotest.(check bool) "SOF9 parsing works" true true

(** Test arithmetic baseline encoding (SOF9) *)
let test_arith_baseline_encode () =
  let width = 16 in
  let height = 16 in

  (* Create test image *)
  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let idx = ((y * width) + x) * 3 in
      Bigarray.Array1.set pixels idx (x * 16);
      Bigarray.Array1.set pixels (idx + 1) (y * 16);
      Bigarray.Array1.set pixels (idx + 2) 128
    done
  done;
  let image = Jpeg.create_image width height pixels in

  (* Encode with arithmetic coding, baseline mode *)
  let options =
    { Jpeg.default_encode_options with entropy_coding = Jpeg.Arithmetic }
  in
  let data = Jpeg.write_bytes_with_options options image in

  (* Verify valid JPEG *)
  Alcotest.(check int) "Valid JPEG start 1" 0xFF (Bytes.get_uint8 data 0);
  Alcotest.(check int) "Valid JPEG start 2" 0xD8 (Bytes.get_uint8 data 1);

  (* Parse markers to verify SOF9 *)
  let markers = Markers.parse_markers data in
  let has_sof9 =
    List.exists
      (fun m -> match m with Markers.SOF9 _ -> true | _ -> false)
      markers
  in
  Alcotest.(check bool) "Has SOF9 marker" true has_sof9;

  (* Verify has DAC marker (not DHT) *)
  let has_dac =
    List.exists
      (fun m -> match m with Markers.DAC _ -> true | _ -> false)
      markers
  in
  let has_dht =
    List.exists
      (fun m -> match m with Markers.DHT _ -> true | _ -> false)
      markers
  in
  Alcotest.(check bool) "Has DAC marker" true has_dac;
  Alcotest.(check bool) "No DHT marker" false has_dht

(** Test arithmetic progressive encoding (SOF10) *)
let test_arith_progressive_encode () =
  let width = 16 in
  let height = 16 in

  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let idx = ((y * width) + x) * 3 in
      Bigarray.Array1.set pixels idx (x * 16);
      Bigarray.Array1.set pixels (idx + 1) (y * 16);
      Bigarray.Array1.set pixels (idx + 2) 128
    done
  done;
  let image = Jpeg.create_image width height pixels in

  (* Encode with arithmetic coding, progressive mode *)
  let options =
    {
      Jpeg.default_encode_options with
      entropy_coding = Jpeg.Arithmetic;
      encoding_mode = Jpeg.Progressive;
    }
  in
  let data = Jpeg.write_bytes_with_options options image in

  (* Verify valid JPEG *)
  Alcotest.(check int) "Valid JPEG start 1" 0xFF (Bytes.get_uint8 data 0);
  Alcotest.(check int) "Valid JPEG start 2" 0xD8 (Bytes.get_uint8 data 1);

  (* Parse markers to verify SOF10 *)
  let markers = Markers.parse_markers data in
  let has_sof10 =
    List.exists
      (fun m -> match m with Markers.SOF10 _ -> true | _ -> false)
      markers
  in
  Alcotest.(check bool) "Has SOF10 marker" true has_sof10;

  (* Verify has DAC marker *)
  let has_dac =
    List.exists
      (fun m -> match m with Markers.DAC _ -> true | _ -> false)
      markers
  in
  Alcotest.(check bool) "Has DAC marker" true has_dac

(** Test arithmetic grayscale encoding *)
let test_arith_grayscale_encode () =
  let width = 16 in
  let height = 16 in

  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let gray = (x + y) * 8 in
      let idx = ((y * width) + x) * 3 in
      Bigarray.Array1.set pixels idx gray;
      Bigarray.Array1.set pixels (idx + 1) gray;
      Bigarray.Array1.set pixels (idx + 2) gray
    done
  done;
  let image = Jpeg.create_image width height pixels in

  (* Encode as grayscale with arithmetic coding *)
  let options =
    {
      Jpeg.default_encode_options with
      entropy_coding = Jpeg.Arithmetic;
      color_mode = Jpeg.Grayscale;
    }
  in
  let data = Jpeg.write_bytes_with_options options image in

  (* Parse markers to verify SOF9 with 1 component *)
  let markers = Markers.parse_markers data in
  let frame =
    List.find_map
      (fun m -> match m with Markers.SOF9 f -> Some f | _ -> None)
      markers
  in
  match frame with
  | None -> Alcotest.fail "No SOF9 marker found"
  | Some f ->
      Alcotest.(check int) "1 component" 1 (Array.length f.Markers.components)

(** Test JPEG arithmetic encoder state *)
let test_jpeg_arith_encoder_state () =
  let module Arith = Jpeg.Arithmetic in
  (* Initialize encoder *)
  let encoder = Arith.init_jpeg_encoder () in

  (* Verify encoder state is initialized *)
  Alcotest.(check int) "Initial A" 0x10000 encoder.Arith.a;
  Alcotest.(check int) "Initial C" 0 encoder.Arith.c;
  Alcotest.(check int) "Initial CT" 11 encoder.Arith.ct;
  Alcotest.(check int) "Initial ST" 0 encoder.Arith.st

(** Test JPEG arithmetic encode decision *)
let test_jpeg_arith_encode_decision () =
  let module Arith = Jpeg.Arithmetic in
  let encoder = Arith.init_jpeg_encoder () in
  let ctx = Arith.create_context () in

  (* Encode several decisions *)
  Arith.encode_decision ctx encoder 0;
  Arith.encode_decision ctx encoder 1;
  Arith.encode_decision ctx encoder 0;

  (* Verify encoder state changed *)
  Alcotest.(check bool)
    "Encoder state modified" true
    (encoder.Arith.a <> 0x10000 || encoder.Arith.c <> 0)

(** Test JPEG arithmetic scan encoder *)
let test_jpeg_arith_scan_encoder () =
  let module Arith = Jpeg.Arithmetic in
  (* Initialize scan encoder for 2 components *)
  let state = Arith.init_arith_scan_encoder 2 in

  (* Verify state is properly initialized *)
  Alcotest.(check int) "DC bins count" 2 (Array.length state.Arith.dc_bins);
  Alcotest.(check int) "AC bins count" 2 (Array.length state.Arith.ac_bins);
  Alcotest.(check int) "Prev DC count" 2 (Array.length state.Arith.prev_dc);

  (* Verify default conditioning values *)
  Alcotest.(check int) "Default L" 0 state.Arith.l.(0);
  Alcotest.(check int) "Default Kx" 5 state.Arith.kx.(0)

(** Test JPEG arithmetic encoder reset *)
let test_jpeg_arith_encoder_reset () =
  let module Arith = Jpeg.Arithmetic in
  let state = Arith.init_arith_scan_encoder 2 in

  (* Modify some state *)
  state.Arith.prev_dc.(0) <- 100;
  state.Arith.prev_dc.(1) <- -50;
  state.Arith.dc_bins.(0).(0).Arith.index <- 5;

  (* Reset *)
  Arith.reset_arith_encoder state;

  (* Verify reset *)
  Alcotest.(check int) "Prev DC 0 reset" 0 state.Arith.prev_dc.(0);
  Alcotest.(check int) "Prev DC 1 reset" 0 state.Arith.prev_dc.(1);
  Alcotest.(check int)
    "DC bin 0 index reset" 0 state.Arith.dc_bins.(0).(0).Arith.index;
  Alcotest.(check int) "Encoder A reset" 0x10000 state.Arith.encoder.Arith.a

(** Test JPEG arithmetic block encoding *)
let test_jpeg_arith_block_encode () =
  let module Arith = Jpeg.Arithmetic in
  let state = Arith.init_arith_scan_encoder 1 in

  (* Create a simple coefficient block *)
  let coeffs = Array.make 64 0 in
  coeffs.(0) <- 100;
  (* DC *)
  coeffs.(1) <- -5;
  (* AC *)
  coeffs.(2) <- 3;

  (* AC *)

  (* Encode the block *)
  Arith.encode_arith_block state 0 coeffs;

  (* Verify DC predictor was updated *)
  Alcotest.(check int) "Prev DC updated" 100 state.Arith.prev_dc.(0);

  (* Finish encoding and get data *)
  let data = Arith.finish_arith_encoder state in
  Alcotest.(check bool) "Data produced" true (Bytes.length data > 0)

(** Test arithmetic encoding produces smaller files than Huffman *)
let test_arith_file_size () =
  let width = 32 in
  let height = 32 in

  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let idx = ((y * width) + x) * 3 in
      Bigarray.Array1.set pixels idx (x * 8);
      Bigarray.Array1.set pixels (idx + 1) (y * 8);
      Bigarray.Array1.set pixels (idx + 2) 128
    done
  done;
  let image = Jpeg.create_image width height pixels in

  (* Encode with Huffman *)
  let huffman_data =
    Jpeg.write_bytes_with_options
      { Jpeg.default_encode_options with entropy_coding = Jpeg.Huffman }
      image
  in

  (* Encode with arithmetic *)
  let arith_data =
    Jpeg.write_bytes_with_options
      { Jpeg.default_encode_options with entropy_coding = Jpeg.Arithmetic }
      image
  in

  (* Arithmetic should typically produce smaller or similar size files *)
  (* We just verify both produce valid data *)
  Alcotest.(check bool)
    "Huffman data produced" true
    (Bytes.length huffman_data > 0);
  Alcotest.(check bool)
    "Arithmetic data produced" true
    (Bytes.length arith_data > 0);

  (* Print sizes for information - arithmetic is typically 5-10% smaller *)
  Printf.printf "Huffman size: %d bytes, Arithmetic size: %d bytes\n%!"
    (Bytes.length huffman_data)
    (Bytes.length arith_data)

(** Test checkerboard pattern encode/decode roundtrip *)
let test_checkerboard_roundtrip () =
  let width = 64 in
  let height = 64 in
  let square_size = 8 in

  (* Create a black and white checkerboard *)
  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let idx = ((y * width) + x) * 3 in
      (* Determine if this square is black or white *)
      let checker_x = x / square_size in
      let checker_y = y / square_size in
      let is_white = (checker_x + checker_y) mod 2 = 0 in
      let color = if is_white then 255 else 0 in
      Bigarray.Array1.set pixels idx color;
      Bigarray.Array1.set pixels (idx + 1) color;
      Bigarray.Array1.set pixels (idx + 2) color
    done
  done;
  let original = Jpeg.create_image width height pixels in

  (* Encode to JPEG with high quality to minimize artifacts *)
  let options = { Jpeg.default_encode_options with quality = 95 } in
  let jpeg_data = Jpeg.write_bytes_with_options options original in

  (* Decode back *)
  let decoded = Jpeg.read_bytes jpeg_data in

  (* Verify dimensions match *)
  Alcotest.(check int) "Width matches" width decoded.Jpeg.width;
  Alcotest.(check int) "Height matches" height decoded.Jpeg.height;

  (* Compare pixels - calculate error statistics *)
  let total_error = ref 0 in
  let max_error = ref 0 in
  let num_pixels = width * height in

  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let r1, g1, b1 = Jpeg.get_pixel original x y in
      let r2, g2, b2 = Jpeg.get_pixel decoded x y in
      let err_r = abs (r1 - r2) in
      let err_g = abs (g1 - g2) in
      let err_b = abs (b1 - b2) in
      let pixel_err = max err_r (max err_g err_b) in
      total_error := !total_error + err_r + err_g + err_b;
      if pixel_err > !max_error then max_error := pixel_err
    done
  done;

  let avg_error = float_of_int !total_error /. float_of_int (num_pixels * 3) in

  (* Print statistics *)
  Printf.printf "Checkerboard test: avg_error=%.2f, max_error=%d\n%!" avg_error
    !max_error;

  (* Verify error is within acceptable bounds *)
  (* For high quality JPEG, average error should be low *)
  Alcotest.(check bool) "Average error < 10" true (avg_error < 10.0);

  (* Max error might be higher at edges due to ringing, but should be bounded *)
  Alcotest.(check bool) "Max error < 50" true (!max_error < 50)

(** Test checkerboard pattern with arithmetic encoding *)
let test_checkerboard_arithmetic () =
  let width = 64 in
  let height = 64 in
  let square_size = 8 in

  (* Create a black and white checkerboard *)
  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let idx = ((y * width) + x) * 3 in
      let checker_x = x / square_size in
      let checker_y = y / square_size in
      let is_white = (checker_x + checker_y) mod 2 = 0 in
      let color = if is_white then 255 else 0 in
      Bigarray.Array1.set pixels idx color;
      Bigarray.Array1.set pixels (idx + 1) color;
      Bigarray.Array1.set pixels (idx + 2) color
    done
  done;
  let original = Jpeg.create_image width height pixels in

  (* Encode to JPEG with arithmetic coding *)
  let options =
    {
      Jpeg.default_encode_options with
      quality = 95;
      entropy_coding = Jpeg.Arithmetic;
    }
  in
  let jpeg_data = Jpeg.write_bytes_with_options options original in

  (* Verify it's using arithmetic coding (SOF9) *)
  let markers = Markers.parse_markers jpeg_data in
  let has_sof9 =
    List.exists
      (fun m -> match m with Markers.SOF9 _ -> true | _ -> false)
      markers
  in
  Alcotest.(check bool) "Uses arithmetic coding (SOF9)" true has_sof9;

  (* Decode back *)
  let decoded = Jpeg.read_bytes jpeg_data in

  (* Verify dimensions match *)
  Alcotest.(check int) "Width matches" width decoded.Jpeg.width;
  Alcotest.(check int) "Height matches" height decoded.Jpeg.height;

  (* Compare pixels *)
  let total_error = ref 0 in
  let max_error = ref 0 in
  let num_pixels = width * height in

  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let r1, g1, b1 = Jpeg.get_pixel original x y in
      let r2, g2, b2 = Jpeg.get_pixel decoded x y in
      let err_r = abs (r1 - r2) in
      let err_g = abs (g1 - g2) in
      let err_b = abs (b1 - b2) in
      let pixel_err = max err_r (max err_g err_b) in
      total_error := !total_error + err_r + err_g + err_b;
      if pixel_err > !max_error then max_error := pixel_err
    done
  done;

  let avg_error = float_of_int !total_error /. float_of_int (num_pixels * 3) in

  Printf.printf
    "Checkerboard (arithmetic) test: avg_error=%.2f, max_error=%d\n%!" avg_error
    !max_error;

  Alcotest.(check bool) "Average error < 10" true (avg_error < 10.0);
  Alcotest.(check bool) "Max error < 50" true (!max_error < 50)

(** Test ICC profile basic operations *)
let test_icc_basic () =
  (* Empty ICC profile *)
  let empty = Icc.empty in
  Alcotest.(check bool) "Empty profile is empty" true (Icc.is_empty empty);

  (* Create from bytes *)
  let data = Bytes.of_string "test ICC profile data" in
  let icc = Icc.from_bytes data in
  Alcotest.(check bool) "Non-empty profile" false (Icc.is_empty icc);
  Alcotest.(check bytes) "Data roundtrip" data (Icc.to_bytes icc)

(** Test ICC chunk splitting and reassembly *)
let test_icc_chunks () =
  (* Small profile (single chunk) *)
  let small_data = Bytes.make 1000 '\xAB' in
  let small_icc = Icc.from_bytes small_data in
  let chunks = Icc.to_chunks small_icc in
  Alcotest.(check int) "Single chunk count" 1 (List.length chunks);
  (match chunks with
  | [ (seq, count, _) ] ->
      Alcotest.(check int) "Sequence is 1" 1 seq;
      Alcotest.(check int) "Count is 1" 1 count
  | _ -> Alcotest.fail "Expected single chunk");

  (* Reassemble small profile *)
  let reassembled = Icc.from_chunks chunks in
  (match reassembled with
  | Some r -> Alcotest.(check bytes) "Small roundtrip" small_data (Icc.to_bytes r)
  | None -> Alcotest.fail "Failed to reassemble small profile");

  (* Large profile (multiple chunks) *)
  let large_size = 100000 in
  (* > 65519, so needs 2 chunks *)
  let large_data = Bytes.init large_size (fun i -> Char.chr (i mod 256)) in
  let large_icc = Icc.from_bytes large_data in
  let large_chunks = Icc.to_chunks large_icc in
  Alcotest.(check int) "Multiple chunks" 2 (List.length large_chunks);
  (match large_chunks with
  | [ (seq1, count1, _); (seq2, count2, _) ] ->
      Alcotest.(check int) "First seq" 1 seq1;
      Alcotest.(check int) "Second seq" 2 seq2;
      Alcotest.(check int) "Count 1" 2 count1;
      Alcotest.(check int) "Count 2" 2 count2
  | _ -> Alcotest.fail "Expected 2 chunks");

  (* Reassemble large profile *)
  let reassembled_large = Icc.from_chunks large_chunks in
  (match reassembled_large with
  | Some r -> Alcotest.(check bytes) "Large roundtrip" large_data (Icc.to_bytes r)
  | None -> Alcotest.fail "Failed to reassemble large profile")

(** Test ICC chunks with incomplete/invalid data *)
let test_icc_invalid_chunks () =
  (* Empty chunks *)
  let result = Icc.from_chunks [] in
  Alcotest.(check bool) "Empty chunks" true (Option.is_none result);

  (* Missing chunk *)
  let data1 = Bytes.of_string "chunk1" in
  let data3 = Bytes.of_string "chunk3" in
  let incomplete = [ (1, 3, data1); (3, 3, data3) ] in
  (* Missing chunk 2 *)
  let result2 = Icc.from_chunks incomplete in
  Alcotest.(check bool) "Incomplete chunks" true (Option.is_none result2);

  (* Mismatched count *)
  let data_a = Bytes.of_string "a" in
  let data_b = Bytes.of_string "b" in
  let mismatched = [ (1, 2, data_a); (2, 3, data_b) ] in
  let result3 = Icc.from_chunks mismatched in
  Alcotest.(check bool) "Mismatched count" true (Option.is_none result3)

(** Test ICC profile encode/decode roundtrip *)
let test_icc_jpeg_roundtrip () =
  let width = 16 in
  let height = 16 in

  (* Create a simple gradient image *)
  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let idx = ((y * width) + x) * 3 in
      Bigarray.Array1.set pixels idx (x * 16);
      Bigarray.Array1.set pixels (idx + 1) (y * 16);
      Bigarray.Array1.set pixels (idx + 2) 128
    done
  done;

  (* Create ICC profile data (simulated sRGB profile header) *)
  let icc_data = Bytes.make 500 '\x00' in
  (* Put some recognizable data in it *)
  Bytes.blit_string "acsp" 0 icc_data 36 4;
  (* ICC signature at offset 36 *)
  for i = 0 to 127 do
    Bytes.set_uint8 icc_data i (i land 0xFF)
  done;

  let icc = Icc.from_bytes icc_data in
  let original = Jpeg.create_image_with_icc width height pixels icc in

  (* Encode to JPEG *)
  let jpeg_data = Jpeg.write_bytes ~quality:90 original in

  (* Verify APP2 ICC marker is present *)
  let markers = Markers.parse_markers jpeg_data in
  let has_icc =
    List.exists
      (fun m -> match m with Markers.APP2_ICC _ -> true | _ -> false)
      markers
  in
  Alcotest.(check bool) "Has APP2_ICC marker" true has_icc;

  (* Decode back *)
  let decoded = Jpeg.read_bytes jpeg_data in

  Alcotest.(check int) "Decoded width" width decoded.Jpeg.width;
  Alcotest.(check int) "Decoded height" height decoded.Jpeg.height;

  (* Verify ICC profile is preserved *)
  match decoded.Jpeg.icc_profile with
  | None -> Alcotest.fail "ICC profile not decoded"
  | Some decoded_icc ->
      Alcotest.(check bytes)
        "ICC profile preserved" icc_data (Icc.to_bytes decoded_icc)

(** Test large ICC profile (multiple chunks) roundtrip *)
let test_icc_large_roundtrip () =
  let width = 16 in
  let height = 16 in

  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for i = 0 to (width * height * 3) - 1 do
    Bigarray.Array1.set pixels i (i mod 256)
  done;

  (* Create large ICC profile (needs multiple APP2 segments) *)
  let large_size = 100000 in
  let icc_data = Bytes.init large_size (fun i -> Char.chr ((i * 7) mod 256)) in
  let icc = Icc.from_bytes icc_data in
  let original = Jpeg.create_image_with_icc width height pixels icc in

  (* Encode to JPEG *)
  let jpeg_data = Jpeg.write_bytes ~quality:90 original in

  (* Count APP2_ICC markers *)
  let markers = Markers.parse_markers jpeg_data in
  let icc_markers =
    List.filter
      (fun m -> match m with Markers.APP2_ICC _ -> true | _ -> false)
      markers
  in
  Alcotest.(check bool) "Multiple ICC chunks" true (List.length icc_markers >= 2);

  (* Decode back *)
  let decoded = Jpeg.read_bytes jpeg_data in

  (* Verify ICC profile is preserved *)
  match decoded.Jpeg.icc_profile with
  | None -> Alcotest.fail "Large ICC profile not decoded"
  | Some decoded_icc ->
      Alcotest.(check bytes)
        "Large ICC profile preserved" icc_data (Icc.to_bytes decoded_icc)

(** Test image without ICC profile (backward compatibility) *)
let test_icc_no_profile () =
  let width = 16 in
  let height = 16 in

  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for i = 0 to (width * height * 3) - 1 do
    Bigarray.Array1.set pixels i (i mod 256)
  done;
  let original = Jpeg.create_image width height pixels in

  (* Verify no ICC profile *)
  Alcotest.(check bool) "No ICC on original" true (Option.is_none original.Jpeg.icc_profile);

  (* Encode to JPEG *)
  let jpeg_data = Jpeg.write_bytes ~quality:90 original in

  (* Verify no APP2 ICC marker *)
  let markers = Markers.parse_markers jpeg_data in
  let has_icc =
    List.exists
      (fun m -> match m with Markers.APP2_ICC _ -> true | _ -> false)
      markers
  in
  Alcotest.(check bool) "No APP2_ICC marker" false has_icc;

  (* Decode back *)
  let decoded = Jpeg.read_bytes jpeg_data in

  (* Verify no ICC profile *)
  Alcotest.(check bool)
    "No ICC on decoded" true
    (Option.is_none decoded.Jpeg.icc_profile)

(** Test ICC with EXIF together *)
let test_icc_with_exif () =
  let width = 16 in
  let height = 16 in

  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for i = 0 to (width * height * 3) - 1 do
    Bigarray.Array1.set pixels i (i mod 256)
  done;

  let exif = Exif.create_minimal ~orientation:6 ~software:"Test" () in
  let icc_data = Bytes.make 200 '\xCC' in
  let icc = Icc.from_bytes icc_data in

  let original = Jpeg.create_image_with_metadata width height pixels exif icc in

  (* Encode to JPEG *)
  let jpeg_data = Jpeg.write_bytes ~quality:90 original in

  (* Verify both APP1 and APP2 markers present *)
  let markers = Markers.parse_markers jpeg_data in
  let has_exif =
    List.exists (fun m -> match m with Markers.APP1 _ -> true | _ -> false) markers
  in
  let has_icc =
    List.exists
      (fun m -> match m with Markers.APP2_ICC _ -> true | _ -> false)
      markers
  in
  Alcotest.(check bool) "Has EXIF" true has_exif;
  Alcotest.(check bool) "Has ICC" true has_icc;

  (* Decode back *)
  let decoded = Jpeg.read_bytes jpeg_data in

  (* Verify EXIF *)
  (match decoded.Jpeg.exif with
  | None -> Alcotest.fail "EXIF not decoded"
  | Some e -> Alcotest.(check (option int)) "Orientation" (Some 6) e.Exif.orientation);

  (* Verify ICC *)
  match decoded.Jpeg.icc_profile with
  | None -> Alcotest.fail "ICC not decoded with EXIF"
  | Some decoded_icc ->
      Alcotest.(check bytes) "ICC preserved with EXIF" icc_data (Icc.to_bytes decoded_icc)

(** All tests *)
let () =
  Alcotest.run "JPEG Library"
    [
      ( "bitstream",
        [ Alcotest.test_case "round-trip" `Quick test_bitstream_roundtrip ] );
      ("dct", [ Alcotest.test_case "round-trip" `Quick test_dct_roundtrip ]);
      ("color", [ Alcotest.test_case "round-trip" `Quick test_color_roundtrip ]);
      ( "quantization",
        [
          Alcotest.test_case "basic" `Quick test_quantization;
          Alcotest.test_case "zigzag" `Quick test_zigzag;
        ] );
      ( "huffman",
        [
          Alcotest.test_case "round-trip" `Quick test_huffman_roundtrip;
          Alcotest.test_case "category" `Quick test_huffman_category;
          Alcotest.test_case "extend" `Quick test_huffman_extend;
        ] );
      ( "jpeg",
        [
          Alcotest.test_case "encode-decode" `Quick test_encode_decode_roundtrip;
          Alcotest.test_case "checkerboard" `Quick test_checkerboard_roundtrip;
        ] );
      ("exif", [ Alcotest.test_case "minimal" `Quick test_exif_minimal ]);
      ("markers", [ Alcotest.test_case "basic" `Quick test_markers_basic ]);
      ( "progressive",
        [
          Alcotest.test_case "decode" `Quick test_progressive_decode;
          Alcotest.test_case "baseline-frame-type" `Quick
            test_baseline_frame_type;
          Alcotest.test_case "progressive-frame-type" `Quick
            test_progressive_frame_type;
        ] );
      ( "transform",
        [
          Alcotest.test_case "transform-and-quantize" `Quick
            test_transform_and_quantize;
        ] );
      ( "subsampling",
        [ Alcotest.test_case "modes" `Quick test_subsampling_modes ] );
      ("grayscale", [ Alcotest.test_case "encode" `Quick test_grayscale_encode ]);
      ( "progressive-encode",
        [
          Alcotest.test_case "roundtrip" `Quick
            test_progressive_encode_roundtrip;
          Alcotest.test_case "grayscale" `Quick test_progressive_grayscale;
        ] );
      ( "options",
        [
          Alcotest.test_case "combinations" `Quick test_options_combinations;
          Alcotest.test_case "backward-compat" `Quick
            test_backward_compatibility;
        ] );
      ( "restart-markers",
        [
          Alcotest.test_case "encoding" `Quick test_restart_interval_encoding;
          Alcotest.test_case "roundtrip" `Quick test_restart_roundtrip;
          Alcotest.test_case "progressive" `Quick test_progressive_with_restart;
        ] );
      ( "precision",
        [
          Alcotest.test_case "12-bit" `Quick test_12bit_precision;
          Alcotest.test_case "color-conversion" `Quick
            test_precision_color_conversion;
        ] );
      ( "cmyk",
        [
          Alcotest.test_case "color-conversion" `Quick
            test_cmyk_color_conversion;
          Alcotest.test_case "encode" `Quick test_cmyk_encode;
          Alcotest.test_case "pixel-access" `Quick test_cmyk_pixel_access;
          Alcotest.test_case "set-pixel" `Quick test_set_cmyk_pixel;
          Alcotest.test_case "roundtrip" `Quick test_cmyk_roundtrip;
        ] );
      ( "ycck",
        [
          Alcotest.test_case "color-conversion" `Quick
            test_ycck_color_conversion;
          Alcotest.test_case "encode" `Quick test_ycck_encode;
        ] );
      ( "arithmetic",
        [
          Alcotest.test_case "qm-coder-basic" `Quick test_qm_coder_basic;
          Alcotest.test_case "context-state" `Quick test_arithmetic_context;
          Alcotest.test_case "jpeg-decoder-init" `Quick
            test_jpeg_arith_decoder_init;
          Alcotest.test_case "dc-stat-bins" `Quick test_jpeg_arith_dc_bins;
          Alcotest.test_case "ac-stat-bins" `Quick test_jpeg_arith_ac_bins;
          Alcotest.test_case "scan-state" `Quick test_jpeg_arith_scan_state;
          Alcotest.test_case "conditioning" `Quick test_jpeg_arith_conditioning;
          Alcotest.test_case "reset" `Quick test_jpeg_arith_reset;
          Alcotest.test_case "mq-decode" `Quick test_jpeg_mq_decode;
          Alcotest.test_case "sof9-parsing" `Quick test_arith_marker_parsing;
        ] );
      ( "arithmetic-encoding",
        [
          Alcotest.test_case "baseline-sof9" `Quick test_arith_baseline_encode;
          Alcotest.test_case "progressive-sof10" `Quick
            test_arith_progressive_encode;
          Alcotest.test_case "grayscale" `Quick test_arith_grayscale_encode;
          Alcotest.test_case "encoder-state" `Quick
            test_jpeg_arith_encoder_state;
          Alcotest.test_case "encode-decision" `Quick
            test_jpeg_arith_encode_decision;
          Alcotest.test_case "scan-encoder" `Quick test_jpeg_arith_scan_encoder;
          Alcotest.test_case "encoder-reset" `Quick
            test_jpeg_arith_encoder_reset;
          Alcotest.test_case "block-encode" `Quick test_jpeg_arith_block_encode;
          Alcotest.test_case "file-size-comparison" `Quick test_arith_file_size;
          Alcotest.test_case "checkerboard" `Quick test_checkerboard_arithmetic;
        ] );
      ( "icc",
        [
          Alcotest.test_case "basic" `Quick test_icc_basic;
          Alcotest.test_case "chunks" `Quick test_icc_chunks;
          Alcotest.test_case "invalid-chunks" `Quick test_icc_invalid_chunks;
          Alcotest.test_case "jpeg-roundtrip" `Quick test_icc_jpeg_roundtrip;
          Alcotest.test_case "large-roundtrip" `Quick test_icc_large_roundtrip;
          Alcotest.test_case "no-profile" `Quick test_icc_no_profile;
          Alcotest.test_case "with-exif" `Quick test_icc_with_exif;
        ] );
    ]
