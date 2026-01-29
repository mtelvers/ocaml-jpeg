(** Unit tests for the JPEG library *)

module Bitstream = Jpeg.Bitstream
module Dct = Jpeg.Dct
module Color = Jpeg.Color
module Quantization = Jpeg.Quantization
module Huffman = Jpeg.Huffman
module Exif = Jpeg.Exif
module Markers = Jpeg.Markers

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
    ]
