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
    ]
