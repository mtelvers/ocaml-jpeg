let () =
  Printexc.record_backtrace true;
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

  Printf.printf "Encoding checkerboard %dx%d with arithmetic coding\n" width height;

  (* Create the image *)
  let image = Jpeg.create_image width height pixels in

  (* Encode with arithmetic coding - use quality=95 like the alcotest *)
  let options = { Jpeg.default_encode_options with
    quality = 95;
    entropy_coding = Jpeg.Arithmetic;
  } in
  (* Jpeg.Arithmetic.debug_byteout := true; *)
  let encoded = Jpeg.write_bytes_with_options options image in

  Printf.printf "Encoded size: %d bytes\n" (Bytes.length encoded);

  (* Print all bytes around problem areas *)
  Printf.printf "\nBytes around position 177:\n";
  for i = max 0 (177 - 10) to min (Bytes.length encoded - 1) (177 + 10) do
    Printf.printf "%02x " (Bytes.get_uint8 encoded i)
  done;
  Printf.printf "\n";

  Printf.printf "\nBytes around position 213:\n";
  for i = max 0 (213 - 10) to min (Bytes.length encoded - 1) (213 + 10) do
    Printf.printf "%02x " (Bytes.get_uint8 encoded i)
  done;
  Printf.printf "\n";

  (* Manually trace the file structure *)
  Printf.printf "\n--- Manual JPEG structure analysis ---\n";
  let pos = ref 0 in
  let len = Bytes.length encoded in
  while !pos < len - 1 do
    let b = Bytes.get_uint8 encoded !pos in
    if b = 0xFF then begin
      let marker = Bytes.get_uint8 encoded (!pos + 1) in
      Printf.printf "pos=%d: FF %02x" !pos marker;
      if marker = 0xD8 then Printf.printf " (SOI)\n"
      else if marker = 0xD9 then (Printf.printf " (EOI)\n"; pos := len)
      else if marker = 0x00 then Printf.printf " (stuffed byte)\n"
      else if marker >= 0xD0 && marker <= 0xD7 then Printf.printf " (RST)\n"
      else if marker = 0xE0 then begin
        let seg_len = (Bytes.get_uint8 encoded (!pos + 2) lsl 8) lor Bytes.get_uint8 encoded (!pos + 3) in
        Printf.printf " (APP0, len=%d)\n" seg_len;
        pos := !pos + 2 + seg_len - 1
      end
      else if marker = 0xDB then begin
        let seg_len = (Bytes.get_uint8 encoded (!pos + 2) lsl 8) lor Bytes.get_uint8 encoded (!pos + 3) in
        Printf.printf " (DQT, len=%d)\n" seg_len;
        pos := !pos + 2 + seg_len - 1
      end
      else if marker = 0xCC then begin
        let seg_len = (Bytes.get_uint8 encoded (!pos + 2) lsl 8) lor Bytes.get_uint8 encoded (!pos + 3) in
        Printf.printf " (DAC, len=%d)\n" seg_len;
        pos := !pos + 2 + seg_len - 1
      end
      else if marker = 0xC9 then begin
        let seg_len = (Bytes.get_uint8 encoded (!pos + 2) lsl 8) lor Bytes.get_uint8 encoded (!pos + 3) in
        Printf.printf " (SOF9, len=%d)\n" seg_len;
        pos := !pos + 2 + seg_len - 1
      end
      else if marker = 0xDA then begin
        let seg_len = (Bytes.get_uint8 encoded (!pos + 2) lsl 8) lor Bytes.get_uint8 encoded (!pos + 3) in
        Printf.printf " (SOS, len=%d) - start of entropy data\n" seg_len;
        pos := !pos + 2 + seg_len - 1;
        (* Now scan for next marker (skipping stuffed bytes) *)
        Printf.printf "     Scanning entropy data...\n";
        let found = ref false in
        while !pos < len - 1 && not !found do
          incr pos;
          if Bytes.get_uint8 encoded !pos = 0xFF then begin
            let next = Bytes.get_uint8 encoded (!pos + 1) in
            if next <> 0x00 && (next < 0xD0 || next > 0xD7) then begin
              Printf.printf "     Found marker at %d: FF %02x\n" !pos next;
              found := true;
              decr pos  (* Will be incremented below *)
            end
          end
        done
      end
      else Printf.printf " (unknown marker)\n"
    end;
    incr pos
  done;

  (* Try parsing markers - this is what fails in alcotest *)
  Printf.printf "\nTrying Markers.parse_markers...\n";
  (try
    let markers = Jpeg.Markers.parse_markers encoded in
    Printf.printf "Successfully parsed %d markers\n" (List.length markers);
    List.iter (fun m ->
      match m with
      | Jpeg.Markers.SOI -> Printf.printf "  SOI\n"
      | Jpeg.Markers.EOI -> Printf.printf "  EOI\n"
      | Jpeg.Markers.SOF0 _ -> Printf.printf "  SOF0\n"
      | Jpeg.Markers.SOF2 _ -> Printf.printf "  SOF2\n"
      | Jpeg.Markers.SOF9 _ -> Printf.printf "  SOF9\n"
      | Jpeg.Markers.SOF10 _ -> Printf.printf "  SOF10\n"
      | Jpeg.Markers.DHT _ -> Printf.printf "  DHT\n"
      | Jpeg.Markers.DAC _ -> Printf.printf "  DAC\n"
      | Jpeg.Markers.DQT _ -> Printf.printf "  DQT\n"
      | Jpeg.Markers.DRI _ -> Printf.printf "  DRI\n"
      | Jpeg.Markers.SOS _ -> Printf.printf "  SOS\n"
      | Jpeg.Markers.APP0 _ -> Printf.printf "  APP0\n"
      | Jpeg.Markers.APP1 _ -> Printf.printf "  APP1\n"
      | Jpeg.Markers.APP2_ICC _ -> Printf.printf "  APP2_ICC\n"
      | Jpeg.Markers.COM _ -> Printf.printf "  COM\n"
      | Jpeg.Markers.Unknown (m, _) -> Printf.printf "  Unknown(0x%02x)\n" m
    ) markers
  with e ->
    Printf.printf "parse_markers failed: %s\n" (Printexc.to_string e);
    Printexc.print_backtrace stdout);

  (* Print first 50 bytes in hex *)
  Printf.printf "First bytes: ";
  for i = 0 to min 49 (Bytes.length encoded - 1) do
    Printf.printf "%02x " (Bytes.get_uint8 encoded i)
  done;
  Printf.printf "\n";

  (* Check for FF bytes that aren't properly stuffed *)
  let ff_count = ref 0 in
  let unstuffed = ref 0 in
  for i = 0 to Bytes.length encoded - 2 do
    if Bytes.get_uint8 encoded i = 0xFF then begin
      incr ff_count;
      let next = Bytes.get_uint8 encoded (i + 1) in
      if next <> 0x00 && (next < 0xD0 || next > 0xD7) && next <> 0xD8 && next <> 0xD9
         && next <> 0xC0 && next <> 0xC4 && next <> 0xDA && next <> 0xDB && next <> 0xDD
         && next <> 0xC9 && next <> 0xC1 && next <> 0xC2 then begin
        (* Unexpected marker in data *)
        incr unstuffed;
        Printf.printf "Unexpected FF at %d followed by %02x\n" i next
      end
    end
  done;
  Printf.printf "Total FF bytes: %d, unexpected markers: %d\n" !ff_count !unstuffed;

  (* Try to decode *)
  Printf.printf "\nAttempting decode...\n";
  try
    let decoded = Jpeg.read_bytes encoded in
    Printf.printf "Decoded successfully: %dx%d\n" decoded.Jpeg.width decoded.Jpeg.height;

    (* Compare original and decoded pixels *)
    let total_error = ref 0 in
    let max_error = ref 0 in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let orig_r, orig_g, orig_b = Jpeg.get_pixel image x y in
        let dec_r, dec_g, dec_b = Jpeg.get_pixel decoded x y in
        let err = max (abs (orig_r - dec_r)) (max (abs (orig_g - dec_g)) (abs (orig_b - dec_b))) in
        total_error := !total_error + err;
        max_error := max !max_error err
      done
    done;
    let avg_error = float_of_int !total_error /. float_of_int (width * height) in
    Printf.printf "Pixel comparison: avg_error=%.2f, max_error=%d\n" avg_error !max_error;

    (* Print some sample pixels *)
    Printf.printf "\nSample pixels (x,y -> orig vs decoded):\n";
    let samples = [(0,0); (4,0); (8,0); (0,8); (4,8)] in
    List.iter (fun (x,y) ->
      let orig_r, orig_g, orig_b = Jpeg.get_pixel image x y in
      let dec_r, dec_g, dec_b = Jpeg.get_pixel decoded x y in
      Printf.printf "  (%d,%d): orig=(%d,%d,%d) dec=(%d,%d,%d)\n"
        x y orig_r orig_g orig_b dec_r dec_g dec_b
    ) samples
  with e ->
    Printf.printf "Decode failed: %s\n" (Printexc.to_string e)
