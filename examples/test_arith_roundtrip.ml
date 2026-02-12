let () =
  Printf.printf "Detailed trace of DC=-64 encoding/decoding\n\n";

  let module A = Jpeg.Arithmetic in

  (* Create a block with DC = -64 *)
  let coeffs = Array.make 64 0 in
  coeffs.(0) <- (-64);

  Printf.printf "Encoding DC = -64:\n";
  Printf.printf "  Expected: category 7, sign 1, magnitude bits 000000\n\n";

  (* Encode *)
  let enc_state = A.init_arith_scan_encoder 1 in
  A.encode_arith_block enc_state 0 coeffs;
  let encoded = A.finish_arith_encoder enc_state in

  Printf.printf "Encoded %d bytes: " (Bytes.length encoded);
  for i = 0 to Bytes.length encoded - 1 do
    Printf.printf "%02x " (Bytes.get_uint8 encoded i)
  done;
  Printf.printf "\n\n";

  (* Decode using the proper scan decoder API *)
  Printf.printf "Decoding via decode_arith_block:\n";
  let dec_state = A.init_arith_scan_decoder encoded 1 in
  let decoded_coeffs = A.decode_arith_block dec_state 0 in
  let decoded_dc = decoded_coeffs.(0) in
  Printf.printf "  Decoded DC = %d (expected -64)\n\n" decoded_dc;

  if decoded_dc <> -64 then begin
    Printf.printf "FAIL: DC mismatch: got %d, expected -64\n" decoded_dc;
    exit 1
  end;

  (* Also do a step-by-step manual decode to trace the protocol *)
  Printf.printf "Step-by-step manual decode trace:\n";
  let decoder = A.init_jpeg_decoder encoded in
  let dc_bins = A.create_dc_stat_bins () in
  let dc_context = 0 in

  (* S0: is the difference non-zero?
     Convention: decode_decision returns 0 -> diff is zero, 1 -> diff is non-zero *)
  Printf.printf "1. Decode S0 (non-zero flag):\n";
  let is_nonzero = A.decode_decision dc_bins.(dc_context + 0) decoder in
  Printf.printf "   is_nonzero = %d (expected 1 for DC=-64)\n\n" is_nonzero;

  if is_nonzero = 0 then begin
    Printf.printf "   ERROR: got 0 (zero), but DC should be non-zero\n";
    exit 1
  end;

  (* Sign: 0 = positive, 1 = negative *)
  Printf.printf "2. Decode sign:\n";
  let sign = A.decode_decision dc_bins.(dc_context + 1) decoder in
  Printf.printf "   sign = %d (expected 1 for negative)\n\n" sign;

  let st_mag = if sign = 0 then dc_context + 2 else dc_context + 3 in

  (* Category via unary coding matching libjpeg protocol:
     First test at st_mag, then X1 range starting at bins.(20) *)
  Printf.printf "3. Decode category (unary):\n";
  let st_ref = ref st_mag in
  let sx = A.decode_decision dc_bins.(!st_ref) decoder in
  Printf.printf "   sx=%d (magnitude > 1?)\n" sx;
  let category =
    if sx = 0 then begin
      Printf.printf "   Category = 1\n\n";
      1
    end else begin
      st_ref := 20;  (* X1 *)
      let m = ref 1 in
      let continue_loop = ref true in
      while !continue_loop do
        let bit = A.decode_decision dc_bins.(!st_ref) decoder in
        Printf.printf "   st=%d: bit=%d\n" !st_ref bit;
        if bit <> 0 then begin
          m := !m lsl 1;
          st_ref := !st_ref + 1
        end else
          continue_loop := false
      done;
      (* category = number of bits needed to represent magnitude *)
      let cat = ref 1 in
      let tmp = ref !m in
      while !tmp > 0 do incr cat; tmp := !tmp lsr 1 done;
      Printf.printf "   Category = %d (expected 7)\n\n" !cat;
      !cat
    end
  in

  (* Magnitude bit pattern at st_ref + 14 *)
  Printf.printf "4. Decode magnitude bits (category-1 = %d bits):\n" (category - 1);
  let st_bits = !st_ref + 14 in
  let v = ref 0 in
  let m2 = ref (1 lsl (category - 2)) in
  while !m2 >= 1 do
    let bit = A.decode_decision dc_bins.(st_bits) decoder in
    Printf.printf "   bit = %d\n" bit;
    if bit <> 0 then v := !v lor !m2;
    m2 := !m2 lsr 1
  done;
  Printf.printf "   extra_bits = %d\n\n" !v;

  let magnitude = !v + 1 in
  Printf.printf "5. Magnitude = %d + 1 = %d\n" !v magnitude;

  let dc_value = if sign = 0 then magnitude else -magnitude in
  Printf.printf "6. DC value = %d (expected -64)\n\n" dc_value;

  if dc_value = -64 then
    Printf.printf "SUCCESS: Manual step-by-step decode matches expected value\n"
  else begin
    Printf.printf "FAIL: Manual decode got %d, expected -64\n" dc_value;
    exit 1
  end
