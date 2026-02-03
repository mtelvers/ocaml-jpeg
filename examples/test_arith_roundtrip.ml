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

  (* S0: is the difference non-zero?
     Convention: decode_decision returns 0 → diff is zero, 1 → diff is non-zero *)
  Printf.printf "1. Decode S0 (non-zero flag):\n";
  let is_nonzero = A.decode_decision dc_bins.A.dc_s0 decoder in
  Printf.printf "   is_nonzero = %d (expected 1 for DC=-64)\n\n" is_nonzero;

  if is_nonzero = 0 then begin
    Printf.printf "   ERROR: got 0 (zero), but DC should be non-zero\n";
    exit 1
  end;

  (* Sign: 0 = positive, 1 = negative *)
  Printf.printf "2. Decode sign:\n";
  let sign = A.decode_decision dc_bins.A.dc_sign decoder in
  Printf.printf "   sign = %d (expected 1 for negative)\n\n" sign;

  let sign_ctx = if sign = 0 then dc_bins.A.dc_sp else dc_bins.A.dc_sn in

  (* Category via unary coding *)
  Printf.printf "3. Decode category (unary):\n";
  let rec decode_category sz =
    if sz >= 15 then sz
    else begin
      let bit = A.decode_decision sign_ctx.(min sz 4) decoder in
      Printf.printf "   sz=%d: bit=%d\n" sz bit;
      if bit = 0 then sz else decode_category (sz + 1)
    end
  in
  let category = decode_category 1 in
  Printf.printf "   Category = %d (expected 7)\n\n" category;

  (* Magnitude bits *)
  Printf.printf "4. Decode magnitude bits (category-1 = %d bits):\n" (category - 1);
  let rec decode_bits acc remaining =
    if remaining <= 0 then acc
    else begin
      let bit = A.decode_decision dc_bins.A.dc_x1 decoder in
      Printf.printf "   bit = %d\n" bit;
      decode_bits ((acc lsl 1) lor bit) (remaining - 1)
    end
  in
  let extra_bits = decode_bits 0 (category - 1) in
  Printf.printf "   extra_bits = %d\n\n" extra_bits;

  let magnitude = (1 lsl (category - 1)) + extra_bits in
  Printf.printf "5. Magnitude = (1 << %d) + %d = %d\n" (category - 1) extra_bits magnitude;

  let dc_value = if sign = 0 then magnitude else -magnitude in
  Printf.printf "6. DC value = %d (expected -64)\n\n" dc_value;

  if dc_value = -64 then
    Printf.printf "SUCCESS: Manual step-by-step decode matches expected value\n"
  else begin
    Printf.printf "FAIL: Manual decode got %d, expected -64\n" dc_value;
    exit 1
  end
