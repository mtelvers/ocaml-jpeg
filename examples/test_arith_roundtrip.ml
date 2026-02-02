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

  (* Now decode bit by bit to see what happens *)
  Printf.printf "Decoding step by step:\n";
  let decoder = A.init_jpeg_decoder encoded in
  let dc_bins = A.create_dc_stat_bins () in

  Printf.printf "Initial decoder: a=%04x c=%08x\n\n" decoder.A.a decoder.A.c;

  (* S0: is zero? *)
  Printf.printf "1. Decode S0 (is_zero):\n";
  let is_zero = A.decode_decision dc_bins.A.dc_s0 decoder in
  Printf.printf "   is_zero = %d (expected 0 meaning non-zero)\n\n" is_zero;

  if is_zero = 1 then begin
    Printf.printf "   ERROR: got is_zero=1, DC should be 0\n";
    exit 1
  end;

  (* Sign *)
  Printf.printf "2. Decode sign:\n";
  let sign = A.decode_decision dc_bins.A.dc_sign decoder in
  Printf.printf "   sign = %d (expected 1 for negative)\n\n" sign;

  let sign_ctx = if sign = 0 then dc_bins.A.dc_sp else dc_bins.A.dc_sn in

  (* Category *)
  Printf.printf "3. Decode category (unary):\n";
  let rec decode_category sz bits =
    if sz >= 15 then (sz, bits)
    else begin
      let bit = A.decode_decision sign_ctx.(min sz 4) decoder in
      Printf.printf "   sz=%d: bit=%d\n" sz bit;
      if bit = 0 then (sz, bits @ [bit])
      else decode_category (sz + 1) (bits @ [bit])
    end
  in
  let category, cat_bits = decode_category 1 [] in
  Printf.printf "   Category = %d (bits: " category;
  List.iter (Printf.printf "%d") cat_bits;
  Printf.printf "0) (expected 7)\n\n";

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
  Printf.printf "5. Magnitude = (1 << %d) + %d = %d\n" (category-1) extra_bits magnitude;

  let dc_value = if sign = 0 then magnitude else -magnitude in
  Printf.printf "6. DC value = %d (expected -64)\n" dc_value
