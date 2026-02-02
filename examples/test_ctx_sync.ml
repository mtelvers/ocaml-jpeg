(* Test context synchronization between encoder and decoder *)
let () =
  let module A = Jpeg.Arithmetic in
  A.debug_byteout := false;
  A.debug_decode := false;

  Printf.printf "=== Testing context sync for multiple bits ===\n\n";

  (* Test encoding sequence: 1,1,1,1,1,1,0 into same context *)
  let bits_to_encode = [| 1; 1; 1; 1; 1; 1; 0 |] in

  let encoder = A.init_jpeg_encoder () in
  let enc_ctx = A.create_context () in

  Printf.printf "Encoding %d bits: " (Array.length bits_to_encode);
  Array.iter (fun b -> Printf.printf "%d " b) bits_to_encode;
  Printf.printf "\n\n";

  Printf.printf "Initial encoder: c=%08x a=%04x ct=%d\n" encoder.A.c encoder.A.a encoder.A.ct;
  Printf.printf "Initial enc_ctx: idx=%d mps=%d\n\n" enc_ctx.A.index enc_ctx.A.mps;

  Array.iteri (fun i bit ->
    Printf.printf "Encoding bit %d = %d:\n" i bit;
    Printf.printf "  Before: c=%08x a=%04x ct=%d idx=%d mps=%d\n"
      encoder.A.c encoder.A.a encoder.A.ct enc_ctx.A.index enc_ctx.A.mps;
    A.encode_decision enc_ctx encoder bit;
    Printf.printf "  After:  c=%08x a=%04x ct=%d idx=%d mps=%d\n"
      encoder.A.c encoder.A.a encoder.A.ct enc_ctx.A.index enc_ctx.A.mps
  ) bits_to_encode;

  let encoded = A.flush_jpeg_encoder encoder in

  Printf.printf "\nEncoded %d bytes: " (Bytes.length encoded);
  for i = 0 to Bytes.length encoded - 1 do
    Printf.printf "%02x " (Bytes.get_uint8 encoded i)
  done;
  Printf.printf "\n\n";

  (* Now decode *)
  Printf.printf "=== Decoding ===\n\n";

  let decoder = A.init_jpeg_decoder encoded in
  let dec_ctx = A.create_context () in

  Printf.printf "Initial decoder: c=%08x a=%d ct=%d\n" decoder.A.c decoder.A.a decoder.A.ct;
  Printf.printf "Initial dec_ctx: idx=%d mps=%d\n\n" dec_ctx.A.index dec_ctx.A.mps;

  let decoded_bits = Array.make (Array.length bits_to_encode) 0 in
  for i = 0 to Array.length bits_to_encode - 1 do
    Printf.printf "Decoding bit %d:\n" i;
    Printf.printf "  Before: c=%08x a=%04x ct=%d idx=%d mps=%d\n"
      decoder.A.c decoder.A.a decoder.A.ct dec_ctx.A.index dec_ctx.A.mps;
    let bit = A.decode_decision dec_ctx decoder in
    decoded_bits.(i) <- bit;
    Printf.printf "  Decoded: %d (expected %d)\n" bit bits_to_encode.(i);
    Printf.printf "  After:  c=%08x a=%04x ct=%d idx=%d mps=%d\n"
      decoder.A.c decoder.A.a decoder.A.ct dec_ctx.A.index dec_ctx.A.mps
  done;

  Printf.printf "\nDecoded bits: ";
  Array.iter (fun b -> Printf.printf "%d " b) decoded_bits;
  Printf.printf "\n";

  if decoded_bits = bits_to_encode then
    Printf.printf "\nSUCCESS!\n"
  else
    Printf.printf "\nFAILURE!\n"
