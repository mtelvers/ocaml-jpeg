(* Test DC coefficient encoding/decoding roundtrip *)
let () =
  let module A = Jpeg.Arithmetic in

  Printf.printf "=== Testing DC coefficient roundtrip ===\n\n";

  (* Test various DC values *)
  let test_values = [| 0; 1; -1; 64; -64; 127; -127; 255; -255; 512; -512 |] in

  let all_ok = ref true in
  Array.iter (fun dc_value ->
    (* Create encoder state - use the scan-level encoder *)
    let enc_state = A.init_arith_scan_encoder 1 in

    (* Create a block with just the DC coefficient *)
    let coeffs = Array.make 64 0 in
    coeffs.(0) <- dc_value;

    (* Encode the block *)
    A.encode_arith_block enc_state 0 coeffs;

    (* Flush encoder *)
    let encoded = A.finish_arith_encoder enc_state in

    (* Create decoder state *)
    let dec_state = A.init_arith_scan_decoder encoded 1 in

    (* Decode the block *)
    let decoded_coeffs = A.decode_arith_block dec_state 0 in

    let decoded = decoded_coeffs.(0) in
    let status = if decoded = dc_value then "OK" else (all_ok := false; "MISMATCH") in
    Printf.printf "DC %4d: encoded %2d bytes -> decoded %4d %s\n"
      dc_value (Bytes.length encoded) decoded status
  ) test_values;

  if !all_ok then
    Printf.printf "\nSUCCESS: All DC values roundtrip correctly\n"
  else
    Printf.printf "\nFAILURE: Some DC values don't match\n"
