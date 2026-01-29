(** Arithmetic coding for JPEG (QM-Coder per ISO 10918-2)

    The QM-coder is a binary arithmetic coder used in JPEG arithmetic mode. It
    uses adaptive probability estimation with 113 states. *)

(** QM-Coder probability state table (from ISO 10918-2 Table D.2). Each entry is
    (Qe value, NMPS index, NLPS index, switch flag). Qe is the probability
    estimate for the LPS (less probable symbol). *)
let qm_table =
  [|
    (* Index 0-9 *)
    (0x5A1D, 1, 1, 1);
    (0x2586, 2, 6, 0);
    (0x1114, 3, 9, 0);
    (0x080B, 4, 12, 0);
    (0x03D8, 5, 29, 0);
    (0x01DA, 6, 33, 0);
    (0x00E5, 7, 6, 1);
    (0x006F, 8, 14, 0);
    (0x0036, 9, 14, 0);
    (0x001A, 10, 14, 0);
    (* Index 10-19 *)
    (0x000D, 11, 17, 0);
    (0x0006, 12, 18, 0);
    (0x0003, 13, 20, 0);
    (0x0001, 13, 21, 0);
    (0x5A7F, 15, 14, 1);
    (0x3F25, 16, 14, 0);
    (0x2CF2, 17, 15, 0);
    (0x207C, 18, 16, 0);
    (0x17B9, 19, 17, 0);
    (0x1182, 20, 18, 0);
    (* Index 20-29 *)
    (0x0CEF, 21, 19, 0);
    (0x09A1, 22, 19, 0);
    (0x072F, 23, 20, 0);
    (0x055C, 24, 21, 0);
    (0x0406, 25, 22, 0);
    (0x0303, 26, 23, 0);
    (0x0240, 27, 24, 0);
    (0x01B1, 28, 25, 0);
    (0x0144, 29, 26, 0);
    (0x00F5, 30, 27, 0);
    (* Index 30-39 *)
    (0x00B7, 31, 28, 0);
    (0x008A, 32, 29, 0);
    (0x0068, 33, 30, 0);
    (0x004E, 34, 31, 0);
    (0x003B, 35, 32, 0);
    (0x002C, 36, 33, 0);
    (0x5AE1, 37, 34, 1);
    (0x484C, 38, 35, 0);
    (0x3A0D, 39, 36, 0);
    (0x2EF1, 40, 37, 0);
    (* Index 40-49 *)
    (0x261F, 41, 38, 0);
    (0x1F33, 42, 39, 0);
    (0x19A8, 43, 40, 0);
    (0x1518, 44, 41, 0);
    (0x1177, 45, 42, 0);
    (0x0E74, 46, 43, 0);
    (0x0BFB, 47, 44, 0);
    (0x09F8, 48, 45, 0);
    (0x0861, 49, 46, 0);
    (0x0706, 50, 47, 0);
    (* Index 50-59 *)
    (0x05CD, 51, 48, 0);
    (0x04DE, 52, 49, 0);
    (0x040F, 53, 50, 0);
    (0x0363, 54, 51, 0);
    (0x02D4, 55, 52, 0);
    (0x025C, 56, 53, 0);
    (0x01F8, 57, 54, 0);
    (0x01A4, 58, 55, 0);
    (0x0160, 59, 56, 0);
    (0x0125, 60, 57, 0);
    (* Index 60-69 *)
    (0x00F6, 61, 58, 0);
    (0x00CB, 62, 59, 0);
    (0x00AB, 63, 60, 0);
    (0x008F, 64, 61, 0);
    (0x0068, 65, 62, 0);
    (0x004E, 66, 63, 0);
    (0x003B, 67, 64, 0);
    (0x002C, 67, 65, 0);
    (0x5B12, 69, 66, 1);
    (0x4D04, 70, 67, 0);
    (* Index 70-79 *)
    (0x412C, 71, 68, 0);
    (0x37D8, 72, 69, 0);
    (0x2FE8, 73, 70, 0);
    (0x293C, 74, 71, 0);
    (0x2379, 75, 72, 0);
    (0x1EDF, 76, 73, 0);
    (0x1AA9, 77, 74, 0);
    (0x174E, 78, 75, 0);
    (0x1424, 79, 76, 0);
    (0x119C, 80, 77, 0);
    (* Index 80-89 *)
    (0x0F6B, 81, 78, 0);
    (0x0D51, 82, 79, 0);
    (0x0BB6, 83, 80, 0);
    (0x0A40, 84, 81, 0);
    (0x0861, 85, 82, 0);
    (0x0706, 86, 83, 0);
    (0x05CD, 87, 84, 0);
    (0x04DE, 88, 85, 0);
    (0x040F, 89, 86, 0);
    (0x0363, 90, 87, 0);
    (* Index 90-99 *)
    (0x02D4, 91, 88, 0);
    (0x025C, 92, 89, 0);
    (0x01F8, 93, 90, 0);
    (0x01A4, 94, 91, 0);
    (0x0160, 95, 92, 0);
    (0x0125, 96, 93, 0);
    (0x00F6, 97, 94, 0);
    (0x00CB, 98, 95, 0);
    (0x00AB, 99, 96, 0);
    (0x008F, 100, 97, 0);
    (* Index 100-109 *)
    (0x0068, 101, 98, 0);
    (0x004E, 102, 99, 0);
    (0x003B, 103, 100, 0);
    (0x002C, 103, 101, 0);
    (0x0019, 105, 102, 0);
    (0x0012, 106, 103, 0);
    (0x000D, 107, 104, 0);
    (0x0009, 108, 105, 0);
    (0x0006, 109, 106, 0);
    (0x0004, 110, 107, 0);
    (* Index 110-112 *)
    (0x0002, 111, 108, 0);
    (0x0001, 112, 109, 0);
    (0x0001, 112, 110, 0);
  |]

(** Get Qe value for state *)
let get_qe index =
  let qe, _, _, _ = qm_table.(index) in
  qe

(** Get next MPS state index *)
let get_nmps index =
  let _, nmps, _, _ = qm_table.(index) in
  nmps

(** Get next LPS state index *)
let get_nlps index =
  let _, _, nlps, _ = qm_table.(index) in
  nlps

(** Get switch flag (1 = exchange MPS sense after LPS) *)
let get_switch index =
  let _, _, _, switch = qm_table.(index) in
  switch

type context = {
  mutable index : int; (* State index in qm_table *)
  mutable mps : int; (* Most probable symbol: 0 or 1 *)
}
(** Binary context for arithmetic coding *)

(** Create a new context with initial state *)
let create_context () = { index = 0; mps = 0 }

(* Use 32-bit precision for the range coder *)
let precision = 32
let whole = 1 lsl precision (* 2^32 *)
let half = whole / 2 (* 2^31 *)
let quarter = whole / 4 (* 2^30 *)

type encoder_state = {
  mutable low : int;
  mutable high : int;
  mutable pending : int; (* Pending bits for underflow *)
  mutable output : int list; (* Output bits in reverse order *)
}
(** Arithmetic encoder state *)

type decoder_state = {
  mutable low : int;
  mutable high : int;
  mutable code : int; (* Current code value *)
  bits : int array;
  mutable bit_pos : int;
}
(** Arithmetic decoder state *)

(** Initialize encoder *)
let init_encoder () = { low = 0; high = whole - 1; pending = 0; output = [] }

(** Output a bit and any pending opposite bits *)
let output_bit_plus_pending (state : encoder_state) bit =
  state.output <- bit :: state.output;
  (* Output pending bits with opposite value *)
  for _ = 1 to state.pending do
    state.output <- (1 - bit) :: state.output
  done;
  state.pending <- 0

(** Renormalize encoder - expand interval when it gets small *)
let renorm_encoder (state : encoder_state) =
  while state.high - state.low < quarter do
    if state.high < half then begin
      (* Interval is in lower half [0, 0.5) *)
      output_bit_plus_pending state 0;
      state.low <- state.low * 2;
      state.high <- (state.high * 2) + 1
    end
    else if state.low >= half then begin
      (* Interval is in upper half [0.5, 1) *)
      output_bit_plus_pending state 1;
      state.low <- (state.low - half) * 2;
      state.high <- ((state.high - half) * 2) + 1
    end
    else begin
      (* Interval straddles the middle - underflow *)
      state.pending <- state.pending + 1;
      state.low <- (state.low - quarter) * 2;
      state.high <- ((state.high - quarter) * 2) + 1
    end
  done

(** Encode a single binary decision *)
let encode ctx (state : encoder_state) d =
  let qe = get_qe ctx.index in
  let range = state.high - state.low + 1 in
  (* Scale qe from 16-bit to our precision *)
  let lps_count = (range * qe) lsr 16 in
  let lps_count = max 1 lps_count in
  let split = state.low + lps_count in

  if d = ctx.mps then begin
    (* MPS: take upper sub-interval [split, high] *)
    state.low <- split;
    ctx.index <- get_nmps ctx.index
  end
  else begin
    (* LPS: take lower sub-interval [low, split-1] *)
    state.high <- split - 1;
    if get_switch ctx.index <> 0 then ctx.mps <- 1 - ctx.mps;
    ctx.index <- get_nlps ctx.index
  end;
  renorm_encoder state

(** Flush encoder - output final bits to uniquely identify interval *)
let flush_encoder (state : encoder_state) =
  (* Output two bits to narrow down to a unique point in the final interval *)
  state.pending <- state.pending + 1;
  if state.low < quarter then output_bit_plus_pending state 0
  else output_bit_plus_pending state 1;
  (* Convert bit list to bytes *)
  let bits = List.rev state.output in
  let bit_count = List.length bits in
  let byte_count = (bit_count + 7) / 8 in
  let result = Bytes.create byte_count in
  Bytes.fill result 0 byte_count (Char.chr 0);
  List.iteri
    (fun i bit ->
      if bit = 1 then begin
        let byte_idx = i / 8 in
        let bit_idx = 7 - (i mod 8) in
        let old_byte = Bytes.get_uint8 result byte_idx in
        Bytes.set_uint8 result byte_idx (old_byte lor (1 lsl bit_idx))
      end)
    bits;
  result

(** Read a bit from decoder input *)
let read_bit (state : decoder_state) =
  if state.bit_pos < Array.length state.bits then begin
    let bit = state.bits.(state.bit_pos) in
    state.bit_pos <- state.bit_pos + 1;
    bit
  end
  else 0 (* Pad with zeros *)

(** Initialize decoder from bytes *)
let init_decoder_bytes data =
  (* Convert bytes to bits *)
  let byte_len = Bytes.length data in
  let bits = Array.make ((byte_len * 8) + precision) 0 in
  for i = 0 to byte_len - 1 do
    let byte = Bytes.get_uint8 data i in
    for j = 0 to 7 do
      bits.((i * 8) + j) <- (byte lsr (7 - j)) land 1
    done
  done;
  let state = { low = 0; high = whole - 1; code = 0; bits; bit_pos = 0 } in
  (* Read initial bits to fill code register *)
  for _ = 1 to precision do
    state.code <- (state.code * 2) + read_bit state
  done;
  state

(** Initialize decoder from bitstream reader *)
let init_decoder reader =
  let data = reader.Bitstream.data in
  let pos = reader.Bitstream.pos in
  init_decoder_bytes (Bytes.sub data pos (Bytes.length data - pos))

(** Renormalize decoder - keep interval and code in sync *)
let renorm_decoder (state : decoder_state) =
  while state.high - state.low < quarter do
    if state.high < half then begin
      (* Lower half *)
      state.low <- state.low * 2;
      state.high <- (state.high * 2) + 1;
      state.code <- (state.code * 2) + read_bit state
    end
    else if state.low >= half then begin
      (* Upper half *)
      state.low <- (state.low - half) * 2;
      state.high <- ((state.high - half) * 2) + 1;
      state.code <- ((state.code - half) * 2) + read_bit state
    end
    else begin
      (* Middle - underflow *)
      state.low <- (state.low - quarter) * 2;
      state.high <- ((state.high - quarter) * 2) + 1;
      state.code <- ((state.code - quarter) * 2) + read_bit state
    end
  done

(** Decode a single binary decision *)
let decode ctx (state : decoder_state) =
  let qe = get_qe ctx.index in
  let range = state.high - state.low + 1 in
  let lps_count = (range * qe) lsr 16 in
  let lps_count = max 1 lps_count in
  let split = state.low + lps_count in

  let d =
    if state.code < split then begin
      (* Code is in LPS interval [low, split-1] *)
      state.high <- split - 1;
      let result = 1 - ctx.mps in
      if get_switch ctx.index <> 0 then ctx.mps <- 1 - ctx.mps;
      ctx.index <- get_nlps ctx.index;
      result
    end
    else begin
      (* Code is in MPS interval [split, high] *)
      state.low <- split;
      ctx.index <- get_nmps ctx.index;
      ctx.mps
    end
  in
  renorm_decoder state;
  d

(** Context sets for JPEG arithmetic coding *)

(** DC contexts: indexed by component (0=luma, 1=chroma) and classification *)
let create_dc_contexts () = Array.init 10 (fun _ -> create_context ())

(** AC contexts *)
let create_ac_contexts () = Array.init 252 (fun _ -> create_context ())

(** DC context selection *)
let select_dc_context contexts component_class diff_magnitude =
  let base = component_class * 5 in
  let bin =
    if diff_magnitude = 0 then 0
    else if diff_magnitude <= 1 then 1
    else if diff_magnitude <= 3 then 2
    else if diff_magnitude <= 7 then 3
    else 4
  in
  contexts.(base + bin)

(** AC context selection *)
let select_ac_context contexts component_class k se =
  let base = component_class * 126 in
  let offset =
    if k <= 5 then (k - 1) * 2
    else if k <= 14 then 10 + ((k - 6) * 2)
    else 28 + min (k - 15) 29
  in
  let bin = offset + if se >= k then 1 else 0 in
  contexts.(base + min bin 125)

(** Encode DC difference *)
let encode_dc_diff state contexts component_class diff =
  let mag = abs diff in
  let sign_ctx = select_dc_context contexts component_class 0 in
  if mag = 0 then encode sign_ctx state 0
  else begin
    encode sign_ctx state 1;
    let sign_bit = if diff < 0 then 1 else 0 in
    let sign_ctx2 = select_dc_context contexts component_class 1 in
    encode sign_ctx2 state sign_bit;
    let rec encode_mag m ctx_mag =
      if m > 1 then begin
        let ctx = select_dc_context contexts component_class ctx_mag in
        encode ctx state 1;
        encode_mag (m - 1) (min (ctx_mag + 1) 4)
      end
      else begin
        let ctx = select_dc_context contexts component_class ctx_mag in
        encode ctx state 0
      end
    in
    encode_mag mag 2
  end

(** Decode DC difference *)
let decode_dc_diff state contexts component_class =
  let sign_ctx = select_dc_context contexts component_class 0 in
  let is_nonzero = decode sign_ctx state in
  if is_nonzero = 0 then 0
  else begin
    let sign_ctx2 = select_dc_context contexts component_class 1 in
    let sign = decode sign_ctx2 state in
    let rec decode_mag acc ctx_mag =
      let ctx = select_dc_context contexts component_class ctx_mag in
      let bit = decode ctx state in
      if bit = 1 then decode_mag (acc + 1) (min (ctx_mag + 1) 4) else acc
    in
    let mag = decode_mag 1 2 in
    if sign = 1 then -mag else mag
  end

(** Encode AC coefficients *)
let encode_ac_coeffs state contexts component_class coeffs =
  let se = ref 63 in
  while !se > 0 && coeffs.(!se) = 0 do
    decr se
  done;
  for k = 1 to 63 do
    let coeff = coeffs.(k) in
    let ctx = select_ac_context contexts component_class k !se in
    if coeff = 0 then encode ctx state 0
    else begin
      encode ctx state 1;
      let sign = if coeff < 0 then 1 else 0 in
      let sign_ctx = select_ac_context contexts component_class k !se in
      encode sign_ctx state sign;
      let mag = abs coeff in
      let rec encode_mag m =
        if m > 1 then begin
          encode ctx state 1;
          encode_mag (m - 1)
        end
        else encode ctx state 0
      in
      encode_mag mag
    end
  done

(** Decode AC coefficients *)
let decode_ac_coeffs state contexts component_class =
  let coeffs = Array.make 64 0 in
  let se = ref 0 in
  for k = 1 to 63 do
    let ctx = select_ac_context contexts component_class k !se in
    let is_nonzero = decode ctx state in
    if is_nonzero = 1 then begin
      se := k;
      let sign_ctx = select_ac_context contexts component_class k !se in
      let sign = decode sign_ctx state in
      let rec decode_mag acc =
        let bit = decode ctx state in
        if bit = 1 then decode_mag (acc + 1) else acc
      in
      let mag = decode_mag 1 in
      coeffs.(k) <- (if sign = 1 then -mag else mag)
    end
  done;
  coeffs
