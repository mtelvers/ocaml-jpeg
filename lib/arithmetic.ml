(** Arithmetic coding for JPEG (MQ-Coder per ITU-T T.81 / ISO 10918-1)

    The MQ-coder is a binary arithmetic coder used in JPEG arithmetic mode. It
    uses adaptive probability estimation with 113 states and operates on 16-bit
    precision intervals. *)

(** QM-Coder probability state table (from ITU-T T.81 Table D.3). Each entry is
    (Qe value, NMPS index, NLPS index, switch flag). Qe is the probability
    estimate for the LPS (less probable symbol) scaled to 16 bits. *)
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

(** Create a context with a specific initial index (for conditioning) *)
let create_context_with_index idx = { index = idx; mps = 0 }

(* ============================================================================
   JPEG MQ-Coder Decoder (ITU-T T.81 Annex D)

   Uses 16-bit precision with:
   - A: interval size (0x8000 to 0xFFFF after normalization)
   - C: code register (32 bits, upper 16 bits are active)
   - CT: bit counter for renormalization
   ============================================================================ *)

type jpeg_decoder_state = {
  mutable a : int; (* Interval register, 16-bit *)
  mutable c : int; (* Code register, 32-bit (using lower bits) *)
  mutable ct : int; (* Bit counter *)
  mutable data : bytes; (* Input data *)
  mutable pos : int; (* Current byte position *)
  mutable buffer : int; (* Current byte buffer *)
  mutable next_byte : int; (* Next byte (for marker detection) *)
}
(** JPEG arithmetic decoder state per ITU-T T.81 *)

(** Read a byte from input, handling byte stuffing (FF00 -> FF) *)
let read_byte_stuffed state =
  if state.pos >= Bytes.length state.data then 0xFF (* Pad with 0xFF at end *)
  else begin
    let b = Bytes.get_uint8 state.data state.pos in
    state.pos <- state.pos + 1;
    (* Handle byte stuffing: after 0xFF, skip the 0x00 *)
    if b = 0xFF && state.pos < Bytes.length state.data then begin
      let next = Bytes.get_uint8 state.data state.pos in
      if next = 0x00 then state.pos <- state.pos + 1
      (* Skip stuffed byte *)
      (* If next is a marker (not 0x00), we've hit the end of entropy data *)
    end;
    b
  end

(** Initialize the JPEG arithmetic decoder (INITDEC procedure from T.81 D.2.6)
*)
let init_jpeg_decoder data =
  let state =
    { a = 0; c = 0; ct = 0; data; pos = 0; buffer = 0; next_byte = 0 }
  in

  (* Read first two bytes into C register *)
  let b1 = read_byte_stuffed state in
  let b2 = read_byte_stuffed state in

  (* C = (B1 << 8) | B2, shifted left by 16 for 32-bit register *)
  state.c <- ((b1 lsl 8) lor b2) lsl 16;

  (* Initialize A to 0x10000 (will be normalized) *)
  state.a <- 0x10000;

  (* CT = 0 initially, first renormalization will set it up *)
  state.ct <- 0;

  (* Perform initial renormalization to get A into proper range *)
  (* Actually per spec, we need to do BYTEIN first *)
  state.ct <- 16;

  (* We've already read 16 bits *)
  state

(** Byte-in procedure (BYTEIN from T.81 D.2.7) - shift in new byte when needed
*)
let bytein state =
  if state.pos < Bytes.length state.data then begin
    let b = Bytes.get_uint8 state.data state.pos in
    state.pos <- state.pos + 1;

    if state.buffer = 0xFF then begin
      (* Previous byte was FF - check for stuffing or marker *)
      if b = 0x00 then begin
        (* Stuffed byte: use 0xFF, b becomes part of next byte *)
        state.c <- state.c + (0xFF00 lsl (8 - state.ct));
        state.ct <- state.ct + 8
      end
      else begin
        (* Marker detected - don't consume, just pad with zeros *)
        state.pos <- state.pos - 1;
        (* Put back the marker byte *)
        state.c <- state.c + (0xFF00 lsl (8 - state.ct));
        state.ct <- state.ct + 8
      end
    end
    else begin
      state.c <- state.c + (b lsl (8 - state.ct));
      state.ct <- state.ct + 8
    end;
    state.buffer <- b
  end
  else begin
    (* End of data - pad with 0xFF *)
    state.c <- state.c + (0xFF lsl (8 - state.ct));
    state.ct <- state.ct + 8;
    state.buffer <- 0xFF
  end

(** Renormalize decoder (RENORMD from T.81 D.2.5) *)
let renormd state =
  while state.a < 0x8000 do
    if state.ct = 0 then bytein state;
    state.a <- state.a lsl 1;
    state.c <- state.c lsl 1;
    state.ct <- state.ct - 1
  done;
  (* Keep C in 32-bit range *)
  state.c <- state.c land 0xFFFFFFFF

(** Decode a single binary decision (DECODE from T.81 D.2.4) *)
let decode_decision ctx state =
  let qe = get_qe ctx.index in

  (* A = A - Qe *)
  state.a <- state.a - qe;

  (* Chigh = C >> 16 (upper 16 bits of C) *)
  let chigh = state.c lsr 16 in

  let d =
    if chigh < state.a then begin
      (* MPS path *)
      if state.a < 0x8000 then begin
        (* Conditional exchange *)
        if state.a < qe then begin
          (* LPS and MPS exchange *)
          let result = 1 - ctx.mps in
          if get_switch ctx.index <> 0 then ctx.mps <- 1 - ctx.mps;
          ctx.index <- get_nlps ctx.index;
          result
        end
        else begin
          ctx.index <- get_nmps ctx.index;
          ctx.mps
        end
      end
      else begin
        (* No renormalization needed, definitely MPS *)
        ctx.mps
      end
    end
    else begin
      (* LPS path: C >= A *)
      (* C = C - (A << 16) *)
      state.c <- state.c - (state.a lsl 16);

      if state.a < qe then begin
        (* Conditional exchange - this is actually MPS *)
        state.a <- qe;
        ctx.index <- get_nmps ctx.index;
        ctx.mps
      end
      else begin
        (* Normal LPS *)
        state.a <- qe;
        let result = 1 - ctx.mps in
        if get_switch ctx.index <> 0 then ctx.mps <- 1 - ctx.mps;
        ctx.index <- get_nlps ctx.index;
        result
      end
    end
  in

  (* Renormalize if needed *)
  if state.a < 0x8000 then renormd state;

  d

(** Initialize decoder from bitstream reader position *)
let init_jpeg_decoder_from_reader reader =
  let data = reader.Bitstream.data in
  let pos = reader.Bitstream.pos in
  let remaining = Bytes.sub data pos (Bytes.length data - pos) in
  init_jpeg_decoder remaining

(* ============================================================================
   JPEG DC/AC Coefficient Decoding (ITU-T T.81 Annex F.1.4)

   DC and AC coefficients use specific coding procedures with multiple
   context bins based on previous values and coefficient positions.
   ============================================================================ *)

type dc_stat_bins = {
  dc_s0 : context; (* Zero/non-zero decision *)
  dc_sign : context; (* Sign decision *)
  dc_sp : context array; (* Positive magnitude bins (5 bins) *)
  dc_sn : context array; (* Negative magnitude bins (5 bins) *)
  dc_x1 : context; (* LSB extension *)
  dc_x2 : context; (* V continuation *)
}
(** Statistical area for DC (per component, based on conditioning value L) *)

(** Create DC statistical bins for one component *)
let create_dc_stat_bins () =
  {
    dc_s0 = create_context ();
    dc_sign = create_context ();
    dc_sp = Array.init 5 (fun _ -> create_context ());
    dc_sn = Array.init 5 (fun _ -> create_context ());
    dc_x1 = create_context ();
    dc_x2 = create_context ();
  }

type ac_stat_bins = {
  ac_se : context array; (* EOB decision bins (63 bins for k=1..63) *)
  ac_s0 : context array; (* Zero/non-zero (63 bins) *)
  ac_sign : context array; (* Sign (63 bins) *)
  ac_sp : context array; (* Positive magnitude *)
  ac_sn : context array; (* Negative magnitude *)
  ac_x1 : context array; (* LSB extension *)
  ac_x2 : context array; (* V extension *)
}
(** Statistical bins for AC coefficients *)

(** Create AC statistical bins for one component *)
let create_ac_stat_bins () =
  {
    ac_se = Array.init 63 (fun _ -> create_context ());
    ac_s0 = Array.init 63 (fun _ -> create_context ());
    ac_sign = Array.init 63 (fun _ -> create_context ());
    ac_sp = Array.init 63 (fun _ -> create_context ());
    ac_sn = Array.init 63 (fun _ -> create_context ());
    ac_x1 = Array.init 63 (fun _ -> create_context ());
    ac_x2 = Array.init 63 (fun _ -> create_context ());
  }

(** Conditioning value L bounds the context bin selection. Default L=0 means use
    standard context selection. *)
let dc_context_bin l diff =
  if l = 0 then
    (* Standard: 5 bins based on magnitude *)
    min 4 (abs diff)
  else if
    (* With conditioning: bins based on L threshold *)
    abs diff <= l
  then 0
  else min 4 (((abs diff - l - 1) / (l + 1)) + 1)

(** Decode DC difference value (ITU-T T.81 F.1.4.4.1) *)
let decode_dc_diff state bins prev_diff l =
  (* Select context bin based on previous difference *)
  let _ctx_bin = dc_context_bin l prev_diff in

  (* S0: Is the difference zero? *)
  let is_zero = decode_decision bins.dc_s0 state in
  if is_zero = 0 then 0
  else begin
    (* Sign bit *)
    let sign = decode_decision bins.dc_sign state in
    let sign_ctx = if sign = 0 then bins.dc_sp else bins.dc_sn in

    (* Decode magnitude using categories *)
    (* First, decode the category (number of bits needed) *)
    let rec decode_category sz =
      if sz >= 15 then sz (* Max category *)
      else begin
        let continue = decode_decision sign_ctx.(min sz 4) state in
        if continue = 0 then sz else decode_category (sz + 1)
      end
    in
    let category = decode_category 1 in

    (* Decode the magnitude bits *)
    let magnitude =
      if category <= 1 then 1
      else begin
        (* First bit is always 1 (implicit) *)
        let rec decode_bits acc remaining =
          if remaining <= 0 then acc
          else begin
            let bit = decode_decision bins.dc_x1 state in
            decode_bits ((acc lsl 1) lor bit) (remaining - 1)
          end
        in
        (* Decode category-1 additional bits *)
        let extra_bits = decode_bits 0 (category - 1) in
        (1 lsl (category - 1)) + extra_bits
      end
    in

    if sign = 0 then magnitude else -magnitude
  end

(** Decode AC coefficients for a block (ITU-T T.81 F.1.4.4.2) *)
let decode_ac_block state bins kx =
  let coeffs = Array.make 64 0 in
  let k = ref 1 in
  (* Start at position 1 (DC is position 0) *)

  while !k <= 63 do
    let ki = !k - 1 in
    (* 0-based index for bins *)

    (* SE: End of block decision *)
    let eob = decode_decision bins.ac_se.(ki) state in
    if eob = 1 then k := 64 (* Exit loop - rest are zeros *)
    else begin
      (* S0: Is this coefficient zero? *)
      let is_zero = decode_decision bins.ac_s0.(ki) state in
      if is_zero = 0 then begin
        (* Zero coefficient, move to next *)
        incr k
      end
      else begin
        (* Non-zero coefficient *)
        (* Sign bit *)
        let sign = decode_decision bins.ac_sign.(ki) state in
        let mag_ctx = if sign = 0 then bins.ac_sp.(ki) else bins.ac_sn.(ki) in

        (* Decode magnitude category *)
        let rec decode_category sz =
          if sz >= 15 then sz
          else if sz > kx then sz (* Kx limits max category *)
          else begin
            let continue = decode_decision mag_ctx state in
            if continue = 0 then sz else decode_category (sz + 1)
          end
        in
        let category = decode_category 1 in

        (* Decode magnitude bits *)
        let magnitude =
          if category <= 1 then 1
          else begin
            let rec decode_bits acc remaining =
              if remaining <= 0 then acc
              else begin
                let bit = decode_decision bins.ac_x1.(ki) state in
                decode_bits ((acc lsl 1) lor bit) (remaining - 1)
              end
            in
            let extra = decode_bits 0 (category - 1) in
            (1 lsl (category - 1)) + extra
          end
        in

        coeffs.(!k) <- (if sign = 0 then magnitude else -magnitude);
        incr k
      end
    end
  done;

  coeffs

(* ============================================================================
   Full JPEG Arithmetic Scan Decoder
   ============================================================================ *)

type arith_scan_state = {
  decoder : jpeg_decoder_state;
  dc_bins : dc_stat_bins array; (* Per component *)
  ac_bins : ac_stat_bins array; (* Per component *)
  prev_dc : int array; (* Previous DC values per component *)
  l : int array; (* DC conditioning values per component *)
  kx : int array; (* AC conditioning values per component *)
}
(** Arithmetic decoding state for a full scan *)

(** Initialize arithmetic scan decoder *)
let init_arith_scan_decoder data num_components =
  {
    decoder = init_jpeg_decoder data;
    dc_bins = Array.init num_components (fun _ -> create_dc_stat_bins ());
    ac_bins = Array.init num_components (fun _ -> create_ac_stat_bins ());
    prev_dc = Array.make num_components 0;
    l = Array.make num_components 0;
    (* Default conditioning *)
    kx = Array.make num_components 5;
    (* Default Kx = 5 *)
  }

(** Set conditioning values from DAC markers *)
let set_conditioning state component_idx is_dc value =
  if is_dc then state.l.(component_idx) <- value
  else state.kx.(component_idx) <- value

(** Decode a single 8x8 block with arithmetic coding *)
let decode_arith_block state component_idx =
  let dc_bins = state.dc_bins.(component_idx) in
  let ac_bins = state.ac_bins.(component_idx) in
  let l = state.l.(component_idx) in
  let kx = state.kx.(component_idx) in

  (* Decode DC difference *)
  let prev_dc = state.prev_dc.(component_idx) in
  let dc_diff = decode_dc_diff state.decoder dc_bins prev_dc l in
  let dc_value = prev_dc + dc_diff in
  state.prev_dc.(component_idx) <- dc_value;

  (* Decode AC coefficients *)
  let coeffs = decode_ac_block state.decoder ac_bins kx in
  coeffs.(0) <- dc_value;

  coeffs

(** Reset decoder state at restart marker *)
let reset_arith_decoder state =
  (* Reset DC predictors *)
  Array.fill state.prev_dc 0 (Array.length state.prev_dc) 0;
  (* Reset statistical bins to initial state *)
  Array.iter
    (fun bins ->
      bins.dc_s0.index <- 0;
      bins.dc_s0.mps <- 0;
      bins.dc_sign.index <- 0;
      bins.dc_sign.mps <- 0;
      Array.iter
        (fun c ->
          c.index <- 0;
          c.mps <- 0)
        bins.dc_sp;
      Array.iter
        (fun c ->
          c.index <- 0;
          c.mps <- 0)
        bins.dc_sn;
      bins.dc_x1.index <- 0;
      bins.dc_x1.mps <- 0;
      bins.dc_x2.index <- 0;
      bins.dc_x2.mps <- 0)
    state.dc_bins;
  Array.iter
    (fun bins ->
      Array.iter
        (fun c ->
          c.index <- 0;
          c.mps <- 0)
        bins.ac_se;
      Array.iter
        (fun c ->
          c.index <- 0;
          c.mps <- 0)
        bins.ac_s0;
      Array.iter
        (fun c ->
          c.index <- 0;
          c.mps <- 0)
        bins.ac_sign;
      Array.iter
        (fun c ->
          c.index <- 0;
          c.mps <- 0)
        bins.ac_sp;
      Array.iter
        (fun c ->
          c.index <- 0;
          c.mps <- 0)
        bins.ac_sn;
      Array.iter
        (fun c ->
          c.index <- 0;
          c.mps <- 0)
        bins.ac_x1;
      Array.iter
        (fun c ->
          c.index <- 0;
          c.mps <- 0)
        bins.ac_x2)
    state.ac_bins

(* ============================================================================
   JPEG MQ-Coder Encoder (ITU-T T.81 Annex D)

   Implements the arithmetic encoder for JPEG with proper byte stuffing.
   Uses 16-bit precision intervals matching the decoder.
   ============================================================================ *)

type jpeg_encoder_state = {
  mutable a : int; (* Interval register, 16-bit *)
  mutable c : int; (* Code register, 32-bit *)
  mutable ct : int; (* Bit counter for byte output *)
  mutable st : int; (* Stack count for 0xFF bytes *)
  mutable bp : int; (* Last byte output position *)
  mutable buffer : int; (* Byte buffer for output *)
  mutable output : bytes; (* Output buffer *)
  mutable output_pos : int; (* Current output position *)
  mutable output_cap : int; (* Output buffer capacity *)
}
(** JPEG arithmetic encoder state per ITU-T T.81 *)

(** Grow output buffer if needed *)
let ensure_capacity state needed =
  if state.output_pos + needed > state.output_cap then begin
    let new_cap = max (state.output_cap * 2) (state.output_pos + needed) in
    let new_buf = Bytes.create new_cap in
    Bytes.blit state.output 0 new_buf 0 state.output_pos;
    state.output <- new_buf;
    state.output_cap <- new_cap
  end

(** Output a byte with byte stuffing (BYTEOUT procedure from T.81 D.1.5) *)
let byteout state =
  ensure_capacity state 2;
  let temp = state.c lsr 19 in
  (* Get high 8 bits of C after shifting *)
  if temp > 0xFF then begin
    (* Carry occurred - propagate carry through stacked 0xFF bytes *)
    if state.bp >= 0 then begin
      Bytes.set_uint8 state.output state.bp
        (Bytes.get_uint8 state.output state.bp + 1)
    end;
    while state.st > 0 do
      ensure_capacity state 1;
      Bytes.set_uint8 state.output state.output_pos 0x00;
      state.output_pos <- state.output_pos + 1;
      state.st <- state.st - 1
    done;
    state.buffer <- temp land 0xFF
  end
  else if temp = 0xFF then begin
    (* Stack the 0xFF to handle potential carry later *)
    state.st <- state.st + 1
  end
  else begin
    (* Normal byte - output any stacked 0xFF bytes with stuffing *)
    if state.buffer >= 0 then begin
      ensure_capacity state 1;
      Bytes.set_uint8 state.output state.output_pos state.buffer;
      state.bp <- state.output_pos;
      state.output_pos <- state.output_pos + 1
    end;
    while state.st > 0 do
      ensure_capacity state 2;
      Bytes.set_uint8 state.output state.output_pos 0xFF;
      state.output_pos <- state.output_pos + 1;
      Bytes.set_uint8 state.output state.output_pos 0x00;
      (* Byte stuffing *)
      state.output_pos <- state.output_pos + 1;
      state.st <- state.st - 1
    done;
    state.buffer <- temp
  end;
  state.c <- (state.c land 0x7FFFF) lsl 8;
  (* Keep low 19 bits, shift left 8 *)
  state.ct <- 8

(** Initialize the JPEG arithmetic encoder (INITENC from T.81 D.1.4) *)
let init_jpeg_encoder () =
  let initial_cap = 4096 in
  {
    a = 0x10000;
    (* Full interval *)
    c = 0;
    (* Code register *)
    ct = 11;
    (* Counter - first byte will have 3 spacer bits *)
    st = 0;
    (* No stacked 0xFF bytes *)
    bp = -1;
    (* No bytes output yet *)
    buffer = -1;
    (* No buffered byte *)
    output = Bytes.create initial_cap;
    output_pos = 0;
    output_cap = initial_cap;
  }

(** Renormalize encoder after encoding (RENORME from T.81 D.1.5) *)
let renorme state =
  while state.a < 0x8000 do
    state.a <- state.a lsl 1;
    state.c <- state.c lsl 1;
    state.ct <- state.ct - 1;
    if state.ct = 0 then byteout state
  done

(** Encode a single binary decision (ENCODE from T.81 D.1.6) *)
let encode_decision ctx state d =
  let qe = get_qe ctx.index in

  (* A = A - Qe *)
  state.a <- state.a - qe;

  if d = ctx.mps then begin
    (* MPS: C unchanged, A = A (already subtracted Qe, need to add back if no renorm) *)
    if state.a < 0x8000 then begin
      if state.a < qe then begin
        (* Conditional exchange - this is actually LPS *)
        state.a <- qe
      end;
      ctx.index <- get_nmps ctx.index
    end
    else begin
      (* No renormalization needed, restore A *)
      state.a <- state.a + qe
    end
  end
  else begin
    (* LPS: C = C + A *)
    state.c <- state.c + state.a;
    if state.a >= qe then begin
      (* Normal LPS *)
      state.a <- qe
    end;
    (* Conditional exchange handled by state machine *)
    if get_switch ctx.index <> 0 then ctx.mps <- 1 - ctx.mps;
    ctx.index <- get_nlps ctx.index
  end;

  (* Renormalize if needed *)
  if state.a < 0x8000 then renorme state

(** Flush the encoder (FLUSH from T.81 D.1.7) *)
let flush_jpeg_encoder state =
  (* Clear final bits *)
  let temp = (state.c + state.a - 1) land 0xFFFF0000 in
  let c =
    if temp < state.c then state.c (* Would cause carry, use C *) else temp
  in
  state.c <- c;

  (* Output remaining bytes *)
  state.c <- state.c lsl state.ct;
  byteout state;
  state.c <- state.c lsl 8;
  byteout state;

  (* Output any remaining buffered byte *)
  if state.buffer >= 0 then begin
    ensure_capacity state 1;
    Bytes.set_uint8 state.output state.output_pos state.buffer;
    state.output_pos <- state.output_pos + 1;
    (* Add stuffing if it was 0xFF *)
    if state.buffer = 0xFF then begin
      ensure_capacity state 1;
      Bytes.set_uint8 state.output state.output_pos 0x00;
      state.output_pos <- state.output_pos + 1
    end
  end;

  (* Output any stacked 0xFF bytes *)
  while state.st > 0 do
    ensure_capacity state 2;
    Bytes.set_uint8 state.output state.output_pos 0xFF;
    state.output_pos <- state.output_pos + 1;
    Bytes.set_uint8 state.output state.output_pos 0x00;
    state.output_pos <- state.output_pos + 1;
    state.st <- state.st - 1
  done;

  (* Return the output bytes *)
  Bytes.sub state.output 0 state.output_pos

(** Encode DC difference value (ITU-T T.81 F.1.4.4.1) *)
let encode_dc_diff state bins diff _l =
  let mag = abs diff in

  (* S0: Is the difference zero? *)
  if mag = 0 then encode_decision bins.dc_s0 state 0
  else begin
    encode_decision bins.dc_s0 state 1;

    (* Sign bit: 0 for positive, 1 for negative *)
    let sign = if diff < 0 then 1 else 0 in
    encode_decision bins.dc_sign state sign;

    let sign_ctx = if sign = 0 then bins.dc_sp else bins.dc_sn in

    (* Encode magnitude using categories *)
    (* First, determine category (number of bits needed) *)
    let rec find_category m cat =
      if m <= 1 then cat else find_category (m lsr 1) (cat + 1)
    in
    let category = if mag <= 1 then 1 else find_category mag 0 in

    (* Encode category using unary coding *)
    for i = 1 to category - 1 do
      encode_decision sign_ctx.(min i 4) state 1
    done;
    encode_decision sign_ctx.(min category 4) state 0;

    (* Encode the magnitude bits if category > 1 *)
    if category > 1 then begin
      (* Encode category-1 bits of magnitude (MSB to LSB) *)
      for i = category - 2 downto 0 do
        let bit = (mag lsr i) land 1 in
        encode_decision bins.dc_x1 state bit
      done
    end
  end

(** Encode AC coefficients for a block (ITU-T T.81 F.1.4.4.2) *)
let encode_ac_block state bins coeffs kx =
  (* Find the last non-zero coefficient (end of block) *)
  let se = ref 0 in
  for k = 1 to 63 do
    if coeffs.(k) <> 0 then se := k
  done;

  let k = ref 1 in
  while !k <= 63 do
    let ki = !k - 1 in
    (* 0-based index for bins *)

    if !k > !se then begin
      (* SE: End of block - signal EOB *)
      encode_decision bins.ac_se.(ki) state 1;
      k := 64 (* Exit loop *)
    end
    else begin
      (* SE: Not end of block *)
      encode_decision bins.ac_se.(ki) state 0;

      let coeff = coeffs.(!k) in
      if coeff = 0 then begin
        (* S0: Zero coefficient *)
        encode_decision bins.ac_s0.(ki) state 0;
        incr k
      end
      else begin
        (* S0: Non-zero coefficient *)
        encode_decision bins.ac_s0.(ki) state 1;

        (* Sign bit *)
        let sign = if coeff < 0 then 1 else 0 in
        encode_decision bins.ac_sign.(ki) state sign;

        let mag_ctx = if sign = 0 then bins.ac_sp.(ki) else bins.ac_sn.(ki) in
        let mag = abs coeff in

        (* Encode magnitude category *)
        let rec find_category m cat =
          if m <= 1 then cat else find_category (m lsr 1) (cat + 1)
        in
        let category = if mag <= 1 then 1 else find_category mag 0 in
        let category = min category (max 15 kx) in

        (* Encode category using unary coding *)
        for _ = 1 to category - 1 do
          encode_decision mag_ctx state 1
        done;
        encode_decision mag_ctx state 0;

        (* Encode magnitude bits if category > 1 *)
        if category > 1 then begin
          for i = category - 2 downto 0 do
            let bit = (mag lsr i) land 1 in
            encode_decision bins.ac_x1.(ki) state bit
          done
        end;

        incr k
      end
    end
  done

(* ============================================================================
   Full JPEG Arithmetic Scan Encoder
   ============================================================================ *)

type arith_encode_scan_state = {
  encoder : jpeg_encoder_state;
  dc_bins : dc_stat_bins array; (* Per component *)
  ac_bins : ac_stat_bins array; (* Per component *)
  prev_dc : int array; (* Previous DC values per component *)
  l : int array; (* DC conditioning values per component *)
  kx : int array; (* AC conditioning values per component *)
}
(** Arithmetic encoding state for a full scan *)

(** Initialize arithmetic scan encoder *)
let init_arith_scan_encoder num_components =
  {
    encoder = init_jpeg_encoder ();
    dc_bins = Array.init num_components (fun _ -> create_dc_stat_bins ());
    ac_bins = Array.init num_components (fun _ -> create_ac_stat_bins ());
    prev_dc = Array.make num_components 0;
    l = Array.make num_components 0;
    (* Default conditioning *)
    kx = Array.make num_components 5;
    (* Default Kx = 5 *)
  }

(** Set encoder conditioning values *)
let set_encoder_conditioning state component_idx is_dc value =
  if is_dc then state.l.(component_idx) <- value
  else state.kx.(component_idx) <- value

(** Encode a single 8x8 block with arithmetic coding *)
let encode_arith_block state component_idx coeffs =
  let dc_bins = state.dc_bins.(component_idx) in
  let ac_bins = state.ac_bins.(component_idx) in
  let l = state.l.(component_idx) in
  let kx = state.kx.(component_idx) in

  (* Encode DC difference *)
  let prev_dc = state.prev_dc.(component_idx) in
  let dc_diff = coeffs.(0) - prev_dc in
  encode_dc_diff state.encoder dc_bins dc_diff l;
  state.prev_dc.(component_idx) <- coeffs.(0);

  (* Encode AC coefficients *)
  encode_ac_block state.encoder ac_bins coeffs kx

(** Reset encoder state at restart marker *)
let reset_arith_encoder state =
  (* Reset DC predictors *)
  Array.fill state.prev_dc 0 (Array.length state.prev_dc) 0;
  (* Reset statistical bins to initial state *)
  Array.iter
    (fun bins ->
      bins.dc_s0.index <- 0;
      bins.dc_s0.mps <- 0;
      bins.dc_sign.index <- 0;
      bins.dc_sign.mps <- 0;
      Array.iter
        (fun c ->
          c.index <- 0;
          c.mps <- 0)
        bins.dc_sp;
      Array.iter
        (fun c ->
          c.index <- 0;
          c.mps <- 0)
        bins.dc_sn;
      bins.dc_x1.index <- 0;
      bins.dc_x1.mps <- 0;
      bins.dc_x2.index <- 0;
      bins.dc_x2.mps <- 0)
    state.dc_bins;
  Array.iter
    (fun bins ->
      Array.iter
        (fun c ->
          c.index <- 0;
          c.mps <- 0)
        bins.ac_se;
      Array.iter
        (fun c ->
          c.index <- 0;
          c.mps <- 0)
        bins.ac_s0;
      Array.iter
        (fun c ->
          c.index <- 0;
          c.mps <- 0)
        bins.ac_sign;
      Array.iter
        (fun c ->
          c.index <- 0;
          c.mps <- 0)
        bins.ac_sp;
      Array.iter
        (fun c ->
          c.index <- 0;
          c.mps <- 0)
        bins.ac_sn;
      Array.iter
        (fun c ->
          c.index <- 0;
          c.mps <- 0)
        bins.ac_x1;
      Array.iter
        (fun c ->
          c.index <- 0;
          c.mps <- 0)
        bins.ac_x2)
    state.ac_bins;
  (* Re-initialize the encoder for the new segment *)
  state.encoder.a <- 0x10000;
  state.encoder.c <- 0;
  state.encoder.ct <- 11;
  state.encoder.st <- 0;
  state.encoder.buffer <- -1

(** Flush encoder and return scan data *)
let finish_arith_encoder state = flush_jpeg_encoder state.encoder

(* ============================================================================
   Legacy encoder interface (kept for compatibility)
   ============================================================================ *)

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
(** Arithmetic decoder state (legacy) *)

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

(** Decode DC difference (legacy interface) *)
let decode_dc_diff_legacy state contexts component_class =
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

(** Decode AC coefficients (legacy interface) *)
let decode_ac_coeffs_legacy state contexts component_class =
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
