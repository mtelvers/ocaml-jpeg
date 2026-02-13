(** Arithmetic coding for JPEG (MQ-Coder per ITU-T T.81 / ISO 10918-1)

    The MQ-coder is a binary arithmetic coder used in JPEG arithmetic mode. It
    uses adaptive probability estimation with 113 states and operates on 16-bit
    precision intervals. *)

(** QM-Coder probability state table (from ITU-T T.81 Table D.3). Each entry is
    (Qe value, NMPS index, NLPS index, switch flag). Qe is the probability
    estimate for the LPS (less probable symbol) scaled to 16 bits. *)
let qm_table =
  [|
    (* ITU-T T.81 Table D.3 / ISO 10918-1, matching libjpeg-turbo jaricom.c *)
    (* Format: (Qe, NMPS, NLPS, Switch) *)
    (* Index 0-9 *)
    (0x5A1D,   1,   1, 1);
    (0x2586,   2,  14, 0);
    (0x1114,   3,  16, 0);
    (0x080B,   4,  18, 0);
    (0x03D8,   5,  20, 0);
    (0x01DA,   6,  23, 0);
    (0x00E5,   7,  25, 0);
    (0x006F,   8,  28, 0);
    (0x0036,   9,  30, 0);
    (0x001A,  10,  33, 0);
    (* Index 10-19 *)
    (0x000D,  11,  35, 0);
    (0x0006,  12,   9, 0);
    (0x0003,  13,  10, 0);
    (0x0001,  13,  12, 0);
    (0x5A7F,  15,  15, 1);
    (0x3F25,  16,  36, 0);
    (0x2CF2,  17,  38, 0);
    (0x207C,  18,  39, 0);
    (0x17B9,  19,  40, 0);
    (0x1182,  20,  42, 0);
    (* Index 20-29 *)
    (0x0CEF,  21,  43, 0);
    (0x09A1,  22,  45, 0);
    (0x072F,  23,  46, 0);
    (0x055C,  24,  48, 0);
    (0x0406,  25,  49, 0);
    (0x0303,  26,  51, 0);
    (0x0240,  27,  52, 0);
    (0x01B1,  28,  54, 0);
    (0x0144,  29,  56, 0);
    (0x00F5,  30,  57, 0);
    (* Index 30-39 *)
    (0x00B7,  31,  59, 0);
    (0x008A,  32,  60, 0);
    (0x0068,  33,  62, 0);
    (0x004E,  34,  63, 0);
    (0x003B,  35,  32, 0);
    (0x002C,   9,  33, 0);
    (0x5AE1,  37,  37, 1);
    (0x484C,  38,  64, 0);
    (0x3A0D,  39,  65, 0);
    (0x2EF1,  40,  67, 0);
    (* Index 40-49 *)
    (0x261F,  41,  68, 0);
    (0x1F33,  42,  69, 0);
    (0x19A8,  43,  70, 0);
    (0x1518,  44,  72, 0);
    (0x1177,  45,  73, 0);
    (0x0E74,  46,  74, 0);
    (0x0BFB,  47,  75, 0);
    (0x09F8,  48,  77, 0);
    (0x0861,  49,  78, 0);
    (0x0706,  50,  79, 0);
    (* Index 50-59 *)
    (0x05CD,  51,  48, 0);
    (0x04DE,  52,  50, 0);
    (0x040F,  53,  50, 0);
    (0x0363,  54,  51, 0);
    (0x02D4,  55,  52, 0);
    (0x025C,  56,  53, 0);
    (0x01F8,  57,  54, 0);
    (0x01A4,  58,  55, 0);
    (0x0160,  59,  56, 0);
    (0x0125,  60,  57, 0);
    (* Index 60-69 *)
    (0x00F6,  61,  58, 0);
    (0x00CB,  62,  59, 0);
    (0x00AB,  63,  61, 0);
    (0x008F,  32,  61, 0);
    (0x5B12,  65,  65, 1);
    (0x4D04,  66,  80, 0);
    (0x412C,  67,  81, 0);
    (0x37D8,  68,  82, 0);
    (0x2FE8,  69,  83, 0);
    (0x293C,  70,  84, 0);
    (* Index 70-79 *)
    (0x2379,  71,  86, 0);
    (0x1EDF,  72,  87, 0);
    (0x1AA9,  73,  87, 0);
    (0x174E,  74,  72, 0);
    (0x1424,  75,  72, 0);
    (0x119C,  76,  74, 0);
    (0x0F6B,  77,  74, 0);
    (0x0D51,  78,  75, 0);
    (0x0BB6,  79,  77, 0);
    (0x0A40,  48,  77, 0);
    (* Index 80-89 *)
    (0x5832,  81,  80, 1);
    (0x4D1C,  82,  88, 0);
    (0x438E,  83,  89, 0);
    (0x3BDD,  84,  90, 0);
    (0x34EE,  85,  91, 0);
    (0x2EAE,  86,  92, 0);
    (0x299A,  87,  93, 0);
    (0x2516,  71,  86, 0);
    (0x5570,  89,  88, 1);
    (0x4CA9,  90,  95, 0);
    (* Index 90-99 *)
    (0x44D9,  91,  96, 0);
    (0x3E22,  92,  97, 0);
    (0x3824,  93,  99, 0);
    (0x32B4,  94,  99, 0);
    (0x2E17,  86,  93, 0);
    (0x56A8,  96,  95, 1);
    (0x4F46,  97, 101, 0);
    (0x47E5,  98, 102, 0);
    (0x41CF,  99, 103, 0);
    (0x3C3D, 100, 104, 0);
    (* Index 100-109 *)
    (0x375E,  93,  99, 0);
    (0x5231, 102, 105, 0);
    (0x4C0F, 103, 106, 0);
    (0x4639, 104, 107, 0);
    (0x415E,  99, 103, 0);
    (0x5627, 106, 105, 1);
    (0x50E7, 107, 108, 0);
    (0x4B85, 103, 109, 0);
    (0x5597, 109, 110, 0);
    (0x504F, 107, 111, 0);
    (* Index 110-113 *)
    (0x5A10, 111, 110, 1);
    (0x5522, 109, 112, 0);
    (0x59EB, 111, 112, 1);
    (* Index 113: Fixed probability 0.5 per ITU-T T.851 Section 10.3 *)
    (0x5A1D, 113, 113, 0);
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
  mutable c : int; (* Code register, 32-bit *)
  mutable ct : int; (* Bit counter - can be negative during init! *)
  mutable data : bytes; (* Input data *)
  mutable pos : int; (* Current byte position *)
  mutable marker_found : bool; (* True if we hit a marker *)
}
(** JPEG arithmetic decoder state per ITU-T T.81 *)

(* Debug flag for decode tracing *)
let debug_decode = ref false

(** Read next byte from input, handling stuffing (libjpeg style) *)
let rec get_byte state =
  if state.marker_found then 0
  else if state.pos >= Bytes.length state.data then 0
  else begin
    let b = Bytes.get_uint8 state.data state.pos in
    state.pos <- state.pos + 1;
    if b = 0xFF then begin
      (* Check for stuffing or marker *)
      if state.pos >= Bytes.length state.data then 0
      else begin
        let next = Bytes.get_uint8 state.data state.pos in
        if next = 0x00 then begin
          (* Stuffed zero - skip it and use 0xFF *)
          state.pos <- state.pos + 1;
          0xFF
        end
        else if next = 0xFF then begin
          (* Multiple 0xFF bytes - skip and recurse *)
          state.pos <- state.pos + 1;
          get_byte state
        end
        else begin
          (* Marker found - don't consume it, return 0 *)
          state.marker_found <- true;
          0
        end
      end
    end
    else b
  end

(** Initialize the JPEG arithmetic decoder (libjpeg style)
    CT starts at -16 to force reading 2 initial bytes during first renorm *)
let init_jpeg_decoder data =
  { a = 0; c = 0; ct = -16; data; pos = 0; marker_found = false }

(** Decode a single binary decision (DECODE - exact libjpeg algorithm)

    This implements the core arithmetic decode from ITU-T T.81 D.2.4-D.2.6
    with libjpeg's optimization of a floating cut-point in the C register.

    CT tracks the bit position: negative during init, 0-7 during normal decode.
    The comparison threshold is shifted by CT to match C's current alignment.
*)
let decode_decision ctx state =
  (* Renormalization & data input per section D.2.6 *)
  while state.a < 0x8000 do
    state.ct <- state.ct - 1;
    if state.ct < 0 then begin
      (* Need to fetch next data byte *)
      let data = get_byte state in
      state.c <- ((state.c lsl 8) lor data) land 0xFFFFFFFF;
      state.ct <- state.ct + 8;
      if state.ct < 0 then begin
        (* Need more initial bytes *)
        state.ct <- state.ct + 1;
        if state.ct = 0 then
          (* Got 2 initial bytes -> set A to exit loop after shift *)
          state.a <- 0x8000
      end
    end;
    state.a <- state.a lsl 1
  done;

  let qe = get_qe ctx.index in

  (* Decode & estimation procedures per sections D.2.4 & D.2.5 *)
  let temp = state.a - qe in
  state.a <- temp;
  let threshold = temp lsl state.ct in

  if !debug_decode then
    Printf.printf "  decode: a=0x%04x c=0x%08x ct=%d threshold=0x%08x, c>=threshold? %b\n"
      state.a state.c state.ct threshold (state.c >= threshold);

  if state.c >= threshold then begin
    (* LPS path: C >= threshold *)
    state.c <- state.c - threshold;
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
  else if state.a < 0x8000 then begin
    (* MPS path with conditional exchange check *)
    if state.a < qe then begin
      (* Conditional exchange - this is actually LPS *)
      let result = 1 - ctx.mps in
      if get_switch ctx.index <> 0 then ctx.mps <- 1 - ctx.mps;
      ctx.index <- get_nlps ctx.index;
      result
    end
    else begin
      (* Normal MPS *)
      ctx.index <- get_nmps ctx.index;
      ctx.mps
    end
  end
  else begin
    (* MPS path - A >= 0x8000, no renorm needed, no context update *)
    (* Per ITU-T T.81: context is only updated when renormalization occurs *)
    ctx.mps
  end

(** Initialize decoder from bitstream reader position *)
let init_jpeg_decoder_from_reader reader =
  let data = reader.Bitstream.data in
  let pos = reader.Bitstream.pos in
  let remaining = Bytes.sub data pos (Bytes.length data - pos) in
  init_jpeg_decoder remaining

(* ============================================================================
   JPEG DC/AC Coefficient Coding (ITU-T T.81 Annex F.1.4)

   Flat bin layout matching libjpeg-turbo (jcarith.c / jdarith.c):
   - DC: 64 contexts per table, indexed by dc_context + offset
   - AC: 256 contexts per table, with 3 bins per coefficient position
   - Fixed bin at state 113 for sign bits and refinement (0.5 probability)
   ============================================================================ *)

let dc_stat_bins_size = 64
let ac_stat_bins_size = 256

type dc_stat_bins = context array
(** DC statistics: flat array of 64 contexts matching libjpeg DC_STAT_BINS *)

type ac_stat_bins = context array
(** AC statistics: flat array of 256 contexts matching libjpeg AC_STAT_BINS *)

let create_dc_stat_bins () =
  Array.init dc_stat_bins_size (fun _ -> create_context ())

let create_ac_stat_bins () =
  Array.init ac_stat_bins_size (fun _ -> create_context ())

(** Fixed-probability context at state 113 (0.5 probability per T.851) *)
let create_fixed_bin () = create_context_with_index 113

(** Decode DC difference value (matching libjpeg jdarith.c decode_mcu exactly)

    DC bin layout (dc_stats[tbl]):
    - [dc_context+0]: S0 (zero/nonzero)
    - [dc_context+1]: SS (sign)
    - [dc_context+2]: SP (positive magnitude extension)
    - [dc_context+3]: SN (negative magnitude extension)
    - [20..]: X1 (extended magnitude category bits)
    - [st+14]: Magnitude bit pattern *)
let decode_dc_diff state (bins : dc_stat_bins) dc_context l u =
  let st = dc_context in

  (* Figure F.4: Decode_DC_DIFF *)
  let s0 = decode_decision bins.(st) state in
  if s0 = 0 then (0, 0)  (* zero diff, dc_context becomes 0 *)
  else begin
    (* Figure F.7: Decode sign *)
    let sign = decode_decision bins.(st + 1) state in
    let st_mag, new_dc_ctx =
      if sign = 0 then (st + 2, 4)   (* positive: SP, small positive category *)
      else (st + 3, 8)               (* negative: SN, small negative category *)
    in

    (* Figure F.23: Decode magnitude category *)
    let m = ref 0 in
    let st_ref = ref st_mag in
    (* First category bit from SP or SN context *)
    m := decode_decision bins.(!st_ref) state;
    if !m <> 0 then begin
      (* Magnitude > 1: switch to X1 context *)
      st_ref := 20;
      while decode_decision bins.(!st_ref) state <> 0 do
        m := !m lsl 1;
        st_ref := !st_ref + 1
      done
    end;

    (* Figure F.24: Decode magnitude bit pattern *)
    let v = ref !m in  (* v starts at m, matching libjpeg's v = m *)
    st_ref := !st_ref + 14;
    let m2 = ref !m in
    while !m2 lsr 1 <> 0 do
      m2 := !m2 lsr 1;
      if decode_decision bins.(!st_ref) state <> 0 then
        v := !v lor !m2
    done;

    (* Reconstruct value: magnitude = v + 1 *)
    let magnitude = !v + 1 in

    (* Conditioning category update *)
    let final_dc_ctx =
      if !m < (1 lsl l) lsr 1 then 0
      else if !m > (1 lsl u) lsr 1 then new_dc_ctx + 8
      else new_dc_ctx
    in

    let diff = if sign = 0 then magnitude else -magnitude in
    (diff, final_dc_ctx)
  end

(** Decode AC coefficients (matching libjpeg jdarith.c decode_mcu exactly)

    AC bin layout (ac_stats[tbl]) - matches libjpeg (k=0 start):
    For coefficient at position p (1-63):
    - [3*(p-1)+0]: EOB decision
    - [3*(p-1)+1]: Zero/nonzero
    - [3*(p-1)+2]: SX (magnitude extension)
    - [189]: X1 for k <= Kx
    - [217]: X1 for k > Kx
    - [st+14]: Magnitude bit pattern
    Sign uses fixed_bin (state 113). *)
let decode_ac_block state (bins : ac_stat_bins) fixed_bin kx =
  let coeffs = Array.make 64 0 in

  (* Figure F.5: Decode_AC_Coefficients - matching jdarith.c decode_mcu *)
  let k = ref 1 in
  let outer_done = ref false in
  while not !outer_done && !k <= 63 do
    let st = ref (3 * (!k - 1)) in

    (* EOB decision *)
    let eob = decode_decision bins.(!st) state in
    if eob <> 0 then
      outer_done := true  (* All remaining coefficients are zero *)
    else begin
      let inner_done = ref false in
      while not !inner_done do
        (* Zero/nonzero decision at 3*k+1 *)
        let nz = decode_decision bins.(!st + 1) state in
        if nz <> 0 then begin
          (* Nonzero coefficient *)
          let sign = decode_decision fixed_bin state in

          (* Decode magnitude category *)
          let st2 = ref (!st + 2) in
          let m = ref (decode_decision bins.(!st2) state) in
          if !m <> 0 then begin
            if decode_decision bins.(!st2) state <> 0 then begin
              m := 2;
              st2 := if !k <= kx then 189 else 217;
              let cat_done = ref false in
              while not !cat_done do
                if decode_decision bins.(!st2) state <> 0 then begin
                  if !m lsl 1 = 0x8000 then cat_done := true
                  else begin m := !m lsl 1; st2 := !st2 + 1 end
                end
                else cat_done := true
              done
            end
          end;

          (* Figure F.24: Decode magnitude bit pattern *)
          let v = ref !m in  (* v starts at m, matching libjpeg *)
          st2 := !st2 + 14;
          let m2 = ref !m in
          while !m2 lsr 1 <> 0 do
            m2 := !m2 lsr 1;
            if decode_decision bins.(!st2) state <> 0 then
              v := !v lor !m2
          done;
          let magnitude = !v + 1 in
          coeffs.(!k) <- (if sign <> 0 then -magnitude else magnitude);
          inner_done := true;
          (* Outer loop will increment k *)
        end
        else begin
          (* Zero coefficient - advance to next position *)
          k := !k + 1;
          if !k > 63 then
            inner_done := true
          else begin
            st := 3 * (!k - 1);
            (* EOB test at new position *)
            if decode_decision bins.(!st) state <> 0 then begin
              inner_done := true;
              outer_done := true
            end
            (* else: not EOB, continue inner loop *)
          end
        end
      done;
      k := !k + 1
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
  fixed_bin : context;          (* Fixed 0.5 probability for sign/refinement *)
  prev_dc : int array; (* Previous DC values per component *)
  dc_context : int array; (* DC conditioning category per component *)
  l : int array; (* DC conditioning L values per component *)
  u : int array; (* DC conditioning U values per component *)
  kx : int array; (* AC conditioning Kx values per component *)
}
(** Arithmetic decoding state for a full scan *)

(** Initialize arithmetic scan decoder *)
let init_arith_scan_decoder data num_components =
  {
    decoder = init_jpeg_decoder data;
    dc_bins = Array.init num_components (fun _ -> create_dc_stat_bins ());
    ac_bins = Array.init num_components (fun _ -> create_ac_stat_bins ());
    fixed_bin = create_fixed_bin ();
    prev_dc = Array.make num_components 0;
    dc_context = Array.make num_components 0;
    l = Array.make num_components 0;
    u = Array.make num_components 1;
    kx = Array.make num_components 5;
  }

(** Set conditioning values from DAC markers *)
let set_conditioning state component_idx is_dc value =
  if is_dc then begin
    state.l.(component_idx) <- value;
    state.u.(component_idx) <- value + 1
  end
  else state.kx.(component_idx) <- value

(** Decode a single 8x8 block with arithmetic coding *)
let decode_arith_block state component_idx =
  let dc_bins = state.dc_bins.(component_idx) in
  let ac_bins = state.ac_bins.(component_idx) in
  let l = state.l.(component_idx) in
  let u = state.u.(component_idx) in
  let kx = state.kx.(component_idx) in
  let dc_ctx = state.dc_context.(component_idx) in

  (* Decode DC difference *)
  let prev_dc = state.prev_dc.(component_idx) in
  let dc_diff, new_dc_ctx = decode_dc_diff state.decoder dc_bins dc_ctx l u in
  let dc_value = prev_dc + dc_diff in
  state.prev_dc.(component_idx) <- dc_value;
  state.dc_context.(component_idx) <- new_dc_ctx;

  (* Decode AC coefficients *)
  let coeffs = decode_ac_block state.decoder ac_bins state.fixed_bin kx in
  coeffs.(0) <- dc_value;

  coeffs

(** Reset all contexts in a flat bin array *)
let reset_bins bins =
  Array.iter (fun c -> c.index <- 0; c.mps <- 0) bins

(** Reset decoder state at restart marker *)
let reset_arith_decoder state =
  (* Reset DC predictors *)
  Array.fill state.prev_dc 0 (Array.length state.prev_dc) 0;
  (* Reset DC conditioning contexts *)
  Array.fill state.dc_context 0 (Array.length state.dc_context) 0;
  (* Reset statistical bins to initial state *)
  Array.iter reset_bins state.dc_bins;
  Array.iter reset_bins state.ac_bins

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
  mutable zc : int; (* Zero count for deferred 0x00 bytes *)
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

(* Debug flag for tracing - set to true to enable *)
let debug_byteout = ref false

(** Output a raw byte to the output buffer *)
let emit_byte state b =
  ensure_capacity state 1;
  Bytes.set_uint8 state.output state.output_pos (b land 0xFF);
  state.output_pos <- state.output_pos + 1

(** Byte output procedure (BYTEOUT from T.81 D.1.5, with Pacman zero suppression)

    Implements libjpeg-turbo's byte output with three deferred-output variables:
    - buffer: previously computed output byte (-1 = empty)
    - st (SC): count of stacked 0xFF bytes awaiting carry resolution
    - zc: count of deferred 0x00 bytes (Pacman optimization)
*)
let byteout state =
  if !debug_byteout then
    Printf.printf "byteout enter: c=0x%08x ct=%d st=%d zc=%d bp=%d buffer=0x%x output_pos=%d\n"
      state.c state.ct state.st state.zc state.bp state.buffer state.output_pos;

  (* Extract byte from bits 19-26 of C *)
  let temp = state.c lsr 19 in

  if temp > 0xFF then begin
    (* Carry path: propagate carry into buffer and stacked bytes *)
    if state.buffer >= 0 then begin
      (* Flush deferred zero bytes *)
      while state.zc > 0 do emit_byte state 0x00; state.zc <- state.zc - 1 done;
      (* Output buffer + 1 (carry absorbed) *)
      emit_byte state (state.buffer + 1);
      if state.buffer + 1 = 0xFF then emit_byte state 0x00
    end;
    (* Stacked 0xFF bytes + carry become 0x00: defer them *)
    state.zc <- state.zc + state.st;
    state.st <- 0;
    state.buffer <- temp land 0xFF
  end
  else if temp = 0xFF then begin
    (* Stack path: defer 0xFF byte *)
    state.st <- state.st + 1
  end
  else begin
    (* Normal path: flush buffer and stacked bytes *)
    if state.buffer = 0 then
      (* Defer zero buffer byte *)
      state.zc <- state.zc + 1
    else if state.buffer >= 0 then begin
      (* Flush deferred zeros, then output buffer *)
      while state.zc > 0 do emit_byte state 0x00; state.zc <- state.zc - 1 done;
      emit_byte state state.buffer
    end;
    (* Output stacked 0xFF bytes with stuffing *)
    if state.st > 0 then begin
      while state.zc > 0 do emit_byte state 0x00; state.zc <- state.zc - 1 done;
      while state.st > 0 do
        emit_byte state 0xFF;
        emit_byte state 0x00;
        state.st <- state.st - 1
      done
    end;
    state.buffer <- temp land 0xFF
  end;

  (* Clear bits 19+ from C, keep bits 0-18 *)
  state.c <- state.c land 0x7FFFF;
  (* Increment CT by 8 (we've made room for 8 more bits) *)
  state.ct <- state.ct + 8;

  if !debug_byteout then
    Printf.printf "byteout exit: c=0x%08x buffer=0x%x st=%d zc=%d ct=%d output_pos=%d\n"
      state.c state.buffer state.st state.zc state.ct state.output_pos

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
    zc = 0;
    (* No deferred zero bytes *)
    bp = -1;
    (* No bytes output yet *)
    buffer = -1;
    (* Buffer empty initially (libjpeg convention: -1 means empty) *)
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

(** Encode a single binary decision (ENCODE - exact libjpeg algorithm)

    This matches jcarith.c arith_encode() exactly:
    - For MPS: if A < Qe, do conditional exchange (C += A, A = Qe)
    - For LPS: if A >= Qe, normal case (C += A, A = Qe), else keep A
*)
let encode_decision ctx state d =
  let qe = get_qe ctx.index in

  (* A = A - Qe *)
  state.a <- state.a - qe;

  if d <> ctx.mps then begin
    (* Encode LPS (less probable symbol) *)
    if state.a >= qe then begin
      (* Normal LPS: interval for LPS is larger, so we use it *)
      state.c <- state.c + state.a;
      state.a <- qe
    end;
    (* else: A < Qe means conditional exchange - we keep the current A
       (which is the MPS interval) but use LPS state transition *)
    if get_switch ctx.index <> 0 then ctx.mps <- 1 - ctx.mps;
    ctx.index <- get_nlps ctx.index
  end
  else begin
    (* Encode MPS (more probable symbol) *)
    if state.a >= 0x8000 then begin
      (* A >= 0x8000: no renormalization needed, just return *)
      ()
    end
    else begin
      if state.a < qe then begin
        (* Conditional exchange: A < Qe means we use LPS interval for MPS *)
        state.c <- state.c + state.a;
        state.a <- qe
      end;
      ctx.index <- get_nmps ctx.index
    end
  end;

  (* Renormalize if needed *)
  if state.a < 0x8000 then renorme state

(** Flush the encoder (libjpeg-turbo finish_pass with Pacman termination) *)
let flush_jpeg_encoder state =
  (* Section D.1.8: Termination of encoding *)
  (* Find the value in the coding interval with the largest number of
     trailing zero bits (Pacman termination) *)
  let temp = (state.a - 1 + state.c) land 0xFFFF0000 in
  if temp < state.c then
    state.c <- temp + 0x8000
  else
    state.c <- temp;

  (* Shift C left by CT to align final bits *)
  state.c <- state.c lsl state.ct;

  (* Check for final carry (bits 27-31) *)
  if state.c land 0xF8000000 <> 0 then begin
    (* Carry path *)
    if state.buffer >= 0 then begin
      while state.zc > 0 do emit_byte state 0x00; state.zc <- state.zc - 1 done;
      emit_byte state (state.buffer + 1);
      if state.buffer + 1 = 0xFF then emit_byte state 0x00
    end;
    state.zc <- state.zc + state.st;
    state.st <- 0
  end
  else begin
    (* Normal path *)
    if state.buffer = 0 then
      state.zc <- state.zc + 1
    else if state.buffer >= 0 then begin
      while state.zc > 0 do emit_byte state 0x00; state.zc <- state.zc - 1 done;
      emit_byte state state.buffer
    end;
    if state.st > 0 then begin
      while state.zc > 0 do emit_byte state 0x00; state.zc <- state.zc - 1 done;
      while state.st > 0 do
        emit_byte state 0xFF;
        emit_byte state 0x00;
        state.st <- state.st - 1
      done
    end
  end;

  (* Output final bytes from C register, only if needed *)
  if state.c land 0x7FFF800 <> 0 then begin
    while state.zc > 0 do emit_byte state 0x00; state.zc <- state.zc - 1 done;
    let b1 = (state.c lsr 19) land 0xFF in
    emit_byte state b1;
    if b1 = 0xFF then emit_byte state 0x00;
    if state.c land 0x7F800 <> 0 then begin
      let b2 = (state.c lsr 11) land 0xFF in
      emit_byte state b2;
      if b2 = 0xFF then emit_byte state 0x00
    end
  end;

  (* Return the output bytes *)
  Bytes.sub state.output 0 state.output_pos

(** Encode DC difference value (matching libjpeg jcarith.c encode_mcu exactly)

    Returns the new dc_context value for conditioning. *)
let encode_dc_diff state (bins : dc_stat_bins) dc_context diff l u =
  let st = dc_context in

  (* Figure F.4: Encode_DC_DIFF *)
  if diff = 0 then begin
    encode_decision bins.(st) state 0;
    0  (* dc_context = 0: zero diff category *)
  end
  else begin
    encode_decision bins.(st) state 1;

    (* Figure F.7: Encoding the sign of v *)
    let v, new_dc_ctx =
      if diff > 0 then begin
        encode_decision bins.(st + 1) state 0;  (* SS: positive *)
        (diff, 4)  (* SP = st+2, small positive diff *)
      end
      else begin
        encode_decision bins.(st + 1) state 1;  (* SS: negative *)
        (-diff, 8)  (* SN = st+3, small negative diff *)
      end
    in
    let st_mag = if diff > 0 then st + 2 else st + 3 in

    (* Figure F.8: Encoding the magnitude category of v *)
    let m = ref 0 in
    let st_ref = ref st_mag in
    let v_minus_1 = v - 1 in
    if v_minus_1 <> 0 then begin
      encode_decision bins.(!st_ref) state 1;
      m := 1;
      let v2 = ref v_minus_1 in
      st_ref := 20;  (* X1 *)
      while !v2 lsr 1 <> 0 do
        v2 := !v2 lsr 1;
        encode_decision bins.(!st_ref) state 1;
        m := !m lsl 1;
        st_ref := !st_ref + 1
      done
    end;
    encode_decision bins.(!st_ref) state 0;  (* End of category *)

    (* Section F.1.4.4.1.2: Establish dc_context conditioning category *)
    let final_dc_ctx =
      if !m < (1 lsl l) lsr 1 then 0
      else if !m > (1 lsl u) lsr 1 then new_dc_ctx + 8
      else new_dc_ctx
    in

    (* Figure F.9: Encoding the magnitude bit pattern of v *)
    let st_bits = !st_ref + 14 in
    let m2 = ref !m in
    while !m2 lsr 1 <> 0 do
      m2 := !m2 lsr 1;
      let bit = if !m2 land v_minus_1 <> 0 then 1 else 0 in
      encode_decision bins.(st_bits) state bit
    done;

    final_dc_ctx
  end

(** Encode AC coefficients for a block (matching libjpeg jcarith.c encode_mcu) *)
let encode_ac_block state (bins : ac_stat_bins) fixed_bin coeffs kx =
  (* Find the last non-zero coefficient (EOB index) *)
  let ke = ref 0 in
  for i = 63 downto 1 do
    if !ke = 0 && coeffs.(i) <> 0 then ke := i
  done;

  (* Figure F.5: Encode_AC_Coefficients - matching jcarith.c encode_mcu *)
  let k = ref 1 in
  let outer_done = ref false in
  while not !outer_done && !k <= 63 do
    let st = ref (3 * (!k - 1)) in
    if !k > !ke then begin
      (* Figure F.6: Encode EOB *)
      encode_decision bins.(!st) state 1;
      outer_done := true
    end
    else begin
      encode_decision bins.(!st) state 0;  (* Not EOB *)
      let inner_done = ref false in
      while not !inner_done do
        let v = coeffs.(!k) in
        if v <> 0 then begin
          encode_decision bins.(!st + 1) state 1;  (* Nonzero *)
          (* Sign *)
          let sign = if v < 0 then 1 else 0 in
          encode_decision fixed_bin state sign;
          (* Magnitude category *)
          let st2 = ref (!st + 2) in
          let abs_v = abs v in
          let m = ref 0 in
          let v_minus_1 = abs_v - 1 in
          if v_minus_1 <> 0 then begin
            encode_decision bins.(!st2) state 1;
            m := 1;
            let v2 = ref v_minus_1 in
            if !v2 lsr 1 <> 0 then begin
              v2 := !v2 lsr 1;
              encode_decision bins.(!st2) state 1;
              m := !m lsl 1;
              st2 := if !k <= kx then 189 else 217;
              while !v2 lsr 1 <> 0 do
                v2 := !v2 lsr 1;
                encode_decision bins.(!st2) state 1;
                m := !m lsl 1;
                st2 := !st2 + 1
              done
            end
          end;
          encode_decision bins.(!st2) state 0;  (* End of category *)
          (* Magnitude bit pattern *)
          st2 := !st2 + 14;
          let m2 = ref !m in
          while !m2 lsr 1 <> 0 do
            m2 := !m2 lsr 1;
            let bit = if !m2 land v_minus_1 <> 0 then 1 else 0 in
            encode_decision bins.(!st2) state bit
          done;
          inner_done := true
          (* Outer loop will increment k *)
        end
        else begin
          encode_decision bins.(!st + 1) state 0;  (* Zero *)
          k := !k + 1;
          if !k > 63 then begin
            inner_done := true;
            outer_done := true
          end
          else begin
            st := 3 * (!k - 1);
            if !k > !ke then begin
              encode_decision bins.(!st) state 1;  (* EOB *)
              inner_done := true;
              outer_done := true
            end
            else
              encode_decision bins.(!st) state 0  (* Not EOB *)
          end
        end
      done;
      k := !k + 1
    end
  done

(* ============================================================================
   Full JPEG Arithmetic Scan Encoder
   ============================================================================ *)

type arith_encode_scan_state = {
  encoder : jpeg_encoder_state;
  dc_bins : dc_stat_bins array; (* Per component *)
  ac_bins : ac_stat_bins array; (* Per component *)
  fixed_bin : context;          (* Fixed 0.5 probability for sign/refinement *)
  prev_dc : int array; (* Previous DC values per component *)
  dc_context : int array; (* DC conditioning category per component *)
  l : int array; (* DC conditioning L values per component *)
  u : int array; (* DC conditioning U values per component *)
  kx : int array; (* AC conditioning Kx values per component *)
}
(** Arithmetic encoding state for a full scan *)

(** Initialize arithmetic scan encoder *)
let init_arith_scan_encoder num_components =
  {
    encoder = init_jpeg_encoder ();
    dc_bins = Array.init num_components (fun _ -> create_dc_stat_bins ());
    ac_bins = Array.init num_components (fun _ -> create_ac_stat_bins ());
    fixed_bin = create_fixed_bin ();
    prev_dc = Array.make num_components 0;
    dc_context = Array.make num_components 0;
    l = Array.make num_components 0;
    u = Array.make num_components 1;
    kx = Array.make num_components 5;
  }

(** Set encoder conditioning values *)
let set_encoder_conditioning state component_idx is_dc value =
  if is_dc then begin
    state.l.(component_idx) <- value;
    state.u.(component_idx) <- value + 1
  end
  else state.kx.(component_idx) <- value

(** Encode a single 8x8 block with arithmetic coding *)
let encode_arith_block state component_idx coeffs =
  let dc_bins = state.dc_bins.(component_idx) in
  let ac_bins = state.ac_bins.(component_idx) in
  let l = state.l.(component_idx) in
  let u = state.u.(component_idx) in
  let kx = state.kx.(component_idx) in
  let dc_ctx = state.dc_context.(component_idx) in

  (* Encode DC difference *)
  let prev_dc = state.prev_dc.(component_idx) in
  let dc_diff = coeffs.(0) - prev_dc in
  let new_dc_ctx = encode_dc_diff state.encoder dc_bins dc_ctx dc_diff l u in
  state.prev_dc.(component_idx) <- coeffs.(0);
  state.dc_context.(component_idx) <- new_dc_ctx;

  (* Encode AC coefficients *)
  encode_ac_block state.encoder ac_bins state.fixed_bin coeffs kx

(** Encode only the DC coefficient of a block (for progressive DC scans) *)
let encode_arith_dc_only state component_idx dc_value =
  let dc_bins = state.dc_bins.(component_idx) in
  let l = state.l.(component_idx) in
  let u = state.u.(component_idx) in
  let dc_ctx = state.dc_context.(component_idx) in
  let prev_dc = state.prev_dc.(component_idx) in
  let dc_diff = dc_value - prev_dc in
  let new_dc_ctx = encode_dc_diff state.encoder dc_bins dc_ctx dc_diff l u in
  state.prev_dc.(component_idx) <- dc_value;
  state.dc_context.(component_idx) <- new_dc_ctx

(** Encode AC coefficients in a spectral range (for progressive AC scans) *)
let encode_arith_ac_only state component_idx coeffs ~ss ~se:scan_se =
  let ac_bins = state.ac_bins.(component_idx) in
  let kx = state.kx.(component_idx) in

  (* Find the last non-zero coefficient in the spectral range *)
  let ke = ref 0 in
  for i = scan_se downto ss do
    if !ke = 0 && coeffs.(i) <> 0 then ke := i
  done;

  (* Matching jcarith.c encode_mcu_AC_first structure with 3*k indexing *)
  let k = ref ss in
  let outer_done = ref false in
  while not !outer_done && !k <= scan_se do
    let st = ref (3 * (!k - 1)) in
    if !k > !ke then begin
      encode_decision ac_bins.(!st) state.encoder 1;  (* EOB *)
      outer_done := true
    end
    else begin
      encode_decision ac_bins.(!st) state.encoder 0;  (* Not EOB *)
      let inner_done = ref false in
      while not !inner_done do
        let v = coeffs.(!k) in
        if v <> 0 then begin
          encode_decision ac_bins.(!st + 1) state.encoder 1;  (* Nonzero *)
          let sign = if v < 0 then 1 else 0 in
          encode_decision state.fixed_bin state.encoder sign;
          let st2 = ref (!st + 2) in
          let abs_v = abs v in
          let m = ref 0 in
          let v_minus_1 = abs_v - 1 in
          if v_minus_1 <> 0 then begin
            encode_decision ac_bins.(!st2) state.encoder 1;
            m := 1;
            let v2 = ref v_minus_1 in
            if !v2 lsr 1 <> 0 then begin
              v2 := !v2 lsr 1;
              encode_decision ac_bins.(!st2) state.encoder 1;
              m := !m lsl 1;
              st2 := if !k <= kx then 189 else 217;
              while !v2 lsr 1 <> 0 do
                v2 := !v2 lsr 1;
                encode_decision ac_bins.(!st2) state.encoder 1;
                m := !m lsl 1;
                st2 := !st2 + 1
              done
            end
          end;
          encode_decision ac_bins.(!st2) state.encoder 0;
          st2 := !st2 + 14;
          let m2 = ref !m in
          while !m2 lsr 1 <> 0 do
            m2 := !m2 lsr 1;
            let bit = if !m2 land v_minus_1 <> 0 then 1 else 0 in
            encode_decision ac_bins.(!st2) state.encoder bit
          done;
          inner_done := true
        end
        else begin
          encode_decision ac_bins.(!st + 1) state.encoder 0;  (* Zero *)
          k := !k + 1;
          if !k > scan_se then begin
            inner_done := true;
            outer_done := true
          end
          else begin
            st := 3 * (!k - 1);
            if !k > !ke then begin
              encode_decision ac_bins.(!st) state.encoder 1;  (* EOB *)
              inner_done := true;
              outer_done := true
            end
            else
              encode_decision ac_bins.(!st) state.encoder 0  (* Not EOB *)
          end
        end
      done;
      k := !k + 1
    end
  done

(** Encode a single prediction error for lossless JPEG *)
let encode_lossless_diff state component_idx diff =
  let dc_bins = state.dc_bins.(component_idx) in
  let l = state.l.(component_idx) in
  let u = state.u.(component_idx) in
  let dc_ctx = state.dc_context.(component_idx) in

  let new_dc_ctx = encode_dc_diff state.encoder dc_bins dc_ctx diff l u in
  state.dc_context.(component_idx) <- new_dc_ctx;
  state.prev_dc.(component_idx) <- diff

(** Reset encoder state at restart marker *)
let reset_arith_encoder state =
  Array.fill state.prev_dc 0 (Array.length state.prev_dc) 0;
  Array.fill state.dc_context 0 (Array.length state.dc_context) 0;
  Array.iter reset_bins state.dc_bins;
  Array.iter reset_bins state.ac_bins;
  state.encoder.a <- 0x10000;
  state.encoder.c <- 0;
  state.encoder.ct <- 11;
  state.encoder.st <- 0;
  state.encoder.zc <- 0;
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
