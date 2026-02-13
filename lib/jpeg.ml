(** Pure OCaml JPEG library *)

(* Re-export internal modules for advanced use *)
module Bitstream = Bitstream
module Markers = Markers
module Huffman = Huffman
module Dct = Dct
module Quantization = Quantization
module Color = Color
module Exif = Exif
module Arithmetic = Arithmetic
module Icc = Icc
module Predictor = Predictor

(** Pixel format for image data *)
type pixel_format =
  | RGB24  (** 3 bytes per pixel: R, G, B *)
  | CMYK32  (** 4 bytes per pixel: C, M, Y, K *)

type pixel_data =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** Pixel data stored as RGB24 or CMYK32 format *)

type image = {
  width : int;
  height : int;
  pixels : pixel_data;
  pixel_format : pixel_format;
  exif : Exif.t option;
  icc_profile : Icc.t option;
}
(** JPEG image *)

(** Chroma subsampling modes *)
type subsampling = Sub_444 | Sub_422 | Sub_420

(** Color modes *)
type color_mode =
  | Color  (** RGB -> YCbCr, 3 components *)
  | Grayscale  (** Y only, 1 component *)
  | CMYK  (** 4 components, no conversion *)
  | YCCK  (** CMYK via YCbCr + K, 4 components *)

(** Encoding modes *)
type encoding_mode = Baseline | Progressive | Lossless

(** Bit precision for sample values *)
type precision = Precision_8 | Precision_12

(** Entropy coding method *)
type entropy_coding = Huffman | Arithmetic

type encode_options = {
  quality : int;
  subsampling : subsampling;
  color_mode : color_mode;
  encoding_mode : encoding_mode;
  restart_interval : int;  (** MCUs between RST markers, 0 = disabled *)
  precision : precision;  (** Sample precision: 8-bit or 12-bit *)
  entropy_coding : entropy_coding;  (** Huffman or arithmetic coding *)
  predictor : int;  (** Predictor selection for lossless mode (1-7), 0 = auto *)
  point_transform : int;  (** Point transform for lossless mode (0 = none) *)
}
(** Encoding options *)

let default_encode_options =
  {
    quality = 75;
    subsampling = Sub_420;
    color_mode = Color;
    encoding_mode = Baseline;
    restart_interval = 0;
    precision = Precision_8;
    entropy_coding = Huffman;
    predictor = 1;
    point_transform = 0;
  }

type decode_state = {
  mutable frame : Markers.frame_header option;
  mutable quant_tables : Markers.quant_table array;
  mutable dc_tables : Huffman.table array;
  mutable ac_tables : Huffman.table array;
  mutable arith_dc_conditioning : int array; (* L values for DC *)
  mutable arith_ac_conditioning : int array; (* Kx values for AC *)
  mutable restart_interval : int;
  mutable exif : Exif.t option;
  mutable icc_chunks : (int * int * bytes) list;
}
(** Decoding state *)

(** Scan types for progressive JPEG *)
type scan_type =
  | DC_First (* ss=0, se=0, ah=0: First appearance of DC coefficient *)
  | DC_Refine (* ss=0, se=0, ah>0: Refinement bit for DC *)
  | AC_First (* ss>0, ah=0: First appearance of AC coefficients *)
  | AC_Refine (* ss>0, ah>0: Refinement bits for AC coefficients *)

type progressive_state = {
  coefficients : int array array array; (* [component][block][coeff] *)
  eob_run : int ref array; (* EOB run counter per component *)
}
(** Progressive decoding state *)

(** Classify scan type from SOS parameters *)
let classify_scan (scan : Markers.scan_header) =
  match (scan.ss, scan.ah) with
  | 0, 0 -> DC_First
  | 0, _ -> DC_Refine
  | _, 0 -> AC_First
  | _, _ -> AC_Refine

(** Initialize progressive state with coefficient storage *)
let init_progressive_state frame mcus_x mcus_y =
  let num_components = Array.length frame.Markers.components in
  let coefficients =
    Array.init num_components (fun ci ->
        let comp = frame.Markers.components.(ci) in
        let h_blocks = mcus_x * comp.Markers.h_sampling in
        let v_blocks = mcus_y * comp.Markers.v_sampling in
        let num_blocks = h_blocks * v_blocks in
        Array.init num_blocks (fun _ -> Array.make 64 0))
  in
  let eob_run = Array.init num_components (fun _ -> ref 0) in
  { coefficients; eob_run }

(** Decode DC coefficient in first scan (DC_First) *)
let decode_dc_first reader dc_table al prev_dc =
  let dc_category = Huffman.decode_symbol reader dc_table in
  let dc_diff =
    if dc_category = 0 then 0
    else
      let bits = Bitstream.read_bits reader dc_category in
      Huffman.extend bits dc_category
  in
  let dc_value = prev_dc + dc_diff in
  (* Apply successive approximation shift *)
  (dc_value lsl al, dc_value)

(** Decode DC refinement bit (DC_Refine) *)
let decode_dc_refine reader al coeff =
  let bit = Bitstream.read_bits reader 1 in
  coeff lor (bit lsl al)

(** Decode AC coefficients in first scan (AC_First) *)
let decode_ac_first reader ac_table ss se al coeffs eob_run =
  if !eob_run > 0 then begin decr eob_run
    (* Block is within EOB run - coefficients stay zero *)
  end
  else begin
    let k = ref ss in
    while !k <= se do
      let symbol = Huffman.decode_symbol reader ac_table in
      let run_length = symbol lsr 4 in
      let size = symbol land 0x0F in
      if size = 0 then begin
        if run_length = 15 then begin
          (* ZRL: 16 zeros *)
          k := !k + 16
        end
        else begin
          (* EOBn: end of block for n blocks *)
          eob_run := (1 lsl run_length) - 1;
          if run_length > 0 then begin
            let extra = Bitstream.read_bits reader run_length in
            eob_run := !eob_run + extra
          end;
          k := se + 1 (* Exit loop *)
        end
      end
      else begin
        k := !k + run_length;
        if !k <= se then begin
          let bits = Bitstream.read_bits reader size in
          let value = Huffman.extend bits size in
          coeffs.(!k) <- value lsl al
        end;
        incr k
      end
    done
  end

(** Apply refinement bit to an existing non-zero coefficient *)
let apply_refine_bit reader coeffs k ~plus ~minus =
  let bit = Bitstream.read_bits reader 1 in
  if bit <> 0 then
    coeffs.(k) <- (coeffs.(k) + if coeffs.(k) > 0 then plus else minus)

(** Decode AC refinement bits (AC_Refine) - most complex scan type *)
let decode_ac_refine reader ac_table ss se al coeffs eob_run =
  let plus = 1 lsl al in
  let minus = -1 lsl al in
  let k = ref ss in

  (* Refine non-zero coefficients from k to se *)
  let refine_remaining () =
    while !k <= se do
      if coeffs.(!k) <> 0 then apply_refine_bit reader coeffs !k ~plus ~minus;
      incr k
    done
  in

  (* Skip zeros while refining non-zeros, returns true if all zeros skipped *)
  let skip_zeros_refining count =
    let remaining = ref count in
    while !remaining > 0 && !k <= se do
      if coeffs.(!k) <> 0 then apply_refine_bit reader coeffs !k ~plus ~minus
      else decr remaining;
      incr k
    done
  in

  if !eob_run > 0 then begin
    refine_remaining ();
    decr eob_run
  end
  else begin
    while !k <= se do
      let symbol = Huffman.decode_symbol reader ac_table in
      let run_length = symbol lsr 4 in
      let size = symbol land 0x0F in

      if size = 0 then begin
        if run_length = 15 then skip_zeros_refining 16
        else begin
          (* EOBn *)
          eob_run := (1 lsl run_length) - 1;
          if run_length > 0 then
            eob_run := !eob_run + Bitstream.read_bits reader run_length;
          refine_remaining ()
        end
      end
      else begin
        (* size = 1: new non-zero coefficient *)
        let bit = Bitstream.read_bits reader 1 in
        let new_value = if bit <> 0 then plus else minus in
        skip_zeros_refining run_length;
        if !k <= se then coeffs.(!k) <- new_value;
        incr k
      end
    done
  end

(** Find component index by selector ID *)
let find_component_index components selector =
  let rec find i =
    if i >= Array.length components then failwith "Component not found"
    else if components.(i).Markers.component_id = selector then i
    else find (i + 1)
  in
  find 0

(** Check if a restart marker should be processed at this MCU *)
let is_restart_boundary ~restart_interval ~mcu_count =
  restart_interval > 0 && mcu_count > 0
  && mcu_count mod restart_interval = 0

(** Calculate block index within a component's block array *)
let block_index ~mcu_x ~mcu_y ~h ~v ~h_sampling ~v_sampling ~h_blocks_total =
  let bx = (mcu_x * h_sampling) + h in
  let by = (mcu_y * v_sampling) + v in
  (by * h_blocks_total) + bx

(** Calculate MCU dimensions from frame header *)
let calculate_mcu_dimensions frame =
  let components = frame.Markers.components in
  let max_h =
    Array.fold_left (fun m c -> max m c.Markers.h_sampling) 1 components
  in
  let max_v =
    Array.fold_left (fun m c -> max m c.Markers.v_sampling) 1 components
  in
  let mcu_width = max_h * 8 in
  let mcu_height = max_v * 8 in
  let mcus_x = (frame.Markers.width + mcu_width - 1) / mcu_width in
  let mcus_y = (frame.Markers.height + mcu_height - 1) / mcu_height in
  (max_h, max_v, mcu_width, mcu_height, mcus_x, mcus_y)

(** Decode one progressive scan *)
let decode_progressive_scan reader frame scan state prog_state mcus_x mcus_y =
  let scan_type = classify_scan scan in
  let ss = scan.Markers.ss in
  let se = scan.Markers.se in
  let al = scan.Markers.al in
  let components = frame.Markers.components in

  let num_scan_components = Array.length scan.Markers.scan_components in
  let prev_dc = Array.make num_scan_components 0 in

  (* Reset EOB runs at start of scan *)
  Array.iter (fun r -> r := 0) prog_state.eob_run;

  let mcu_count = ref 0 in

  for mcu_y = 0 to mcus_y - 1 do
    for mcu_x = 0 to mcus_x - 1 do
      (* Check for restart marker *)
      if
        is_restart_boundary ~restart_interval:state.restart_interval
          ~mcu_count:!mcu_count
      then begin
        Bitstream.align_reader reader;
        Array.fill prev_dc 0 num_scan_components 0;
        Array.iter (fun r -> r := 0) prog_state.eob_run
      end;

      (* Process each component in scan *)
      for sci = 0 to num_scan_components - 1 do
        let scan_comp = scan.Markers.scan_components.(sci) in
        let ci = find_component_index components scan_comp.Markers.selector in
        let comp = components.(ci) in
        let dc_table = state.dc_tables.(scan_comp.Markers.dc_table) in
        let ac_table = state.ac_tables.(scan_comp.Markers.ac_table) in

        let h_blocks_total = mcus_x * comp.Markers.h_sampling in

        for v = 0 to comp.Markers.v_sampling - 1 do
          for h = 0 to comp.Markers.h_sampling - 1 do
            let block_idx = block_index ~mcu_x ~mcu_y ~h ~v
                ~h_sampling:comp.Markers.h_sampling
                ~v_sampling:comp.Markers.v_sampling ~h_blocks_total in
            let coeffs = prog_state.coefficients.(ci).(block_idx) in

            match scan_type with
            | DC_First ->
                let dc_coeff, new_dc =
                  decode_dc_first reader dc_table al prev_dc.(sci)
                in
                prev_dc.(sci) <- new_dc;
                coeffs.(0) <- dc_coeff
            | DC_Refine -> coeffs.(0) <- decode_dc_refine reader al coeffs.(0)
            | AC_First ->
                decode_ac_first reader ac_table ss se al coeffs
                  prog_state.eob_run.(ci)
            | AC_Refine ->
                decode_ac_refine reader ac_table ss se al coeffs
                  prog_state.eob_run.(ci)
          done
        done
      done;

      incr mcu_count
    done
  done

(** Finalize progressive decoding: dequantize and IDCT all blocks *)
let finalize_progressive frame prog_state quant_tables mcus_x mcus_y =
  let num_components = Array.length frame.Markers.components in

  let component_blocks =
    Array.init num_components (fun ci ->
        let comp = frame.Markers.components.(ci) in
        let h_blocks = mcus_x * comp.Markers.h_sampling in
        let v_blocks = mcus_y * comp.Markers.v_sampling in
        let num_blocks = h_blocks * v_blocks in
        let quant_table = quant_tables.(comp.Markers.quant_table_id) in

        Array.init num_blocks (fun block_idx ->
            let coeffs = prog_state.coefficients.(ci).(block_idx) in
            (* Dequantize *)
            let dequant =
              Quantization.dequantize coeffs quant_table.Markers.values
            in
            (* IDCT *)
            let spatial = Dct.idct dequant in
            (* Level shift and clamp *)
            Color.level_shift_block_up spatial))
  in

  component_blocks

(** Create initial decode state *)
let create_decode_state () =
  {
    frame = None;
    quant_tables =
      Array.make 4
        { Markers.table_id = 0; precision = 0; values = Array.make 64 16 };
    dc_tables = Array.make 4 (Huffman.std_dc_luminance_table ());
    ac_tables = Array.make 4 (Huffman.std_ac_luminance_table ());
    arith_dc_conditioning = Array.make 4 0;
    (* Default L=0 *)
    arith_ac_conditioning = Array.make 4 5;
    (* Default Kx=5 *)
    restart_interval = 0;
    exif = None;
    icc_chunks = [];
  }

(** Process marker segments and build decode state *)
let process_markers markers =
  let state = create_decode_state () in

  List.iter
    (fun segment ->
      match segment with
      | Markers.SOF0 frame -> state.frame <- Some frame
      | Markers.SOF2 frame -> state.frame <- Some frame
      | Markers.SOF3 frame -> state.frame <- Some frame
      | Markers.SOF9 frame -> state.frame <- Some frame
      | Markers.SOF10 frame -> state.frame <- Some frame
      | Markers.SOF11 frame -> state.frame <- Some frame
      | Markers.DQT tables ->
          List.iter
            (fun (qt : Markers.quant_table) ->
              state.quant_tables.(qt.table_id) <- qt)
            tables
      | Markers.DHT tables ->
          List.iter
            (fun (ht : Markers.huffman_table) ->
              let table = Huffman.build_table ht.counts ht.values in
              if ht.table_class = 0 then state.dc_tables.(ht.table_id) <- table
              else state.ac_tables.(ht.table_id) <- table)
            tables
      | Markers.DAC tables ->
          (* Process arithmetic conditioning tables *)
          List.iter
            (fun (ac : Markers.arithmetic_conditioning) ->
              if ac.table_class = 0 then
                (* DC conditioning: L value *)
                state.arith_dc_conditioning.(ac.table_id) <-
                  ac.conditioning_value
              else
                (* AC conditioning: Kx value *)
                state.arith_ac_conditioning.(ac.table_id) <-
                  ac.conditioning_value)
            tables
      | Markers.DRI interval -> state.restart_interval <- interval
      | Markers.APP1 data -> state.exif <- Some (Exif.parse data)
      | Markers.APP2_ICC { sequence; count; data } ->
          state.icc_chunks <- (sequence, count, data) :: state.icc_chunks
      | _ -> ())
    markers;

  state

(** Decode a single 8x8 block *)
let decode_block reader dc_table ac_table quant_table prev_dc =
  (* Decode DC coefficient *)
  let dc_category = Huffman.decode_symbol reader dc_table in
  let dc_diff =
    if dc_category = 0 then 0
    else
      let bits = Bitstream.read_bits reader dc_category in
      Huffman.extend bits dc_category
  in
  let dc_value = prev_dc + dc_diff in

  (* Decode AC coefficients *)
  let coeffs = Array.make 64 0 in
  coeffs.(0) <- dc_value;

  let i = ref 1 in
  while !i < 64 do
    let symbol = Huffman.decode_symbol reader ac_table in
    if symbol = 0x00 then begin
      (* EOB - rest are zeros *)
      i := 64
    end
    else if symbol = 0xF0 then begin
      (* ZRL - 16 zeros *)
      i := !i + 16
    end
    else begin
      let run_length = symbol lsr 4 in
      let ac_size = symbol land 0x0F in
      i := !i + run_length;
      if !i < 64 && ac_size > 0 then begin
        let bits = Bitstream.read_bits reader ac_size in
        coeffs.(!i) <- Huffman.extend bits ac_size
      end;
      incr i
    end
  done;

  (* Dequantize *)
  let dequant = Quantization.dequantize coeffs quant_table.Markers.values in

  (* Apply IDCT *)
  let spatial = Dct.idct dequant in

  (* Level shift and clamp *)
  (Color.level_shift_block_up spatial, dc_value)

(** Decode interleaved scan (multiple components) *)
let decode_interleaved_scan reader frame scan state =
  let num_components = Array.length scan.Markers.scan_components in
  let components = frame.Markers.components in

  (* Calculate MCU dimensions based on max sampling factors *)
  let _, _, _, _, mcus_x, mcus_y = calculate_mcu_dimensions frame in

  (* Allocate block storage for each component *)
  let component_blocks =
    Array.init num_components (fun i ->
        let comp = components.(i) in
        let h_blocks = mcus_x * comp.Markers.h_sampling in
        let v_blocks = mcus_y * comp.Markers.v_sampling in
        Array.make (h_blocks * v_blocks) (Array.make 64 0))
  in

  (* Previous DC values for each component *)
  let prev_dc = Array.make num_components 0 in

  (* Decode MCUs *)
  let mcu_count = ref 0 in

  for mcu_y = 0 to mcus_y - 1 do
    for mcu_x = 0 to mcus_x - 1 do
      (* Check for restart marker *)
      if
        is_restart_boundary ~restart_interval:state.restart_interval
          ~mcu_count:!mcu_count
      then begin
        Bitstream.align_reader reader;
        Array.fill prev_dc 0 num_components 0
      end;

      (* Decode each component's blocks within this MCU *)
      for ci = 0 to num_components - 1 do
        let comp = components.(ci) in
        let scan_comp = scan.Markers.scan_components.(ci) in
        let dc_table = state.dc_tables.(scan_comp.Markers.dc_table) in
        let ac_table = state.ac_tables.(scan_comp.Markers.ac_table) in
        let quant_table = state.quant_tables.(comp.Markers.quant_table_id) in

        let h_blocks_total = mcus_x * comp.Markers.h_sampling in

        for v = 0 to comp.Markers.v_sampling - 1 do
          for h = 0 to comp.Markers.h_sampling - 1 do
            let block_idx = block_index ~mcu_x ~mcu_y ~h ~v
                ~h_sampling:comp.Markers.h_sampling
                ~v_sampling:comp.Markers.v_sampling ~h_blocks_total in

            let block, new_dc =
              decode_block reader dc_table ac_table quant_table prev_dc.(ci)
            in
            prev_dc.(ci) <- new_dc;
            component_blocks.(ci).(block_idx) <- block
          done
        done
      done;

      incr mcu_count
    done
  done;

  component_blocks

(** Reconstruct image from decoded blocks *)
let reconstruct_image frame component_blocks =
  let width = frame.Markers.width in
  let height = frame.Markers.height in
  let num_components = Array.length frame.Markers.components in

  let max_h, _, _, _, mcus_x, mcus_y = calculate_mcu_dimensions frame in

  (* Create planes from blocks *)
  let planes =
    Array.init num_components (fun ci ->
        let comp = frame.Markers.components.(ci) in
        let h_blocks = mcus_x * comp.Markers.h_sampling in
        let v_blocks = mcus_y * comp.Markers.v_sampling in
        let plane_width = h_blocks * 8 in
        let plane_height = v_blocks * 8 in
        let plane = Array.make (plane_width * plane_height) 0 in

        for by = 0 to v_blocks - 1 do
          for bx = 0 to h_blocks - 1 do
            let block = component_blocks.(ci).((by * h_blocks) + bx) in
            for y = 0 to 7 do
              for x = 0 to 7 do
                let px = (bx * 8) + x in
                let py = (by * 8) + y in
                if px < plane_width && py < plane_height then
                  plane.((py * plane_width) + px) <- block.((y * 8) + x)
              done
            done
          done
        done;

        (plane, plane_width, plane_height))
  in

  (* Handle upsampling and color conversion *)
  if num_components = 1 then begin
    (* Grayscale *)
    let y_plane, _, _ = planes.(0) in
    let pixels =
      Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
        (width * height * 3)
    in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let v = y_plane.((y * (mcus_x * max_h * 8)) + x) in
        let idx = ((y * width) + x) * 3 in
        Bigarray.Array1.set pixels idx v;
        Bigarray.Array1.set pixels (idx + 1) v;
        Bigarray.Array1.set pixels (idx + 2) v
      done
    done;
    (pixels, RGB24)
  end
  else if num_components = 4 then begin
    (* 4-component: CMYK or YCCK *)
    let comp0 = frame.Markers.components.(0) in
    let plane0, p0_width, _ = planes.(0) in
    let plane1, p1_width, p1_height = planes.(1) in
    let plane2, p2_width, p2_height = planes.(2) in
    let plane3, p3_width, p3_height = planes.(3) in

    (* Upsample components 1, 2, 3 if needed *)
    let full_1 =
      if p1_width < p0_width then
        Color.upsample_420_bilinear plane1 p1_width p1_height p0_width
          (mcus_y * comp0.Markers.v_sampling * 8)
      else plane1
    in
    let full_2 =
      if p2_width < p0_width then
        Color.upsample_420_bilinear plane2 p2_width p2_height p0_width
          (mcus_y * comp0.Markers.v_sampling * 8)
      else plane2
    in
    let full_3 =
      if p3_width < p0_width then
        Color.upsample_420_bilinear plane3 p3_width p3_height p0_width
          (mcus_y * comp0.Markers.v_sampling * 8)
      else plane3
    in

    (* Return as CMYK32 - decoder doesn't know if it's CMYK or YCCK *)
    let pixels =
      Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
        (width * height * 4)
    in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let c = plane0.((y * p0_width) + x) in
        let m = full_1.((y * p0_width) + x) in
        let yy = full_2.((y * p0_width) + x) in
        let k = full_3.((y * p0_width) + x) in
        let idx = ((y * width) + x) * 4 in
        Bigarray.Array1.set pixels idx c;
        Bigarray.Array1.set pixels (idx + 1) m;
        Bigarray.Array1.set pixels (idx + 2) yy;
        Bigarray.Array1.set pixels (idx + 3) k
      done
    done;
    (pixels, CMYK32)
  end
  else begin
    (* YCbCr - need to upsample chroma if subsampled *)
    let comp0 = frame.Markers.components.(0) in
    let comp1 = frame.Markers.components.(1) in
    let comp2 = frame.Markers.components.(2) in

    let y_plane, y_width, y_height = planes.(0) in
    let cb_plane, cb_width, cb_height = planes.(1) in
    let cr_plane, cr_width, cr_height = planes.(2) in

    (* Upsample Cb and Cr if needed *)
    let full_cb =
      if
        comp1.Markers.h_sampling < comp0.Markers.h_sampling
        || comp1.Markers.v_sampling < comp0.Markers.v_sampling
      then
        Color.upsample_420_bilinear cb_plane cb_width cb_height y_width y_height
      else cb_plane
    in

    let full_cr =
      if
        comp2.Markers.h_sampling < comp0.Markers.h_sampling
        || comp2.Markers.v_sampling < comp0.Markers.v_sampling
      then
        Color.upsample_420_bilinear cr_plane cr_width cr_height y_width y_height
      else cr_plane
    in

    (* Convert to RGB *)
    let pixels =
      Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
        (width * height * 3)
    in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let y_val = y_plane.((y * y_width) + x) in
        let cb_val = full_cb.((y * y_width) + x) in
        let cr_val = full_cr.((y * y_width) + x) in
        let r, g, b = Color.ycbcr_to_rgb y_val cb_val cr_val in
        let idx = ((y * width) + x) * 3 in
        Bigarray.Array1.set pixels idx r;
        Bigarray.Array1.set pixels (idx + 1) g;
        Bigarray.Array1.set pixels (idx + 2) b
      done
    done;
    (pixels, RGB24)
  end

(* ============================================================================
   Arithmetic Coding Decoder (for SOF9 and SOF10)
   ============================================================================ *)

(** Decode a single 8x8 block using arithmetic coding *)
let decode_arith_block arith_state component_idx quant_table =
  (* Decode coefficients using arithmetic coder *)
  let coeffs = Arithmetic.decode_arith_block arith_state component_idx in

  (* Dequantize *)
  let dequant = Quantization.dequantize coeffs quant_table.Markers.values in

  (* Apply IDCT *)
  let spatial = Dct.idct dequant in

  (* Level shift and clamp *)
  Color.level_shift_block_up spatial

(** Decode interleaved arithmetic scan (multiple components) *)
let decode_arith_interleaved_scan entropy_data frame scan state =
  let num_components = Array.length scan.Markers.scan_components in
  let components = frame.Markers.components in

  (* Calculate MCU dimensions based on max sampling factors *)
  let _, _, _, _, mcus_x, mcus_y = calculate_mcu_dimensions frame in

  (* Initialize arithmetic decoder *)
  let arith_state =
    Arithmetic.init_arith_scan_decoder entropy_data num_components
  in

  (* Set conditioning values from DAC markers *)
  for ci = 0 to num_components - 1 do
    let scan_comp = scan.Markers.scan_components.(ci) in
    Arithmetic.set_conditioning arith_state ci true
      state.arith_dc_conditioning.(scan_comp.Markers.dc_table);
    Arithmetic.set_conditioning arith_state ci false
      state.arith_ac_conditioning.(scan_comp.Markers.ac_table)
  done;

  (* Allocate block storage for each component *)
  let component_blocks =
    Array.init num_components (fun i ->
        let comp = components.(i) in
        let h_blocks = mcus_x * comp.Markers.h_sampling in
        let v_blocks = mcus_y * comp.Markers.v_sampling in
        Array.make (h_blocks * v_blocks) (Array.make 64 0))
  in

  (* Decode MCUs *)
  let mcu_count = ref 0 in

  for mcu_y = 0 to mcus_y - 1 do
    for mcu_x = 0 to mcus_x - 1 do
      (* Check for restart marker *)
      if
        is_restart_boundary ~restart_interval:state.restart_interval
          ~mcu_count:!mcu_count
      then begin
        Arithmetic.reset_arith_decoder arith_state
      end;

      (* Decode each component's blocks within this MCU *)
      for ci = 0 to num_components - 1 do
        let comp = components.(ci) in
        let quant_table = state.quant_tables.(comp.Markers.quant_table_id) in

        let h_blocks_total = mcus_x * comp.Markers.h_sampling in

        for v = 0 to comp.Markers.v_sampling - 1 do
          for h = 0 to comp.Markers.h_sampling - 1 do
            let block_idx = block_index ~mcu_x ~mcu_y ~h ~v
                ~h_sampling:comp.Markers.h_sampling
                ~v_sampling:comp.Markers.v_sampling ~h_blocks_total in

            let block = decode_arith_block arith_state ci quant_table in
            component_blocks.(ci).(block_idx) <- block
          done
        done
      done;

      incr mcu_count
    done
  done;

  component_blocks

(** Decode arithmetic baseline JPEG (SOF9 - single scan) *)
let decode_arith_baseline markers frame state =
  (* Find and decode scan data *)
  let scan_data =
    List.find_map
      (fun m ->
        match m with
        | Markers.SOS (header, data) -> Some (header, data)
        | _ -> None)
      markers
  in

  match scan_data with
  | None -> failwith "No scan data found"
  | Some (scan_header, entropy_data) ->
      let component_blocks =
        decode_arith_interleaved_scan entropy_data frame scan_header state
      in
      reconstruct_image frame component_blocks

(** Initialize arithmetic progressive state *)
let init_arith_progressive_state frame mcus_x mcus_y =
  let num_components = Array.length frame.Markers.components in
  let coefficients =
    Array.init num_components (fun ci ->
        let comp = frame.Markers.components.(ci) in
        let h_blocks = mcus_x * comp.Markers.h_sampling in
        let v_blocks = mcus_y * comp.Markers.v_sampling in
        let num_blocks = h_blocks * v_blocks in
        Array.init num_blocks (fun _ -> Array.make 64 0))
  in
  coefficients

(** Decode arithmetic DC scan (first or refining) *)
let decode_arith_dc_scan entropy_data frame scan state coefficients mcus_x
    mcus_y =
  let num_scan_components = Array.length scan.Markers.scan_components in
  let components = frame.Markers.components in
  let al = scan.Markers.al in

  (* Initialize arithmetic decoder for this scan *)
  let arith_state =
    Arithmetic.init_arith_scan_decoder entropy_data num_scan_components
  in

  (* Set conditioning values *)
  for sci = 0 to num_scan_components - 1 do
    let scan_comp = scan.Markers.scan_components.(sci) in
    Arithmetic.set_conditioning arith_state sci true
      state.arith_dc_conditioning.(scan_comp.Markers.dc_table)
  done;

  let mcu_count = ref 0 in

  for mcu_y = 0 to mcus_y - 1 do
    for mcu_x = 0 to mcus_x - 1 do
      if
        is_restart_boundary ~restart_interval:state.restart_interval
          ~mcu_count:!mcu_count
      then Arithmetic.reset_arith_decoder arith_state;

      for sci = 0 to num_scan_components - 1 do
        let scan_comp = scan.Markers.scan_components.(sci) in
        let ci = find_component_index components scan_comp.Markers.selector in
        let comp = components.(ci) in

        let h_blocks_total = mcus_x * comp.Markers.h_sampling in

        for v = 0 to comp.Markers.v_sampling - 1 do
          for h = 0 to comp.Markers.h_sampling - 1 do
            let block_idx = block_index ~mcu_x ~mcu_y ~h ~v
                ~h_sampling:comp.Markers.h_sampling
                ~v_sampling:comp.Markers.v_sampling ~h_blocks_total in

            (* Decode DC coefficient *)
            let dc_bins = arith_state.Arithmetic.dc_bins.(sci) in
            let prev_dc = arith_state.Arithmetic.prev_dc.(sci) in
            let l = arith_state.Arithmetic.l.(sci) in
            let u = arith_state.Arithmetic.u.(sci) in
            let dc_ctx = arith_state.Arithmetic.dc_context.(sci) in
            let dc_diff, new_dc_ctx =
              Arithmetic.decode_dc_diff arith_state.Arithmetic.decoder dc_bins
                dc_ctx l u
            in
            let dc_value = prev_dc + dc_diff in
            arith_state.Arithmetic.prev_dc.(sci) <- dc_value;
            arith_state.Arithmetic.dc_context.(sci) <- new_dc_ctx;

            (* Apply successive approximation *)
            coefficients.(ci).(block_idx).(0) <- dc_value lsl al
          done
        done
      done;

      incr mcu_count
    done
  done

(** Decode arithmetic AC scan *)
let decode_arith_ac_scan entropy_data frame scan state coefficients mcus_x
    mcus_y =
  let num_scan_components = Array.length scan.Markers.scan_components in
  let components = frame.Markers.components in
  let ss = scan.Markers.ss in
  let se = scan.Markers.se in
  let al = scan.Markers.al in

  (* Initialize arithmetic decoder *)
  let arith_state =
    Arithmetic.init_arith_scan_decoder entropy_data num_scan_components
  in

  (* Set conditioning values *)
  for sci = 0 to num_scan_components - 1 do
    let scan_comp = scan.Markers.scan_components.(sci) in
    Arithmetic.set_conditioning arith_state sci false
      state.arith_ac_conditioning.(scan_comp.Markers.ac_table)
  done;

  let mcu_count = ref 0 in

  for mcu_y = 0 to mcus_y - 1 do
    for mcu_x = 0 to mcus_x - 1 do
      if
        is_restart_boundary ~restart_interval:state.restart_interval
          ~mcu_count:!mcu_count
      then Arithmetic.reset_arith_decoder arith_state;

      for sci = 0 to num_scan_components - 1 do
        let scan_comp = scan.Markers.scan_components.(sci) in
        let ci = find_component_index components scan_comp.Markers.selector in
        let comp = components.(ci) in

        let h_blocks_total = mcus_x * comp.Markers.h_sampling in

        for v = 0 to comp.Markers.v_sampling - 1 do
          for h = 0 to comp.Markers.h_sampling - 1 do
            let block_idx = block_index ~mcu_x ~mcu_y ~h ~v
                ~h_sampling:comp.Markers.h_sampling
                ~v_sampling:comp.Markers.v_sampling ~h_blocks_total in
            let block_coeffs = coefficients.(ci).(block_idx) in

            (* Decode AC coefficients for this spectral range *)
            Arithmetic.decode_ac_coefficients
              arith_state.Arithmetic.decoder
              arith_state.Arithmetic.ac_bins.(sci)
              arith_state.Arithmetic.fixed_bin
              arith_state.Arithmetic.kx.(sci)
              block_coeffs ~start_k:ss ~end_k:se ~shift:al
          done
        done
      done;

      incr mcu_count
    done
  done

(** Decode arithmetic progressive JPEG (SOF10 - multiple scans) *)
let decode_arith_progressive markers frame state =
  let _, _, _, _, mcus_x, mcus_y = calculate_mcu_dimensions frame in

  (* Initialize coefficient storage *)
  let coefficients = init_arith_progressive_state frame mcus_x mcus_y in

  (* Process each scan *)
  List.iter
    (fun marker ->
      match marker with
      | Markers.DAC tables ->
          (* Update conditioning values *)
          List.iter
            (fun (ac : Markers.arithmetic_conditioning) ->
              if ac.table_class = 0 then
                state.arith_dc_conditioning.(ac.table_id) <-
                  ac.conditioning_value
              else
                state.arith_ac_conditioning.(ac.table_id) <-
                  ac.conditioning_value)
            tables
      | Markers.SOS (scan_header, entropy_data) ->
          let is_dc_scan =
            scan_header.Markers.ss = 0 && scan_header.Markers.se = 0
          in
          if is_dc_scan then
            decode_arith_dc_scan entropy_data frame scan_header state
              coefficients mcus_x mcus_y
          else
            decode_arith_ac_scan entropy_data frame scan_header state
              coefficients mcus_x mcus_y
      | _ -> ())
    markers;

  (* Finalize: dequantize and IDCT all blocks *)
  let num_components = Array.length frame.Markers.components in
  let component_blocks =
    Array.init num_components (fun ci ->
        let comp = frame.Markers.components.(ci) in
        let h_blocks = mcus_x * comp.Markers.h_sampling in
        let v_blocks = mcus_y * comp.Markers.v_sampling in
        let num_blocks = h_blocks * v_blocks in
        let quant_table = state.quant_tables.(comp.Markers.quant_table_id) in

        Array.init num_blocks (fun block_idx ->
            let coeffs = coefficients.(ci).(block_idx) in
            (* Dequantize *)
            let dequant =
              Quantization.dequantize coeffs quant_table.Markers.values
            in
            (* IDCT *)
            let spatial = Dct.idct dequant in
            (* Level shift and clamp *)
            Color.level_shift_block_up spatial))
  in

  reconstruct_image frame component_blocks

(* ============================================================================
   Lossless JPEG Decoder (for SOF3 and SOF11)
   ============================================================================ *)

(** Decode a single difference value using Huffman coding for lossless JPEG.
    In lossless mode, the DC table is used for all samples (there are no AC coefficients). *)
let decode_lossless_diff_huffman reader dc_table =
  let category = Huffman.decode_symbol reader dc_table in
  if category = 0 then 0
  else if category = 16 then
    (* Category 16 represents 32768, which is -32768 in two's complement *)
    -32768
  else
    let bits = Bitstream.read_bits reader category in
    Huffman.extend bits category

(** Decode lossless JPEG scan using Huffman coding.
    Returns pixel planes for each component. *)
let decode_lossless_huffman_scan reader frame scan state =
  let width = frame.Markers.width in
  let height = frame.Markers.height in
  let precision = frame.Markers.precision in
  let num_components = Array.length scan.Markers.scan_components in

  (* Get predictor selection and point transform from scan header *)
  let predictor_sel = scan.Markers.ss in
  let point_transform = scan.Markers.al in
  let predictor = Predictor.predictor_of_int predictor_sel in

  (* For lossless JPEG, there's no subsampling - each component has same dimensions *)
  (* Allocate pixel storage for each component *)
  let component_planes =
    Array.init num_components (fun _ ->
        Array.make (width * height) 0)
  in

  (* Previous row buffer for prediction *)
  let prev_rows =
    Array.init num_components (fun _ ->
        Array.make width 0)
  in

  let mcu_count = ref 0 in

  (* Decode row by row, pixel by pixel *)
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      (* Check for restart marker at row boundaries or after restart interval *)
      if is_restart_boundary ~restart_interval:state.restart_interval
           ~mcu_count:!mcu_count then begin
        Bitstream.align_reader reader;
        (* Reset prediction context - fill prev_rows with default value *)
        let default_val = 1 lsl (precision - point_transform - 1) in
        Array.iter (fun row -> Array.fill row 0 width default_val) prev_rows
      end;

      (* Decode each component *)
      for ci = 0 to num_components - 1 do
        let scan_comp = scan.Markers.scan_components.(ci) in
        let dc_table = state.dc_tables.(scan_comp.Markers.dc_table) in
        let plane = component_planes.(ci) in
        let prev_row = prev_rows.(ci) in

        (* Get neighbor values for prediction *)
        let ra = if x > 0 then plane.((y * width) + x - 1) else 0 in
        let rb = if y > 0 then prev_row.(x) else 0 in
        let rc = if x > 0 && y > 0 then
          if x = 1 then prev_row.(0) else prev_row.(x - 1)
        else 0 in

        (* Calculate prediction *)
        let predicted = Predictor.predict predictor
            ~ra ~rb ~rc ~x ~y ~precision ~point_transform in

        (* Decode difference *)
        let diff = decode_lossless_diff_huffman reader dc_table in

        (* Reconstruct sample value *)
        let sample = Predictor.reconstruct ~predicted ~diff ~precision ~point_transform in

        (* Store the sample *)
        plane.((y * width) + x) <- sample
      done;

      incr mcu_count
    done;

    (* Copy current row to prev_row for next iteration *)
    for ci = 0 to num_components - 1 do
      let plane = component_planes.(ci) in
      let prev_row = prev_rows.(ci) in
      for xx = 0 to width - 1 do
        prev_row.(xx) <- plane.((y * width) + xx)
      done
    done
  done;

  component_planes

(** Decode a single difference value using arithmetic coding for lossless JPEG *)
let decode_lossless_diff_arithmetic (arith_state : Arithmetic.arith_scan_state) component_idx =
  let dc_bins = arith_state.Arithmetic.dc_bins.(component_idx) in
  let l = arith_state.Arithmetic.l.(component_idx) in
  let u = arith_state.Arithmetic.u.(component_idx) in
  let dc_ctx = arith_state.Arithmetic.dc_context.(component_idx) in
  let diff, new_dc_ctx =
    Arithmetic.decode_dc_diff arith_state.Arithmetic.decoder dc_bins dc_ctx l u
  in
  arith_state.Arithmetic.dc_context.(component_idx) <- new_dc_ctx;
  arith_state.Arithmetic.prev_dc.(component_idx) <- diff;
  diff

(** Decode lossless JPEG scan using arithmetic coding *)
let decode_lossless_arithmetic_scan entropy_data frame scan state =
  let width = frame.Markers.width in
  let height = frame.Markers.height in
  let precision = frame.Markers.precision in
  let num_components = Array.length scan.Markers.scan_components in

  (* Get predictor selection and point transform from scan header *)
  let predictor_sel = scan.Markers.ss in
  let point_transform = scan.Markers.al in
  let predictor = Predictor.predictor_of_int predictor_sel in

  (* Initialize arithmetic decoder *)
  let arith_state =
    Arithmetic.init_arith_scan_decoder entropy_data num_components
  in

  (* Set conditioning values from DAC markers *)
  for ci = 0 to num_components - 1 do
    let scan_comp = scan.Markers.scan_components.(ci) in
    Arithmetic.set_conditioning arith_state ci true
      state.arith_dc_conditioning.(scan_comp.Markers.dc_table)
  done;

  (* Allocate pixel storage for each component *)
  let component_planes =
    Array.init num_components (fun _ ->
        Array.make (width * height) 0)
  in

  (* Previous row buffer for prediction *)
  let prev_rows =
    Array.init num_components (fun _ ->
        Array.make width 0)
  in

  let mcu_count = ref 0 in

  (* Decode row by row, pixel by pixel *)
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      (* Check for restart marker *)
      if is_restart_boundary ~restart_interval:state.restart_interval
           ~mcu_count:!mcu_count then begin
        Arithmetic.reset_arith_decoder arith_state;
        let default_val = 1 lsl (precision - point_transform - 1) in
        Array.iter (fun row -> Array.fill row 0 width default_val) prev_rows
      end;

      (* Decode each component *)
      for ci = 0 to num_components - 1 do
        let plane = component_planes.(ci) in
        let prev_row = prev_rows.(ci) in

        (* Get neighbor values for prediction *)
        let ra = if x > 0 then plane.((y * width) + x - 1) else 0 in
        let rb = if y > 0 then prev_row.(x) else 0 in
        let rc = if x > 0 && y > 0 then
          if x = 1 then prev_row.(0) else prev_row.(x - 1)
        else 0 in

        (* Calculate prediction *)
        let predicted = Predictor.predict predictor
            ~ra ~rb ~rc ~x ~y ~precision ~point_transform in

        (* Decode difference *)
        let diff = decode_lossless_diff_arithmetic arith_state ci in

        (* Reconstruct sample value *)
        let sample = Predictor.reconstruct ~predicted ~diff ~precision ~point_transform in

        (* Store the sample *)
        plane.((y * width) + x) <- sample
      done;

      incr mcu_count
    done;

    (* Copy current row to prev_row for next iteration *)
    for ci = 0 to num_components - 1 do
      let plane = component_planes.(ci) in
      let prev_row = prev_rows.(ci) in
      for xx = 0 to width - 1 do
        prev_row.(xx) <- plane.((y * width) + xx)
      done
    done
  done;

  component_planes

(** Reconstruct image from lossless decoded planes *)
let reconstruct_lossless_image frame component_planes =
  let width = frame.Markers.width in
  let height = frame.Markers.height in
  let num_components = Array.length frame.Markers.components in
  let precision = frame.Markers.precision in

  (* Scale values if precision > 8 bits *)
  let scale_value v =
    if precision <= 8 then v
    else v lsr (precision - 8)  (* Scale down to 8 bits *)
  in

  if num_components = 1 then begin
    (* Grayscale *)
    let y_plane = component_planes.(0) in
    let pixels =
      Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
        (width * height * 3)
    in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let v = scale_value y_plane.((y * width) + x) in
        let v = max 0 (min 255 v) in
        let idx = ((y * width) + x) * 3 in
        Bigarray.Array1.set pixels idx v;
        Bigarray.Array1.set pixels (idx + 1) v;
        Bigarray.Array1.set pixels (idx + 2) v
      done
    done;
    (pixels, RGB24)
  end
  else if num_components = 3 then begin
    (* RGB - lossless JPEG typically stores RGB directly, not YCbCr *)
    let r_plane = component_planes.(0) in
    let g_plane = component_planes.(1) in
    let b_plane = component_planes.(2) in
    let pixels =
      Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
        (width * height * 3)
    in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let pos = (y * width) + x in
        let r = max 0 (min 255 (scale_value r_plane.(pos))) in
        let g = max 0 (min 255 (scale_value g_plane.(pos))) in
        let b = max 0 (min 255 (scale_value b_plane.(pos))) in
        let idx = pos * 3 in
        Bigarray.Array1.set pixels idx r;
        Bigarray.Array1.set pixels (idx + 1) g;
        Bigarray.Array1.set pixels (idx + 2) b
      done
    done;
    (pixels, RGB24)
  end
  else if num_components = 4 then begin
    (* CMYK *)
    let c_plane = component_planes.(0) in
    let m_plane = component_planes.(1) in
    let y_plane = component_planes.(2) in
    let k_plane = component_planes.(3) in
    let pixels =
      Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
        (width * height * 4)
    in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let pos = (y * width) + x in
        let c = max 0 (min 255 (scale_value c_plane.(pos))) in
        let m = max 0 (min 255 (scale_value m_plane.(pos))) in
        let yy = max 0 (min 255 (scale_value y_plane.(pos))) in
        let k = max 0 (min 255 (scale_value k_plane.(pos))) in
        let idx = pos * 4 in
        Bigarray.Array1.set pixels idx c;
        Bigarray.Array1.set pixels (idx + 1) m;
        Bigarray.Array1.set pixels (idx + 2) yy;
        Bigarray.Array1.set pixels (idx + 3) k
      done
    done;
    (pixels, CMYK32)
  end
  else
    failwith (Printf.sprintf "Unsupported number of components: %d" num_components)

(** Decode lossless JPEG with Huffman coding (SOF3) *)
let decode_lossless_huffman markers frame state =
  (* Find and decode scan data *)
  let scan_data =
    List.find_map
      (fun m ->
        match m with
        | Markers.SOS (header, data) -> Some (header, data)
        | _ -> None)
      markers
  in

  match scan_data with
  | None -> failwith "No scan data found"
  | Some (scan_header, entropy_data) ->
      let reader = Bitstream.create_reader entropy_data in
      let component_planes =
        decode_lossless_huffman_scan reader frame scan_header state
      in
      reconstruct_lossless_image frame component_planes

(** Decode lossless JPEG with arithmetic coding (SOF11) *)
let decode_lossless_arithmetic markers frame state =
  (* Find and decode scan data *)
  let scan_data =
    List.find_map
      (fun m ->
        match m with
        | Markers.SOS (header, data) -> Some (header, data)
        | _ -> None)
      markers
  in

  match scan_data with
  | None -> failwith "No scan data found"
  | Some (scan_header, entropy_data) ->
      let component_planes =
        decode_lossless_arithmetic_scan entropy_data frame scan_header state
      in
      reconstruct_lossless_image frame component_planes

(** Decode baseline JPEG (single scan) *)
let decode_baseline markers frame state =
  (* Find and decode scan data *)
  let scan_data =
    List.find_map
      (fun m ->
        match m with
        | Markers.SOS (header, data) -> Some (header, data)
        | _ -> None)
      markers
  in

  match scan_data with
  | None -> failwith "No scan data found"
  | Some (scan_header, entropy_data) ->
      let reader = Bitstream.create_reader entropy_data in
      let component_blocks =
        decode_interleaved_scan reader frame scan_header state
      in
      reconstruct_image frame component_blocks

(** Decode progressive JPEG (multiple scans) *)
let decode_progressive markers frame state =
  (* Calculate MCU dimensions *)
  let _, _, _, _, mcus_x, mcus_y = calculate_mcu_dimensions frame in

  (* Initialize progressive state *)
  let prog_state = init_progressive_state frame mcus_x mcus_y in

  (* Process markers in order, updating Huffman tables as we go *)
  List.iter
    (fun marker ->
      match marker with
      | Markers.DHT tables ->
          (* Update Huffman tables before next scan *)
          List.iter
            (fun (ht : Markers.huffman_table) ->
              let table = Huffman.build_table ht.counts ht.values in
              if ht.table_class = 0 then state.dc_tables.(ht.table_id) <- table
              else state.ac_tables.(ht.table_id) <- table)
            tables
      | Markers.SOS (scan_header, entropy_data) ->
          let reader = Bitstream.create_reader entropy_data in
          decode_progressive_scan reader frame scan_header state prog_state
            mcus_x mcus_y
      | _ -> ())
    markers;

  (* Finalize: dequantize and IDCT all blocks *)
  let component_blocks =
    finalize_progressive frame prog_state state.quant_tables mcus_x mcus_y
  in

  reconstruct_image frame component_blocks

(** Decode JPEG from bytes *)
let read_bytes data =
  let markers = Markers.parse_markers data in
  let state = process_markers markers in

  match state.frame with
  | None -> failwith "No SOF marker found"
  | Some frame ->
      if frame.Markers.precision <> 8 && frame.Markers.precision <> 12 then
        failwith "Only 8-bit and 12-bit precision supported";

      let pixels, pixel_format =
        match frame.Markers.frame_type with
        | Markers.Baseline -> decode_baseline markers frame state
        | Markers.Progressive -> decode_progressive markers frame state
        | Markers.ArithmeticSequential ->
            decode_arith_baseline markers frame state
        | Markers.ArithmeticProgressive ->
            decode_arith_progressive markers frame state
        | Markers.LosslessHuffman ->
            decode_lossless_huffman markers frame state
        | Markers.LosslessArithmetic ->
            decode_lossless_arithmetic markers frame state
      in

      (* Reconstruct ICC profile from chunks if present *)
      let icc_profile = Icc.from_chunks state.icc_chunks in

      {
        width = frame.Markers.width;
        height = frame.Markers.height;
        pixels;
        pixel_format;
        exif = state.exif;
        icc_profile;
      }

(** Read JPEG from file *)
let read filename =
  let ic = open_in_bin filename in
  let len = in_channel_length ic in
  let data = Bytes.create len in
  really_input ic data 0 len;
  close_in ic;
  read_bytes data

(* === ENCODING === *)

(** Transform and quantize a block (level shift -> DCT -> quantize) Returns
    quantized coefficients in zig-zag order *)
let transform_and_quantize block quant_table =
  (* Level shift *)
  let shifted = Color.level_shift_block_down block in
  (* Forward DCT *)
  let dct_coeffs = Dct.fdct shifted in
  (* Quantize (returns zig-zag ordered) *)
  Quantization.quantize dct_coeffs quant_table

(** Encode DC coefficient, returns new DC value for prediction *)
let encode_dc writer dc_table dc_value prev_dc =
  let dc_diff = dc_value - prev_dc in
  let dc_category = Huffman.category dc_diff in
  Huffman.encode_symbol writer dc_table dc_category;
  if dc_category > 0 then
    Bitstream.write_bits writer
      (Huffman.encode_value dc_diff dc_category)
      dc_category;
  dc_value

(** Encode AC coefficients from pre-quantized data *)
let encode_ac writer ac_table quantized =
  let run_length = ref 0 in
  for i = 1 to 63 do
    let ac_value = quantized.(i) in
    if ac_value = 0 then incr run_length
    else begin
      (* Emit ZRL for runs > 15 *)
      while !run_length > 15 do
        Huffman.encode_symbol writer ac_table 0xF0;
        run_length := !run_length - 16
      done;
      (* Encode run/size and value *)
      let ac_category = Huffman.category ac_value in
      let symbol = (!run_length lsl 4) lor ac_category in
      Huffman.encode_symbol writer ac_table symbol;
      Bitstream.write_bits writer
        (Huffman.encode_value ac_value ac_category)
        ac_category;
      run_length := 0
    end
  done;
  (* EOB if we ended with zeros *)
  if !run_length > 0 then Huffman.encode_symbol writer ac_table 0x00

(** Encode a block from pre-quantized coefficients *)
let encode_block_from_coeffs writer quantized dc_table ac_table prev_dc =
  let dc_value = quantized.(0) in
  let new_dc = encode_dc writer dc_table dc_value prev_dc in
  encode_ac writer ac_table quantized;
  new_dc

(** Encode a single 8x8 block *)
let encode_block writer block dc_table ac_table quant_table prev_dc =
  (* Level shift *)
  let shifted = Color.level_shift_block_down block in

  (* Forward DCT *)
  let dct_coeffs = Dct.fdct shifted in

  (* Quantize (returns zig-zag ordered) *)
  let quantized = Quantization.quantize dct_coeffs quant_table in

  (* Encode DC coefficient *)
  let dc_value = quantized.(0) in
  let dc_diff = dc_value - prev_dc in
  let dc_category = Huffman.category dc_diff in
  Huffman.encode_symbol writer dc_table dc_category;
  if dc_category > 0 then
    Bitstream.write_bits writer
      (Huffman.encode_value dc_diff dc_category)
      dc_category;

  (* Encode AC coefficients *)
  let run_length = ref 0 in
  for i = 1 to 63 do
    let ac_value = quantized.(i) in
    if ac_value = 0 then incr run_length
    else begin
      (* Emit ZRL for runs > 15 *)
      while !run_length > 15 do
        Huffman.encode_symbol writer ac_table 0xF0;
        (* ZRL *)
        run_length := !run_length - 16
      done;

      (* Encode run/size and value *)
      let ac_category = Huffman.category ac_value in
      let symbol = (!run_length lsl 4) lor ac_category in
      Huffman.encode_symbol writer ac_table symbol;
      Bitstream.write_bits writer
        (Huffman.encode_value ac_value ac_category)
        ac_category;
      run_length := 0
    end
  done;

  (* EOB if we ended with zeros *)
  if !run_length > 0 then Huffman.encode_symbol writer ac_table 0x00;

  dc_value

(** Extract 8x8 block from a plane *)
let extract_block plane plane_width plane_height bx by =
  let block = Array.make 64 128 in
  (* Default to mid-gray for padding *)
  for y = 0 to 7 do
    for x = 0 to 7 do
      let px = (bx * 8) + x in
      let py = (by * 8) + y in
      if px < plane_width && py < plane_height then
        block.((y * 8) + x) <- plane.((py * plane_width) + px)
    done
  done;
  block

(* === PROGRESSIVE ENCODING === *)

type scan_spec = {
  components : int list; (* Component indices to include *)
  ss : int; (* Start of spectral selection *)
  se : int; (* End of spectral selection *)
  ah : int; (* Successive approximation high *)
  al : int; (* Successive approximation low *)
}
(** Scan specification for progressive encoding *)

(** Default progressive scan pattern - standard compliant.
    DC scans can include multiple components, but AC scans (ss > 0) must be
    single-component per JPEG spec (ITU-T T.81 section B.2.3). *)
let default_progressive_scans =
  [
    (* DC for all components - shows blocky preview immediately *)
    { components = [ 0; 1; 2 ]; ss = 0; se = 0; ah = 0; al = 0 };
    (* Y (luminance) AC coefficients 1-5 *)
    { components = [ 0 ]; ss = 1; se = 5; ah = 0; al = 0 };
    (* Cb AC coefficients 1-63 *)
    { components = [ 1 ]; ss = 1; se = 63; ah = 0; al = 0 };
    (* Cr AC coefficients 1-63 *)
    { components = [ 2 ]; ss = 1; se = 63; ah = 0; al = 0 };
    (* Y AC coefficients 6-63 - completes image *)
    { components = [ 0 ]; ss = 6; se = 63; ah = 0; al = 0 };
  ]

(** Grayscale progressive scan pattern *)
let grayscale_progressive_scans =
  [
    { components = [ 0 ]; ss = 0; se = 0; ah = 0; al = 0 };
    { components = [ 0 ]; ss = 1; se = 5; ah = 0; al = 0 };
    { components = [ 0 ]; ss = 6; se = 63; ah = 0; al = 0 };
  ]

(** Encode DC coefficient for first DC scan (DC_First) *)
let encode_dc_first writer dc_table dc_value al prev_dc =
  let shifted_dc = dc_value asr al in
  let shifted_prev = prev_dc asr al in
  let dc_diff = shifted_dc - shifted_prev in
  let dc_category = Huffman.category dc_diff in
  Huffman.encode_symbol writer dc_table dc_category;
  if dc_category > 0 then
    Bitstream.write_bits writer
      (Huffman.encode_value dc_diff dc_category)
      dc_category;
  dc_value

(** Encode AC coefficients for first AC scan (AC_First) Using simple EOB (0x00)
    for each zero block - standard tables don't support extended EOBn *)
let encode_ac_first writer ac_table coeffs ss se al =
  (* Check if all coefficients in range are zero *)
  let all_zero = ref true in
  for k = ss to se do
    let coeff = coeffs.(k) asr al in
    if coeff <> 0 then all_zero := false
  done;

  if !all_zero then begin
    (* Emit EOB for this block *)
    Huffman.encode_symbol writer ac_table 0x00
  end
  else begin
    let run_length = ref 0 in
    for k = ss to se do
      let coeff = coeffs.(k) asr al in
      if coeff = 0 then incr run_length
      else begin
        (* Emit ZRL for runs > 15 *)
        while !run_length > 15 do
          Huffman.encode_symbol writer ac_table 0xF0;
          run_length := !run_length - 16
        done;
        (* Encode run/size and value *)
        let ac_category = Huffman.category coeff in
        let symbol = (!run_length lsl 4) lor ac_category in
        Huffman.encode_symbol writer ac_table symbol;
        Bitstream.write_bits writer
          (Huffman.encode_value coeff ac_category)
          ac_category;
        run_length := 0
      end
    done;
    (* If we ended with zeros, emit EOB *)
    if !run_length > 0 then Huffman.encode_symbol writer ac_table 0x00
  end

(** Encode a progressive DC scan *)
let encode_progressive_dc_scan coefficients h_blocks num_blocks dc_tables
    scan_spec mcu_h mcu_v y_h_sampling y_v_sampling cb_h_sampling cb_v_sampling
    ~restart_interval =
  let writer = Bitstream.create_writer () in
  let al = scan_spec.al in
  let num_components = List.length scan_spec.components in
  let prev_dc = Array.make num_components 0 in
  let mcu_count = ref 0 in
  let rst_counter = ref 0 in

  for mcu_y = 0 to mcu_v - 1 do
    for mcu_x = 0 to mcu_h - 1 do
      (* Check for restart marker insertion *)
      if
        is_restart_boundary ~restart_interval ~mcu_count:!mcu_count
      then begin
        Bitstream.write_rst_marker writer !rst_counter;
        rst_counter := (!rst_counter + 1) land 0x07;
        Array.fill prev_dc 0 num_components 0
      end;

      List.iteri
        (fun sci ci ->
          let dc_table = dc_tables.(if ci = 0 then 0 else 1) in
          let h_sampling = if ci = 0 then y_h_sampling else cb_h_sampling in
          let v_sampling = if ci = 0 then y_v_sampling else cb_v_sampling in

          for v = 0 to v_sampling - 1 do
            for h = 0 to h_sampling - 1 do
              let block_idx = block_index ~mcu_x ~mcu_y ~h ~v
                  ~h_sampling ~v_sampling ~h_blocks_total:h_blocks.(ci) in
              if block_idx < num_blocks.(ci) then begin
                let coeffs = coefficients.(ci).(block_idx) in
                prev_dc.(sci) <-
                  encode_dc_first writer dc_table coeffs.(0) al prev_dc.(sci)
              end
            done
          done)
        scan_spec.components;
      incr mcu_count
    done
  done;

  Bitstream.flush_writer writer;
  Bitstream.get_bytes writer

(** Encode a progressive AC scan *)
let encode_progressive_ac_scan coefficients h_blocks num_blocks ac_tables
    scan_spec mcu_h mcu_v y_h_sampling y_v_sampling cb_h_sampling cb_v_sampling
    ~restart_interval =
  let writer = Bitstream.create_writer () in
  let ss = scan_spec.ss in
  let se = scan_spec.se in
  let al = scan_spec.al in
  let mcu_count = ref 0 in
  let rst_counter = ref 0 in

  for mcu_y = 0 to mcu_v - 1 do
    for mcu_x = 0 to mcu_h - 1 do
      (* Check for restart marker insertion *)
      if
        is_restart_boundary ~restart_interval ~mcu_count:!mcu_count
      then begin
        Bitstream.write_rst_marker writer !rst_counter;
        rst_counter := (!rst_counter + 1) land 0x07
      end;

      List.iter
        (fun ci ->
          let ac_table = ac_tables.(if ci = 0 then 0 else 1) in
          let h_sampling = if ci = 0 then y_h_sampling else cb_h_sampling in
          let v_sampling = if ci = 0 then y_v_sampling else cb_v_sampling in

          for v = 0 to v_sampling - 1 do
            for h = 0 to h_sampling - 1 do
              let block_idx = block_index ~mcu_x ~mcu_y ~h ~v
                  ~h_sampling ~v_sampling ~h_blocks_total:h_blocks.(ci) in
              if block_idx < num_blocks.(ci) then begin
                let coeffs = coefficients.(ci).(block_idx) in
                encode_ac_first writer ac_table coeffs ss se al
              end
            done
          done)
        scan_spec.components;
      incr mcu_count
    done
  done;

  Bitstream.flush_writer writer;
  Bitstream.get_bytes writer

(** Build initial markers (SOI + APP0 + optional EXIF/ICC) *)
let build_initial_markers (image : image) =
  [ Markers.SOI;
    Markers.APP0
      { version_major = 1; version_minor = 1; density_units = 0;
        x_density = 1; y_density = 1;
        thumbnail_width = 0; thumbnail_height = 0 } ]
  @ (match image.exif with
    | Some exif -> [ Markers.APP1 (Exif.to_bytes exif) ]
    | None -> [])
  @ (match image.icc_profile with
    | Some icc ->
        List.map
          (fun (seq, count, data) ->
            Markers.APP2_ICC { sequence = seq; count; data })
          (Icc.to_chunks icc)
    | None -> [])

(** Extract raw pixel planes for lossless encoding.
    For grayscale: 1 plane with luminance. For 4-component: 4 CMYK planes.
    For color: 3 RGB planes (stored directly, not YCbCr). *)
let extract_lossless_planes (image : image) ~is_grayscale ~is_4_component =
  let width = image.width in
  let height = image.height in
  let size = width * height in
  if is_grayscale then begin
    let plane = Array.make size 0 in
    (match image.pixel_format with
    | RGB24 ->
        for i = 0 to size - 1 do
          let r = Bigarray.Array1.get image.pixels (i * 3) in
          let g = Bigarray.Array1.get image.pixels ((i * 3) + 1) in
          let b = Bigarray.Array1.get image.pixels ((i * 3) + 2) in
          plane.(i) <- (r * 77 + g * 150 + b * 29) / 256
        done
    | CMYK32 ->
        for i = 0 to size - 1 do
          let c = Bigarray.Array1.get image.pixels (i * 4) in
          let m = Bigarray.Array1.get image.pixels ((i * 4) + 1) in
          let y = Bigarray.Array1.get image.pixels ((i * 4) + 2) in
          let k = Bigarray.Array1.get image.pixels ((i * 4) + 3) in
          let r, g, b = Color.cmyk_to_rgb c m y k in
          plane.(i) <- (r * 77 + g * 150 + b * 29) / 256
        done);
    [| plane |]
  end
  else if is_4_component then begin
    let planes = Array.init 4 (fun _ -> Array.make size 0) in
    (match image.pixel_format with
    | CMYK32 ->
        for i = 0 to size - 1 do
          planes.(0).(i) <- Bigarray.Array1.get image.pixels (i * 4);
          planes.(1).(i) <- Bigarray.Array1.get image.pixels ((i * 4) + 1);
          planes.(2).(i) <- Bigarray.Array1.get image.pixels ((i * 4) + 2);
          planes.(3).(i) <- Bigarray.Array1.get image.pixels ((i * 4) + 3)
        done
    | RGB24 ->
        for i = 0 to size - 1 do
          let r = Bigarray.Array1.get image.pixels (i * 3) in
          let g = Bigarray.Array1.get image.pixels ((i * 3) + 1) in
          let b = Bigarray.Array1.get image.pixels ((i * 3) + 2) in
          let c, m, y, k = Color.rgb_to_cmyk r g b in
          planes.(0).(i) <- c;
          planes.(1).(i) <- m;
          planes.(2).(i) <- y;
          planes.(3).(i) <- k
        done);
    planes
  end
  else begin
    let planes = Array.init 3 (fun _ -> Array.make size 0) in
    (match image.pixel_format with
    | RGB24 ->
        for i = 0 to size - 1 do
          planes.(0).(i) <- Bigarray.Array1.get image.pixels (i * 3);
          planes.(1).(i) <- Bigarray.Array1.get image.pixels ((i * 3) + 1);
          planes.(2).(i) <- Bigarray.Array1.get image.pixels ((i * 3) + 2)
        done
    | CMYK32 ->
        for i = 0 to size - 1 do
          let c = Bigarray.Array1.get image.pixels (i * 4) in
          let m = Bigarray.Array1.get image.pixels ((i * 4) + 1) in
          let y = Bigarray.Array1.get image.pixels ((i * 4) + 2) in
          let k = Bigarray.Array1.get image.pixels ((i * 4) + 3) in
          let r, g, b = Color.cmyk_to_rgb c m y k in
          planes.(0).(i) <- r;
          planes.(1).(i) <- g;
          planes.(2).(i) <- b
        done);
    planes
  end

(** Build lossless component descriptors (1:1 sampling, no quantization) *)
let make_lossless_components ~is_grayscale ~is_4_component =
  let n = if is_grayscale then 1 else if is_4_component then 4 else 3 in
  Array.init n (fun i ->
    { Markers.component_id = i + 1; h_sampling = 1; v_sampling = 1;
      quant_table_id = 0 })

(** Shared lossless encode loop.
    Iterates over pixels, computes prediction and difference, and calls
    [on_restart] at restart boundaries and [encode_diff ci diff] per sample. *)
let lossless_encode_loop ~width ~height ~lossless_planes ~predictor
    ~precision_value ~point_transform ~restart_interval ~on_restart ~encode_diff =
  let num_components = Array.length lossless_planes in
  let prev_rows = Array.init num_components (fun _ -> Array.make width 0) in
  let mcu_count = ref 0 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if is_restart_boundary ~restart_interval ~mcu_count:!mcu_count then begin
        on_restart prev_rows;
        let default_val = 1 lsl (precision_value - point_transform - 1) in
        Array.iter (fun row -> Array.fill row 0 width default_val) prev_rows
      end;
      for ci = 0 to num_components - 1 do
        let plane = lossless_planes.(ci) in
        let prev_row = prev_rows.(ci) in
        let sample = plane.((y * width) + x) in
        let ra = if x > 0 then plane.((y * width) + x - 1) else 0 in
        let rb = if y > 0 then prev_row.(x) else 0 in
        let rc = if x > 0 && y > 0 then
          if x = 1 then prev_row.(0) else prev_row.(x - 1)
        else 0 in
        let predicted = Predictor.predict predictor
            ~ra ~rb ~rc ~x ~y ~precision:precision_value ~point_transform in
        let diff = Predictor.compute_diff ~sample ~predicted
            ~precision:precision_value ~point_transform in
        encode_diff ci diff
      done;
      incr mcu_count
    done;
    for ci = 0 to num_components - 1 do
      let plane = lossless_planes.(ci) in
      let prev_row = prev_rows.(ci) in
      for xx = 0 to width - 1 do
        prev_row.(xx) <- plane.((y * width) + xx)
      done
    done
  done

(** Encode JPEG with options *)
let write_bytes_with_options options image =
  let width = image.width in
  let height = image.height in
  let quality = options.quality in

  (* Handle different color modes *)
  let num_components, component_planes, sampling_info =
    match options.color_mode with
    | Grayscale ->
        (* Convert to grayscale: just extract Y *)
        let pixels_array =
          match image.pixel_format with
          | RGB24 ->
              Array.init
                (width * height * 3)
                (fun i -> Bigarray.Array1.get image.pixels i)
          | CMYK32 ->
              (* Convert CMYK to RGB first *)
              let arr = Array.make (width * height * 3) 0 in
              for i = 0 to (width * height) - 1 do
                let c = Bigarray.Array1.get image.pixels (i * 4) in
                let m = Bigarray.Array1.get image.pixels ((i * 4) + 1) in
                let y = Bigarray.Array1.get image.pixels ((i * 4) + 2) in
                let k = Bigarray.Array1.get image.pixels ((i * 4) + 3) in
                let r, g, b = Color.cmyk_to_rgb c m y k in
                arr.(i * 3) <- r;
                arr.((i * 3) + 1) <- g;
                arr.((i * 3) + 2) <- b
              done;
              arr
        in
        let y_plane, _, _ =
          Color.rgb_buffer_to_ycbcr pixels_array width height
        in
        (1, [| y_plane |], (1, 1, 1, 1, width, height, [||], [||]))
    | Color ->
        let pixels_array =
          match image.pixel_format with
          | RGB24 ->
              Array.init
                (width * height * 3)
                (fun i -> Bigarray.Array1.get image.pixels i)
          | CMYK32 ->
              let arr = Array.make (width * height * 3) 0 in
              for i = 0 to (width * height) - 1 do
                let c = Bigarray.Array1.get image.pixels (i * 4) in
                let m = Bigarray.Array1.get image.pixels ((i * 4) + 1) in
                let y = Bigarray.Array1.get image.pixels ((i * 4) + 2) in
                let k = Bigarray.Array1.get image.pixels ((i * 4) + 3) in
                let r, g, b = Color.cmyk_to_rgb c m y k in
                arr.(i * 3) <- r;
                arr.((i * 3) + 1) <- g;
                arr.((i * 3) + 2) <- b
              done;
              arr
        in
        let y_plane, cb_plane, cr_plane =
          Color.rgb_buffer_to_ycbcr pixels_array width height
        in
        let info =
          match options.subsampling with
          | Sub_444 ->
              ( 1,
                1,
                1,
                1,
                width,
                height,
                Color.no_subsample cb_plane width height,
                Color.no_subsample cr_plane width height )
          | Sub_422 ->
              let cw = (width + 1) / 2 in
              ( 2,
                1,
                1,
                1,
                cw,
                height,
                Color.subsample_422 cb_plane width height,
                Color.subsample_422 cr_plane width height )
          | Sub_420 ->
              let cw = (width + 1) / 2 in
              let ch = (height + 1) / 2 in
              ( 2,
                2,
                1,
                1,
                cw,
                ch,
                Color.subsample_420 cb_plane width height,
                Color.subsample_420 cr_plane width height )
        in
        let _, _, _, _, _, _, cb_sub, cr_sub = info in
        (3, [| y_plane; cb_sub; cr_sub |], info)
    | CMYK ->
        (* 4 components, no color conversion *)
        let c_plane = Array.make (width * height) 0 in
        let m_plane = Array.make (width * height) 0 in
        let y_plane = Array.make (width * height) 0 in
        let k_plane = Array.make (width * height) 0 in
        (match image.pixel_format with
        | CMYK32 ->
            for i = 0 to (width * height) - 1 do
              c_plane.(i) <- Bigarray.Array1.get image.pixels (i * 4);
              m_plane.(i) <- Bigarray.Array1.get image.pixels ((i * 4) + 1);
              y_plane.(i) <- Bigarray.Array1.get image.pixels ((i * 4) + 2);
              k_plane.(i) <- Bigarray.Array1.get image.pixels ((i * 4) + 3)
            done
        | RGB24 ->
            let pixels_array =
              Array.init
                (width * height * 3)
                (fun i -> Bigarray.Array1.get image.pixels i)
            in
            for i = 0 to (width * height) - 1 do
              let r = pixels_array.(i * 3) in
              let g = pixels_array.((i * 3) + 1) in
              let b = pixels_array.((i * 3) + 2) in
              let c, m, y, k = Color.rgb_to_cmyk r g b in
              c_plane.(i) <- c;
              m_plane.(i) <- m;
              y_plane.(i) <- y;
              k_plane.(i) <- k
            done);
        ( 4,
          [| c_plane; m_plane; y_plane; k_plane |],
          (1, 1, 1, 1, width, height, [||], [||]) )
    | YCCK ->
        (* 4 components via YCbCr + K *)
        let yy_plane = Array.make (width * height) 0 in
        let cb_plane = Array.make (width * height) 0 in
        let cr_plane = Array.make (width * height) 0 in
        let k_plane = Array.make (width * height) 0 in
        (match image.pixel_format with
        | CMYK32 ->
            for i = 0 to (width * height) - 1 do
              yy_plane.(i) <- Bigarray.Array1.get image.pixels (i * 4);
              cb_plane.(i) <- Bigarray.Array1.get image.pixels ((i * 4) + 1);
              cr_plane.(i) <- Bigarray.Array1.get image.pixels ((i * 4) + 2);
              k_plane.(i) <- Bigarray.Array1.get image.pixels ((i * 4) + 3)
            done
        | RGB24 ->
            let pixels_array =
              Array.init
                (width * height * 3)
                (fun i -> Bigarray.Array1.get image.pixels i)
            in
            for i = 0 to (width * height) - 1 do
              let r = pixels_array.(i * 3) in
              let g = pixels_array.((i * 3) + 1) in
              let b = pixels_array.((i * 3) + 2) in
              let y, cb, cr, k = Color.rgb_to_ycck r g b in
              yy_plane.(i) <- y;
              cb_plane.(i) <- cb;
              cr_plane.(i) <- cr;
              k_plane.(i) <- k
            done);
        let info =
          match options.subsampling with
          | Sub_444 ->
              ( 1,
                1,
                1,
                1,
                width,
                height,
                Color.no_subsample cb_plane width height,
                Color.no_subsample cr_plane width height )
          | Sub_422 ->
              let cw = (width + 1) / 2 in
              ( 2,
                1,
                1,
                1,
                cw,
                height,
                Color.subsample_422 cb_plane width height,
                Color.subsample_422 cr_plane width height )
          | Sub_420 ->
              let cw = (width + 1) / 2 in
              let ch = (height + 1) / 2 in
              ( 2,
                2,
                1,
                1,
                cw,
                ch,
                Color.subsample_420 cb_plane width height,
                Color.subsample_420 cr_plane width height )
        in
        let y_hs, y_vs, cb_hs, cb_vs, cw, ch, cb_sub, cr_sub = info in
        let k_sub =
          match options.subsampling with
          | Sub_444 -> Color.no_subsample k_plane width height
          | Sub_422 -> Color.subsample_422 k_plane width height
          | Sub_420 -> Color.subsample_420 k_plane width height
        in
        ( 4,
          [| yy_plane; cb_sub; cr_sub; k_sub |],
          (y_hs, y_vs, cb_hs, cb_vs, cw, ch, [||], [||]) )
  in

  (* Extract sampling info *)
  let ( y_h_sampling,
        y_v_sampling,
        cb_h_sampling,
        cb_v_sampling,
        chroma_width,
        chroma_height,
        _,
        _ ) =
    sampling_info
  in

  (* Get quantization tables *)
  let lum_quant = Quantization.luminance_table quality in
  let chr_quant = Quantization.chrominance_table quality in

  (* Build Huffman tables *)
  let dc_lum = Huffman.std_dc_luminance_table () in
  let ac_lum = Huffman.std_ac_luminance_table () in
  let dc_chr = Huffman.std_dc_chrominance_table () in
  let ac_chr = Huffman.std_ac_chrominance_table () in

  (* Calculate MCU dimensions *)
  let mcu_width = y_h_sampling * 8 in
  let mcu_height = y_v_sampling * 8 in
  let mcu_h = (width + mcu_width - 1) / mcu_width in
  let mcu_v = (height + mcu_height - 1) / mcu_height in

  (* Determine if grayscale or 4-component *)
  let is_grayscale = options.color_mode = Grayscale in
  let is_4_component = options.color_mode = CMYK || options.color_mode = YCCK in

  (* Build frame header components *)
  let frame_components =
    if is_grayscale then
      [|
        {
          Markers.component_id = 1;
          h_sampling = 1;
          v_sampling = 1;
          quant_table_id = 0;
        };
      |]
    else if is_4_component then
      [|
        {
          Markers.component_id = 1;
          h_sampling = y_h_sampling;
          v_sampling = y_v_sampling;
          quant_table_id = 0;
        };
        {
          Markers.component_id = 2;
          h_sampling = cb_h_sampling;
          v_sampling = cb_v_sampling;
          quant_table_id = 1;
        };
        {
          Markers.component_id = 3;
          h_sampling = cb_h_sampling;
          v_sampling = cb_v_sampling;
          quant_table_id = 1;
        };
        {
          Markers.component_id = 4;
          h_sampling = cb_h_sampling;
          v_sampling = cb_v_sampling;
          quant_table_id = 1;
        };
      |]
    else
      [|
        {
          Markers.component_id = 1;
          h_sampling = y_h_sampling;
          v_sampling = y_v_sampling;
          quant_table_id = 0;
        };
        {
          Markers.component_id = 2;
          h_sampling = cb_h_sampling;
          v_sampling = cb_v_sampling;
          quant_table_id = 1;
        };
        {
          Markers.component_id = 3;
          h_sampling = cb_h_sampling;
          v_sampling = cb_v_sampling;
          quant_table_id = 1;
        };
      |]
  in

  (* Calculate block counts for each component *)
  let y_h_blocks = mcu_h * y_h_sampling in
  let y_v_blocks = mcu_v * y_v_sampling in
  let cb_h_blocks = if is_grayscale then 0 else mcu_h * cb_h_sampling in
  let cb_v_blocks = if is_grayscale then 0 else mcu_v * cb_v_sampling in

  let h_blocks =
    if is_grayscale then [| y_h_blocks |]
    else if is_4_component then
      [| y_h_blocks; cb_h_blocks; cb_h_blocks; cb_h_blocks |]
    else [| y_h_blocks; cb_h_blocks; cb_h_blocks |]
  in
  let v_blocks =
    if is_grayscale then [| y_v_blocks |]
    else if is_4_component then
      [| y_v_blocks; cb_v_blocks; cb_v_blocks; cb_v_blocks |]
    else [| y_v_blocks; cb_v_blocks; cb_v_blocks |]
  in
  let num_blocks = Array.map2 ( * ) h_blocks v_blocks in

  (* Transform and quantize all blocks *)
  let coefficients =
    Array.init num_components (fun ci ->
        let quant_table = if ci = 0 then lum_quant else chr_quant in
        let plane = component_planes.(ci) in
        let pw, ph =
          if ci = 0 then (width, height) else (chroma_width, chroma_height)
        in
        Array.init num_blocks.(ci) (fun block_idx ->
            let bx = block_idx mod h_blocks.(ci) in
            let by = block_idx / h_blocks.(ci) in
            let block = extract_block plane pw ph bx by in
            transform_and_quantize block quant_table))
  in

  (* Get precision value *)
  let precision_value =
    match options.precision with Precision_8 -> 8 | Precision_12 -> 12
  in
  let quant_precision = if precision_value = 8 then 0 else 1 in

  (* Build scan components (used by both Huffman and Arithmetic) *)
  let scan_components =
    if is_grayscale then
      [| { Markers.selector = 1; dc_table = 0; ac_table = 0 } |]
    else if is_4_component then
      [|
        { Markers.selector = 1; dc_table = 0; ac_table = 0 };
        { Markers.selector = 2; dc_table = 1; ac_table = 1 };
        { Markers.selector = 3; dc_table = 1; ac_table = 1 };
        { Markers.selector = 4; dc_table = 1; ac_table = 1 };
      |]
    else
      [|
        { Markers.selector = 1; dc_table = 0; ac_table = 0 };
        { Markers.selector = 2; dc_table = 1; ac_table = 1 };
        { Markers.selector = 3; dc_table = 1; ac_table = 1 };
      |]
  in

  (* Common initial markers *)
  let initial_markers =
    build_initial_markers image
    @ [
        Markers.DQT
          (if is_grayscale then
             [
               {
                 Markers.table_id = 0;
                 precision = quant_precision;
                 values = lum_quant;
               };
             ]
           else
             [
               {
                 Markers.table_id = 0;
                 precision = quant_precision;
                 values = lum_quant;
               };
               {
                 Markers.table_id = 1;
                 precision = quant_precision;
                 values = chr_quant;
               };
             ]);
      ]
  in

  (* DAC markers for arithmetic coding (default conditioning values) *)
  let dac_markers =
    if is_grayscale then
      [
        { Markers.table_class = 0; table_id = 0; conditioning_value = 0 };
        { Markers.table_class = 1; table_id = 0; conditioning_value = 5 };
      ]
    else
      [
        { Markers.table_class = 0; table_id = 0; conditioning_value = 0 };
        { Markers.table_class = 1; table_id = 0; conditioning_value = 5 };
        { Markers.table_class = 0; table_id = 1; conditioning_value = 0 };
        { Markers.table_class = 1; table_id = 1; conditioning_value = 5 };
      ]
  in

  (* DHT markers for Huffman coding *)
  let dht_markers =
    if is_grayscale then
      [
        {
          Markers.table_class = 0;
          table_id = 0;
          counts = Huffman.std_dc_luminance_counts;
          values = Huffman.std_dc_luminance_values;
        };
        {
          Markers.table_class = 1;
          table_id = 0;
          counts = Huffman.std_ac_luminance_counts;
          values = Huffman.std_ac_luminance_values;
        };
      ]
    else
      [
        {
          Markers.table_class = 0;
          table_id = 0;
          counts = Huffman.std_dc_luminance_counts;
          values = Huffman.std_dc_luminance_values;
        };
        {
          Markers.table_class = 1;
          table_id = 0;
          counts = Huffman.std_ac_luminance_counts;
          values = Huffman.std_ac_luminance_values;
        };
        {
          Markers.table_class = 0;
          table_id = 1;
          counts = Huffman.std_dc_chrominance_counts;
          values = Huffman.std_dc_chrominance_values;
        };
        {
          Markers.table_class = 1;
          table_id = 1;
          counts = Huffman.std_ac_chrominance_counts;
          values = Huffman.std_ac_chrominance_values;
        };
      ]
  in

  match (options.encoding_mode, options.entropy_coding) with
  | Baseline, Huffman ->
      (* Baseline Huffman encoding: single scan (SOF0) *)
      let writer = Bitstream.create_writer () in
      let prev_dc = Array.make num_components 0 in
      let mcu_count = ref 0 in
      let rst_counter = ref 0 in
      let restart_interval = options.restart_interval in

      for mcu_y = 0 to mcu_v - 1 do
        for mcu_x = 0 to mcu_h - 1 do
          (* Check for restart marker insertion *)
          if
            is_restart_boundary ~restart_interval ~mcu_count:!mcu_count
          then begin
            Bitstream.write_rst_marker writer !rst_counter;
            rst_counter := (!rst_counter + 1) land 0x07;
            (* Reset DC predictors *)
            Array.fill prev_dc 0 num_components 0
          end;

          for ci = 0 to num_components - 1 do
            let dc_table = if ci = 0 then dc_lum else dc_chr in
            let ac_table = if ci = 0 then ac_lum else ac_chr in
            let h_sampling = if ci = 0 then y_h_sampling else cb_h_sampling in
            let v_sampling = if ci = 0 then y_v_sampling else cb_v_sampling in

            for v = 0 to v_sampling - 1 do
              for h = 0 to h_sampling - 1 do
                let block_idx = block_index ~mcu_x ~mcu_y ~h ~v
                    ~h_sampling ~v_sampling ~h_blocks_total:h_blocks.(ci) in
                if block_idx < num_blocks.(ci) then begin
                  let quantized = coefficients.(ci).(block_idx) in
                  prev_dc.(ci) <-
                    encode_block_from_coeffs writer quantized dc_table ac_table
                      prev_dc.(ci)
                end
              done
            done
          done;
          incr mcu_count
        done
      done;

      Bitstream.flush_writer writer;
      let scan_data = Bitstream.get_bytes writer in

      let markers =
        initial_markers
        @ [
            Markers.SOF0
              {
                frame_type = Markers.Baseline;
                precision = precision_value;
                height;
                width;
                components = frame_components;
              };
          ]
        @ (if options.restart_interval > 0 then
             [ Markers.DRI options.restart_interval ]
           else [])
        @ [ Markers.DHT dht_markers ]
        @ [
            Markers.SOS
              ({ scan_components; ss = 0; se = 63; ah = 0; al = 0 }, scan_data);
            Markers.EOI;
          ]
      in
      Markers.write_markers markers
  | Baseline, Arithmetic ->
      (* Baseline Arithmetic encoding: single scan (SOF9) *)
      let arith_state = Arithmetic.init_arith_scan_encoder num_components in
      let mcu_count = ref 0 in
      let restart_interval = options.restart_interval in

      for mcu_y = 0 to mcu_v - 1 do
        for mcu_x = 0 to mcu_h - 1 do
          (* Check for restart marker - for arithmetic coding, we need to flush
             and re-initialize the encoder at restart boundaries *)
          if
            is_restart_boundary ~restart_interval ~mcu_count:!mcu_count
          then begin
            (* Flush current segment, insert RST marker in output *)
            let segment_data = Arithmetic.finish_arith_encoder arith_state in
            (* We'll handle RST markers differently - combine segments later *)
            ignore segment_data;
            Arithmetic.reset_arith_encoder arith_state
          end;

          for ci = 0 to num_components - 1 do
            let h_sampling = if ci = 0 then y_h_sampling else cb_h_sampling in
            let v_sampling = if ci = 0 then y_v_sampling else cb_v_sampling in

            for v = 0 to v_sampling - 1 do
              for h = 0 to h_sampling - 1 do
                let block_idx = block_index ~mcu_x ~mcu_y ~h ~v
                    ~h_sampling ~v_sampling ~h_blocks_total:h_blocks.(ci) in
                if block_idx < num_blocks.(ci) then begin
                  let quantized = coefficients.(ci).(block_idx) in
                  Arithmetic.encode_arith_block arith_state ci quantized
                end
              done
            done
          done;
          incr mcu_count
        done
      done;

      let scan_data = Arithmetic.finish_arith_encoder arith_state in

      let markers =
        initial_markers
        @ [
            Markers.SOF9
              {
                frame_type = Markers.ArithmeticSequential;
                precision = precision_value;
                height;
                width;
                components = frame_components;
              };
          ]
        @ (if restart_interval > 0 then [ Markers.DRI restart_interval ] else [])
        @ [ Markers.DAC dac_markers ]
        @ [
            Markers.SOS
              ({ scan_components; ss = 0; se = 63; ah = 0; al = 0 }, scan_data);
            Markers.EOI;
          ]
      in
      Markers.write_markers markers
  | Progressive, Huffman ->
      (* Progressive Huffman encoding: multiple scans (SOF2) *)
      let scans =
        if is_grayscale then grayscale_progressive_scans
        else default_progressive_scans
      in
      let dc_tables = [| dc_lum; dc_chr |] in
      let ac_tables = [| ac_lum; ac_chr |] in
      let restart_interval = options.restart_interval in

      (* Build initial markers *)
      let prog_initial_markers =
        initial_markers
        @ [
            Markers.SOF2
              {
                frame_type = Markers.Progressive;
                precision = precision_value;
                height;
                width;
                components = frame_components;
              };
          ]
        @ (if restart_interval > 0 then [ Markers.DRI restart_interval ] else [])
        @ [ Markers.DHT dht_markers ]
      in

      (* Encode each scan *)
      let scan_markers =
        List.map
          (fun scan_spec ->
            let is_dc_scan = scan_spec.ss = 0 && scan_spec.se = 0 in
            let scan_data =
              if is_dc_scan then
                encode_progressive_dc_scan coefficients h_blocks num_blocks
                  dc_tables scan_spec mcu_h mcu_v y_h_sampling y_v_sampling
                  cb_h_sampling cb_v_sampling ~restart_interval
              else
                encode_progressive_ac_scan coefficients h_blocks num_blocks
                  ac_tables scan_spec mcu_h mcu_v y_h_sampling y_v_sampling
                  cb_h_sampling cb_v_sampling ~restart_interval
            in
            let prog_scan_components =
              Array.of_list
                (List.map
                   (fun ci ->
                     let dc_tbl = if ci = 0 then 0 else 1 in
                     let ac_tbl = if ci = 0 then 0 else 1 in
                     {
                       Markers.selector = ci + 1;
                       dc_table = dc_tbl;
                       ac_table = ac_tbl;
                     })
                   scan_spec.components)
            in
            Markers.SOS
              ( {
                  Markers.scan_components = prog_scan_components;
                  ss = scan_spec.ss;
                  se = scan_spec.se;
                  ah = scan_spec.ah;
                  al = scan_spec.al;
                },
                scan_data ))
          scans
      in

      let all_markers = prog_initial_markers @ scan_markers @ [ Markers.EOI ] in
      Markers.write_markers all_markers
  | Progressive, Arithmetic ->
      (* Progressive Arithmetic encoding: multiple scans (SOF10) *)
      let scans =
        if is_grayscale then grayscale_progressive_scans
        else default_progressive_scans
      in
      let restart_interval = options.restart_interval in

      (* Build initial markers *)
      let prog_arith_initial_markers =
        initial_markers
        @ [
            Markers.SOF10
              {
                frame_type = Markers.ArithmeticProgressive;
                precision = precision_value;
                height;
                width;
                components = frame_components;
              };
          ]
        @ (if restart_interval > 0 then [ Markers.DRI restart_interval ] else [])
        @ [ Markers.DAC dac_markers ]
      in

      (* Encode each scan with arithmetic coding *)
      let scan_markers =
        List.map
          (fun scan_spec ->
            let is_dc_scan = scan_spec.ss = 0 && scan_spec.se = 0 in
            let num_scan_components = List.length scan_spec.components in
            let arith_state =
              Arithmetic.init_arith_scan_encoder num_scan_components
            in
            let al = scan_spec.al in
            let mcu_count = ref 0 in

            if is_dc_scan then begin
              (* DC scan - encode DC coefficients only *)
              for mcu_y = 0 to mcu_v - 1 do
                for mcu_x = 0 to mcu_h - 1 do
                  if
                    is_restart_boundary ~restart_interval ~mcu_count:!mcu_count
                  then Arithmetic.reset_arith_encoder arith_state;

                  List.iteri
                    (fun sci ci ->
                      let h_sampling =
                        if ci = 0 then y_h_sampling else cb_h_sampling
                      in
                      let v_sampling =
                        if ci = 0 then y_v_sampling else cb_v_sampling
                      in

                      for v = 0 to v_sampling - 1 do
                        for h = 0 to h_sampling - 1 do
                          let block_idx = block_index ~mcu_x ~mcu_y ~h ~v
                              ~h_sampling ~v_sampling ~h_blocks_total:h_blocks.(ci) in
                          if block_idx < num_blocks.(ci) then begin
                            let coeffs = coefficients.(ci).(block_idx) in
                            let dc_value = coeffs.(0) asr al in
                            Arithmetic.encode_arith_dc_only arith_state sci
                              dc_value
                          end
                        done
                      done)
                    scan_spec.components;
                  incr mcu_count
                done
              done
            end
            else begin
              (* AC scan - encode AC coefficients in spectral range *)
              let ss = scan_spec.ss in
              let se = scan_spec.se in

              for mcu_y = 0 to mcu_v - 1 do
                for mcu_x = 0 to mcu_h - 1 do
                  if
                    is_restart_boundary ~restart_interval ~mcu_count:!mcu_count
                  then Arithmetic.reset_arith_encoder arith_state;

                  List.iteri
                    (fun sci ci ->
                      let h_sampling =
                        if ci = 0 then y_h_sampling else cb_h_sampling
                      in
                      let v_sampling =
                        if ci = 0 then y_v_sampling else cb_v_sampling
                      in

                      for v = 0 to v_sampling - 1 do
                        for h = 0 to h_sampling - 1 do
                          let block_idx = block_index ~mcu_x ~mcu_y ~h ~v
                              ~h_sampling ~v_sampling ~h_blocks_total:h_blocks.(ci) in
                          if block_idx < num_blocks.(ci) then begin
                            let orig_coeffs = coefficients.(ci).(block_idx) in
                            (* Create coefficients with only the AC range we care about *)
                            let ac_coeffs = Array.make 64 0 in
                            for k = ss to se do
                              ac_coeffs.(k) <- orig_coeffs.(k) asr al
                            done;
                            (* For AC-only scan, encode only AC coefficients in range *)
                            Arithmetic.encode_arith_ac_only arith_state sci
                              ac_coeffs ~ss ~se
                          end
                        done
                      done)
                    scan_spec.components;
                  incr mcu_count
                done
              done
            end;

            let scan_data = Arithmetic.finish_arith_encoder arith_state in
            let prog_scan_components =
              Array.of_list
                (List.map
                   (fun ci ->
                     let dc_tbl = if ci = 0 then 0 else 1 in
                     let ac_tbl = if ci = 0 then 0 else 1 in
                     {
                       Markers.selector = ci + 1;
                       dc_table = dc_tbl;
                       ac_table = ac_tbl;
                     })
                   scan_spec.components)
            in
            Markers.SOS
              ( {
                  Markers.scan_components = prog_scan_components;
                  ss = scan_spec.ss;
                  se = scan_spec.se;
                  ah = scan_spec.ah;
                  al = scan_spec.al;
                },
                scan_data ))
          scans
      in

      let all_markers =
        prog_arith_initial_markers @ scan_markers @ [ Markers.EOI ]
      in
      Markers.write_markers all_markers
  | Lossless, Huffman ->
      (* Lossless Huffman encoding (SOF3) *)
      let predictor_sel = if options.predictor = 0 then 1 else options.predictor in
      let point_transform = options.point_transform in
      let predictor = Predictor.predictor_of_int predictor_sel in

      let lossless_components = make_lossless_components ~is_grayscale ~is_4_component in

      let lossless_scan_components =
        Array.mapi (fun i comp ->
            let table_id = if i = 0 then 0 else 1 in
            { Markers.selector = comp.Markers.component_id; dc_table = table_id; ac_table = 0 })
          lossless_components
      in

      let lossless_planes = extract_lossless_planes image ~is_grayscale ~is_4_component in

      (* Encode scan data *)
      let writer = Bitstream.create_writer () in
      let dc_lum_enc = Huffman.std_dc_luminance_table () in
      let dc_chr_enc = Huffman.std_dc_chrominance_table () in
      let restart_interval = options.restart_interval in
      let rst_counter = ref 0 in

      lossless_encode_loop ~width ~height ~lossless_planes ~predictor
        ~precision_value ~point_transform ~restart_interval
        ~on_restart:(fun _prev_rows ->
          Bitstream.write_rst_marker writer !rst_counter;
          rst_counter := (!rst_counter + 1) land 0x07)
        ~encode_diff:(fun ci diff ->
          let dc_table = if ci = 0 then dc_lum_enc else dc_chr_enc in
          let category = Huffman.category diff in
          Huffman.encode_symbol writer dc_table category;
          if category > 0 then
            Bitstream.write_bits writer (Huffman.encode_value diff category) category);

      Bitstream.flush_writer writer;
      let scan_data = Bitstream.get_bytes writer in

      (* Build markers - no quantization tables needed for lossless *)
      let lossless_initial_markers = build_initial_markers image in

      let markers =
        lossless_initial_markers
        @ [
            Markers.SOF3
              {
                frame_type = Markers.LosslessHuffman;
                precision = precision_value;
                height;
                width;
                components = lossless_components;
              };
          ]
        @ (if restart_interval > 0 then [ Markers.DRI restart_interval ] else [])
        @ [ Markers.DHT dht_markers ]
        @ [
            Markers.SOS
              ({ scan_components = lossless_scan_components;
                 ss = predictor_sel;  (* Predictor selection in ss field *)
                 se = 0;              (* se = 0 for lossless *)
                 ah = 0;              (* ah = 0 for lossless *)
                 al = point_transform (* Point transform in al field *)
               }, scan_data);
            Markers.EOI;
          ]
      in
      Markers.write_markers markers
  | Lossless, Arithmetic ->
      (* Lossless Arithmetic encoding (SOF11) - not yet fully implemented *)
      (* The arithmetic coding for lossless JPEG requires a different statistical
         model than DCT JPEG. This is a placeholder implementation. *)
      let predictor_sel = if options.predictor = 0 then 1 else options.predictor in
      let point_transform = options.point_transform in
      let predictor = Predictor.predictor_of_int predictor_sel in

      let lossless_components = make_lossless_components ~is_grayscale ~is_4_component in

      let lossless_scan_components =
        Array.mapi (fun i comp ->
            let table_id = if i = 0 then 0 else 1 in
            { Markers.selector = comp.Markers.component_id; dc_table = table_id; ac_table = 0 })
          lossless_components
      in

      let lossless_planes = extract_lossless_planes image ~is_grayscale ~is_4_component in

      (* Encode using arithmetic coding *)
      let arith_state = Arithmetic.init_arith_scan_encoder (Array.length lossless_planes) in
      let restart_interval = options.restart_interval in

      lossless_encode_loop ~width ~height ~lossless_planes ~predictor
        ~precision_value ~point_transform ~restart_interval
        ~on_restart:(fun _prev_rows ->
          ignore (Arithmetic.finish_arith_encoder arith_state);
          Arithmetic.reset_arith_encoder arith_state)
        ~encode_diff:(fun ci diff ->
          Arithmetic.encode_lossless_diff arith_state ci diff);

      let scan_data = Arithmetic.finish_arith_encoder arith_state in

      let lossless_initial_markers = build_initial_markers image in

      let markers =
        lossless_initial_markers
        @ [
            Markers.SOF11
              {
                frame_type = Markers.LosslessArithmetic;
                precision = precision_value;
                height;
                width;
                components = lossless_components;
              };
          ]
        @ (if restart_interval > 0 then [ Markers.DRI restart_interval ] else [])
        @ [ Markers.DAC dac_markers ]
        @ [
            Markers.SOS
              ({ scan_components = lossless_scan_components;
                 ss = predictor_sel;
                 se = 0;
                 ah = 0;
                 al = point_transform
               }, scan_data);
            Markers.EOI;
          ]
      in
      Markers.write_markers markers

(** Encode JPEG with options to file *)
let write_with_options options filename image =
  let data = write_bytes_with_options options image in
  let oc = open_out_bin filename in
  output_bytes oc data;
  close_out oc

(** Encode JPEG to bytes *)
let write_bytes ?(quality = 75) image =
  let width = image.width in
  let height = image.height in

  (* Convert RGB to YCbCr *)
  let pixels_array =
    Array.init
      (width * height * 3)
      (fun i -> Bigarray.Array1.get image.pixels i)
  in

  let y_plane, cb_plane, cr_plane =
    Color.rgb_buffer_to_ycbcr pixels_array width height
  in

  (* Subsample chroma (4:2:0) *)
  let cb_sub = Color.subsample_420 cb_plane width height in
  let cr_sub = Color.subsample_420 cr_plane width height in
  let chroma_width = (width + 1) / 2 in
  let chroma_height = (height + 1) / 2 in

  (* Get quantization tables *)
  let lum_quant = Quantization.luminance_table quality in
  let chr_quant = Quantization.chrominance_table quality in

  (* Build Huffman tables *)
  let dc_lum = Huffman.std_dc_luminance_table () in
  let ac_lum = Huffman.std_ac_luminance_table () in
  let dc_chr = Huffman.std_dc_chrominance_table () in
  let ac_chr = Huffman.std_ac_chrominance_table () in

  (* MCU layout for 4:2:0: 2x2 Y blocks + 1 Cb + 1 Cr *)
  let mcu_h = (width + 15) / 16 in
  let mcu_v = (height + 15) / 16 in

  (* Encode scan data *)
  let writer = Bitstream.create_writer () in

  let prev_dc_y = ref 0 in
  let prev_dc_cb = ref 0 in
  let prev_dc_cr = ref 0 in

  for mcu_y = 0 to mcu_v - 1 do
    for mcu_x = 0 to mcu_h - 1 do
      (* Encode 4 Y blocks (2x2) *)
      for v = 0 to 1 do
        for h = 0 to 1 do
          let bx = (mcu_x * 2) + h in
          let by = (mcu_y * 2) + v in
          let block = extract_block y_plane width height bx by in
          prev_dc_y :=
            encode_block writer block dc_lum ac_lum lum_quant !prev_dc_y
        done
      done;

      (* Encode Cb block *)
      let cb_block =
        extract_block cb_sub chroma_width chroma_height mcu_x mcu_y
      in
      prev_dc_cb :=
        encode_block writer cb_block dc_chr ac_chr chr_quant !prev_dc_cb;

      (* Encode Cr block *)
      let cr_block =
        extract_block cr_sub chroma_width chroma_height mcu_x mcu_y
      in
      prev_dc_cr :=
        encode_block writer cr_block dc_chr ac_chr chr_quant !prev_dc_cr
    done
  done;

  Bitstream.flush_writer writer;
  let scan_data = Bitstream.get_bytes writer in

  (* Build marker list *)
  let markers =
    build_initial_markers image
    @ [
        (* Quantization tables *)
        Markers.DQT
          [
            { table_id = 0; precision = 0; values = lum_quant };
            { table_id = 1; precision = 0; values = chr_quant };
          ];
        (* Frame header *)
        Markers.SOF0
          {
            frame_type = Markers.Baseline;
            precision = 8;
            height;
            width;
            components =
              [|
                {
                  component_id = 1;
                  h_sampling = 2;
                  v_sampling = 2;
                  quant_table_id = 0;
                };
                {
                  component_id = 2;
                  h_sampling = 1;
                  v_sampling = 1;
                  quant_table_id = 1;
                };
                {
                  component_id = 3;
                  h_sampling = 1;
                  v_sampling = 1;
                  quant_table_id = 1;
                };
              |];
          };
        (* Huffman tables *)
        Markers.DHT
          [
            {
              table_class = 0;
              table_id = 0;
              counts = Huffman.std_dc_luminance_counts;
              values = Huffman.std_dc_luminance_values;
            };
            {
              table_class = 1;
              table_id = 0;
              counts = Huffman.std_ac_luminance_counts;
              values = Huffman.std_ac_luminance_values;
            };
            {
              table_class = 0;
              table_id = 1;
              counts = Huffman.std_dc_chrominance_counts;
              values = Huffman.std_dc_chrominance_values;
            };
            {
              table_class = 1;
              table_id = 1;
              counts = Huffman.std_ac_chrominance_counts;
              values = Huffman.std_ac_chrominance_values;
            };
          ];
        (* Scan *)
        Markers.SOS
          ( {
              scan_components =
                [|
                  { selector = 1; dc_table = 0; ac_table = 0 };
                  { selector = 2; dc_table = 1; ac_table = 1 };
                  { selector = 3; dc_table = 1; ac_table = 1 };
                |];
              ss = 0;
              se = 63;
              ah = 0;
              al = 0;
            },
            scan_data );
        Markers.EOI;
      ]
  in

  Markers.write_markers markers

(** Write JPEG to file *)
let write ?(quality = 75) filename image =
  let data = write_bytes ~quality image in
  let oc = open_out_bin filename in
  output_bytes oc data;
  close_out oc

(** Create an image from raw RGB data *)
let create_image width height pixels =
  { width; height; pixels; pixel_format = RGB24; exif = None; icc_profile = None }

(** Create an image with EXIF *)
let create_image_with_exif width height pixels exif =
  { width; height; pixels; pixel_format = RGB24; exif = Some exif; icc_profile = None }

(** Create a CMYK image *)
let create_cmyk_image width height pixels =
  { width; height; pixels; pixel_format = CMYK32; exif = None; icc_profile = None }

(** Create an image with ICC profile *)
let create_image_with_icc width height pixels icc =
  { width; height; pixels; pixel_format = RGB24; exif = None; icc_profile = Some icc }

(** Create an image with both EXIF and ICC profile *)
let create_image_with_metadata width height pixels exif icc =
  { width; height; pixels; pixel_format = RGB24; exif = Some exif; icc_profile = Some icc }

(** Get pixel at (x, y) as (r, g, b) for RGB24 images *)
let get_pixel image x y =
  match image.pixel_format with
  | RGB24 ->
      let idx = ((y * image.width) + x) * 3 in
      let r = Bigarray.Array1.get image.pixels idx in
      let g = Bigarray.Array1.get image.pixels (idx + 1) in
      let b = Bigarray.Array1.get image.pixels (idx + 2) in
      (r, g, b)
  | CMYK32 ->
      (* Convert CMYK to RGB on the fly *)
      let idx = ((y * image.width) + x) * 4 in
      let c = Bigarray.Array1.get image.pixels idx in
      let m = Bigarray.Array1.get image.pixels (idx + 1) in
      let y_val = Bigarray.Array1.get image.pixels (idx + 2) in
      let k = Bigarray.Array1.get image.pixels (idx + 3) in
      Color.cmyk_to_rgb c m y_val k

(** Get CMYK pixel at (x, y) as (c, m, y, k) *)
let get_cmyk_pixel image x y =
  match image.pixel_format with
  | CMYK32 ->
      let idx = ((y * image.width) + x) * 4 in
      let c = Bigarray.Array1.get image.pixels idx in
      let m = Bigarray.Array1.get image.pixels (idx + 1) in
      let y_val = Bigarray.Array1.get image.pixels (idx + 2) in
      let k = Bigarray.Array1.get image.pixels (idx + 3) in
      (c, m, y_val, k)
  | RGB24 -> failwith "get_cmyk_pixel: image is RGB24, not CMYK32"

(** Set pixel at (x, y) *)
let set_pixel image x y r g b =
  match image.pixel_format with
  | RGB24 ->
      let idx = ((y * image.width) + x) * 3 in
      Bigarray.Array1.set image.pixels idx r;
      Bigarray.Array1.set image.pixels (idx + 1) g;
      Bigarray.Array1.set image.pixels (idx + 2) b
  | CMYK32 -> failwith "set_pixel: image is CMYK32, use set_cmyk_pixel"

(** Set CMYK pixel at (x, y) *)
let set_cmyk_pixel image x y c m y_val k =
  match image.pixel_format with
  | CMYK32 ->
      let idx = ((y * image.width) + x) * 4 in
      Bigarray.Array1.set image.pixels idx c;
      Bigarray.Array1.set image.pixels (idx + 1) m;
      Bigarray.Array1.set image.pixels (idx + 2) y_val;
      Bigarray.Array1.set image.pixels (idx + 3) k
  | RGB24 -> failwith "set_cmyk_pixel: image is RGB24, not CMYK32"
