(** Pure OCaml JPEG library *)

(* Re-export internal modules for advanced use *)
module Bitstream = Bitstream
module Markers = Markers
module Huffman = Huffman
module Dct = Dct
module Quantization = Quantization
module Color = Color
module Exif = Exif

type pixel_data =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** Pixel data stored as RGB24 format *)

type image = {
  width : int;
  height : int;
  pixels : pixel_data; (* RGB24 format: R,G,B,R,G,B,... *)
  exif : Exif.t option;
}
(** JPEG image *)

type decode_state = {
  mutable frame : Markers.frame_header option;
  mutable quant_tables : Markers.quant_table array;
  mutable dc_tables : Huffman.table array;
  mutable ac_tables : Huffman.table array;
  mutable restart_interval : int;
  mutable exif : Exif.t option;
}
(** Decoding state *)

(** Create initial decode state *)
let create_decode_state () =
  {
    frame = None;
    quant_tables =
      Array.make 4
        { Markers.table_id = 0; precision = 0; values = Array.make 64 16 };
    dc_tables = Array.make 4 (Huffman.std_dc_luminance_table ());
    ac_tables = Array.make 4 (Huffman.std_ac_luminance_table ());
    restart_interval = 0;
    exif = None;
  }

(** Process marker segments and build decode state *)
let process_markers markers =
  let state = create_decode_state () in

  List.iter
    (fun segment ->
      match segment with
      | Markers.SOF0 frame -> state.frame <- Some frame
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
      | Markers.DRI interval -> state.restart_interval <- interval
      | Markers.APP1 data -> state.exif <- Some (Exif.parse data)
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
        state.restart_interval > 0 && !mcu_count > 0
        && !mcu_count mod state.restart_interval = 0
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
            let bx = (mcu_x * comp.Markers.h_sampling) + h in
            let by = (mcu_y * comp.Markers.v_sampling) + v in
            let block_idx = (by * h_blocks_total) + bx in

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

  let max_h =
    Array.fold_left
      (fun m c -> max m c.Markers.h_sampling)
      1 frame.Markers.components
  in
  let max_v =
    Array.fold_left
      (fun m c -> max m c.Markers.v_sampling)
      1 frame.Markers.components
  in

  let mcu_width = max_h * 8 in
  let mcu_height = max_v * 8 in
  let mcus_x = (width + mcu_width - 1) / mcu_width in
  let mcus_y = (height + mcu_height - 1) / mcu_height in

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
    pixels
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
    pixels
  end

(** Decode JPEG from bytes *)
let read_bytes data =
  let markers = Markers.parse_markers data in
  let state = process_markers markers in

  match state.frame with
  | None -> failwith "No SOF0 marker found"
  | Some frame -> (
      if frame.Markers.precision <> 8 then
        failwith "Only 8-bit precision is supported";

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
          let pixels = reconstruct_image frame component_blocks in

          {
            width = frame.Markers.width;
            height = frame.Markers.height;
            pixels;
            exif = state.exif;
          })

(** Read JPEG from file *)
let read filename =
  let ic = open_in_bin filename in
  let len = in_channel_length ic in
  let data = Bytes.create len in
  really_input ic data 0 len;
  close_in ic;
  read_bytes data

(* === ENCODING === *)

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
    [
      Markers.SOI;
      Markers.APP0
        {
          version_major = 1;
          version_minor = 1;
          density_units = 0;
          x_density = 1;
          y_density = 1;
          thumbnail_width = 0;
          thumbnail_height = 0;
        };
      (* Optional EXIF *)
    ]
    @ (match image.exif with
      | Some exif -> [ Markers.APP1 (Exif.to_bytes exif) ]
      | None -> [])
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
let create_image width height pixels = { width; height; pixels; exif = None }

(** Create an image with EXIF *)
let create_image_with_exif width height pixels exif =
  { width; height; pixels; exif = Some exif }

(** Get pixel at (x, y) as (r, g, b) *)
let get_pixel image x y =
  let idx = ((y * image.width) + x) * 3 in
  let r = Bigarray.Array1.get image.pixels idx in
  let g = Bigarray.Array1.get image.pixels (idx + 1) in
  let b = Bigarray.Array1.get image.pixels (idx + 2) in
  (r, g, b)

(** Set pixel at (x, y) *)
let set_pixel image x y r g b =
  let idx = ((y * image.width) + x) * 3 in
  Bigarray.Array1.set image.pixels idx r;
  Bigarray.Array1.set image.pixels (idx + 1) g;
  Bigarray.Array1.set image.pixels (idx + 2) b
