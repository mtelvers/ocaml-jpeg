(** JPEG marker definitions and parsing *)

(** JPEG markers *)
let marker_soi = 0xD8 (* Start of Image *)

let marker_eoi = 0xD9 (* End of Image *)
let marker_sof0 = 0xC0 (* Start of Frame (Baseline DCT) *)
let marker_sof2 = 0xC2 (* Start of Frame (Progressive DCT) *)
let marker_sof9 = 0xC9 (* Start of Frame (Extended sequential, arithmetic) *)
let marker_sof10 = 0xCA (* Start of Frame (Progressive, arithmetic) *)
let marker_dht = 0xC4 (* Define Huffman Table *)
let marker_dac = 0xCC (* Define Arithmetic Conditioning *)
let marker_dqt = 0xDB (* Define Quantization Table *)
let marker_dri = 0xDD (* Define Restart Interval *)
let marker_sos = 0xDA (* Start of Scan *)
let marker_app0 = 0xE0 (* JFIF marker *)
let marker_app1 = 0xE1 (* EXIF marker *)
let marker_com = 0xFE (* Comment *)

type component_info = {
  component_id : int;
  h_sampling : int;
  v_sampling : int;
  quant_table_id : int;
}
(** Component info in SOF *)

type frame_type =
  | Baseline  (** SOF0: Baseline DCT, Huffman coding *)
  | Progressive  (** SOF2: Progressive DCT, Huffman coding *)
  | ArithmeticSequential  (** SOF9: Extended sequential, arithmetic coding *)
  | ArithmeticProgressive  (** SOF10: Progressive, arithmetic coding *)

type arithmetic_conditioning = {
  table_class : int; (* 0 = DC, 1 = AC *)
  table_id : int;
  conditioning_value : int;
      (* Cs for DC tables (0-255), Kx for AC tables (1-63) *)
}
(** Arithmetic conditioning table entry *)

type frame_header = {
  frame_type : frame_type;
  precision : int;
  height : int;
  width : int;
  components : component_info array;
}
(** Frame header (SOF) data *)

type scan_component = { selector : int; dc_table : int; ac_table : int }
(** Scan component selector *)

type scan_header = {
  scan_components : scan_component array;
  ss : int; (* Start of spectral selection *)
  se : int; (* End of spectral selection *)
  ah : int; (* Successive approximation high *)
  al : int; (* Successive approximation low *)
}
(** Scan header (SOS) data *)

type huffman_table = {
  table_class : int; (* 0 = DC, 1 = AC *)
  table_id : int;
  counts : int array; (* number of codes of each length 1-16 *)
  values : int array; (* symbol values *)
}
(** Huffman table *)

type quant_table = {
  table_id : int;
  precision : int; (* 0 = 8-bit, 1 = 16-bit *)
  values : int array; (* 64 values in zig-zag order *)
}
(** Quantization table *)

type jfif_segment = {
  version_major : int;
  version_minor : int;
  density_units : int;
  x_density : int;
  y_density : int;
  thumbnail_width : int;
  thumbnail_height : int;
}
(** APP0 JFIF segment *)

(** Marker segment types *)
type marker_segment =
  | SOI
  | EOI
  | SOF0 of frame_header
  | SOF2 of frame_header
  | SOF9 of frame_header  (** Extended sequential, arithmetic coding *)
  | SOF10 of frame_header  (** Progressive, arithmetic coding *)
  | DHT of huffman_table list
  | DAC of arithmetic_conditioning list  (** Arithmetic conditioning tables *)
  | DQT of quant_table list
  | DRI of int
  | SOS of scan_header * bytes (* header + entropy-coded data *)
  | APP0 of jfif_segment
  | APP1 of bytes (* EXIF data - parsed separately *)
  | COM of string
  | Unknown of int * bytes

(** Parse a 16-bit big-endian value *)
let read_u16 data pos =
  let b1 = Bytes.get_uint8 data pos in
  let b2 = Bytes.get_uint8 data (pos + 1) in
  (b1 lsl 8) lor b2

(** Parse SOF (Start of Frame) - works for both SOF0 and SOF2 *)
let parse_sof data pos content_len frame_type =
  let precision = Bytes.get_uint8 data pos in
  let height = read_u16 data (pos + 1) in
  let width = read_u16 data (pos + 3) in
  let num_components = Bytes.get_uint8 data (pos + 5) in

  (* content_len = 6 + num_components * 3 (precision + height + width + num_comp + components) *)
  if content_len <> 6 + (num_components * 3) then failwith "Invalid SOF length";

  let components =
    Array.init num_components (fun i ->
        let offset = pos + 6 + (i * 3) in
        let component_id = Bytes.get_uint8 data offset in
        let sampling = Bytes.get_uint8 data (offset + 1) in
        let h_sampling = sampling lsr 4 in
        let v_sampling = sampling land 0x0F in
        let quant_table_id = Bytes.get_uint8 data (offset + 2) in
        { component_id; h_sampling; v_sampling; quant_table_id })
  in

  { frame_type; precision; height; width; components }

(** Parse DHT (Define Huffman Table) *)
let parse_dht data pos len =
  let rec parse_tables offset tables =
    if offset >= pos + len - 2 then List.rev tables
    else begin
      let info = Bytes.get_uint8 data offset in
      let table_class = info lsr 4 in
      let table_id = info land 0x0F in

      let counts =
        Array.init 16 (fun i -> Bytes.get_uint8 data (offset + 1 + i))
      in

      let total_symbols = Array.fold_left ( + ) 0 counts in
      let values =
        Array.init total_symbols (fun i ->
            Bytes.get_uint8 data (offset + 17 + i))
      in

      let table = { table_class; table_id; counts; values } in
      parse_tables (offset + 17 + total_symbols) (table :: tables)
    end
  in
  parse_tables pos []

(** Parse DAC (Define Arithmetic Conditioning) *)
let parse_dac data pos len =
  let rec parse_tables offset tables =
    if offset >= pos + len - 2 then List.rev tables
    else begin
      let info = Bytes.get_uint8 data offset in
      let table_class = info lsr 4 in
      let table_id = info land 0x0F in
      let conditioning_value = Bytes.get_uint8 data (offset + 1) in
      let table = { table_class; table_id; conditioning_value } in
      parse_tables (offset + 2) (table :: tables)
    end
  in
  parse_tables pos []

(** Parse DQT (Define Quantization Table) *)
let parse_dqt data pos len =
  let rec parse_tables offset tables =
    if offset >= pos + len - 2 then List.rev tables
    else begin
      let info = Bytes.get_uint8 data offset in
      let precision = info lsr 4 in
      let table_id = info land 0x0F in

      let elem_size = if precision = 0 then 1 else 2 in
      let values =
        Array.init 64 (fun i ->
            if precision = 0 then Bytes.get_uint8 data (offset + 1 + i)
            else read_u16 data (offset + 1 + (i * 2)))
      in

      let table = { table_id; precision; values } in
      parse_tables (offset + 1 + (64 * elem_size)) (table :: tables)
    end
  in
  parse_tables pos []

(** Parse SOS (Start of Scan) header *)
let parse_sos_header data pos =
  let num_components = Bytes.get_uint8 data pos in

  let scan_components =
    Array.init num_components (fun i ->
        let offset = pos + 1 + (i * 2) in
        let selector = Bytes.get_uint8 data offset in
        let tables = Bytes.get_uint8 data (offset + 1) in
        let dc_table = tables lsr 4 in
        let ac_table = tables land 0x0F in
        { selector; dc_table; ac_table })
  in

  let spec_offset = pos + 1 + (num_components * 2) in
  let ss = Bytes.get_uint8 data spec_offset in
  let se = Bytes.get_uint8 data (spec_offset + 1) in
  let approx = Bytes.get_uint8 data (spec_offset + 2) in
  let ah = approx lsr 4 in
  let al = approx land 0x0F in

  { scan_components; ss; se; ah; al }

(** Check for JFIF signature at position *)
let is_jfif_signature data pos len =
  len >= 14
  && Bytes.sub_string data pos 4 = "JFIF"
  && Bytes.get_uint8 data (pos + 4) = 0x00

(** Parse APP0 JFIF segment *)
let parse_app0 data pos len =
  if is_jfif_signature data pos len then begin
    let version_major = Bytes.get_uint8 data (pos + 5) in
    let version_minor = Bytes.get_uint8 data (pos + 6) in
    let density_units = Bytes.get_uint8 data (pos + 7) in
    let x_density = read_u16 data (pos + 8) in
    let y_density = read_u16 data (pos + 10) in
    let thumbnail_width = Bytes.get_uint8 data (pos + 12) in
    let thumbnail_height = Bytes.get_uint8 data (pos + 13) in
    Some
      {
        version_major;
        version_minor;
        density_units;
        x_density;
        y_density;
        thumbnail_width;
        thumbnail_height;
      }
  end
  else None

(** Find the end of entropy-coded data (scan for next marker) *)
let find_entropy_end data start_pos =
  let len = Bytes.length data in
  let rec scan pos =
    if pos >= len - 1 then len
    else if Bytes.get_uint8 data pos = 0xFF then
      (* FF00 is byte stuffing, FFD0-FFD7 are restart markers *)
      match Bytes.get_uint8 data (pos + 1) with
      | 0x00 -> scan (pos + 2)
      | c when c >= 0xD0 && c <= 0xD7 -> scan (pos + 2)
      | 0xFF -> scan (pos + 1)
      | _ -> pos (* Found a real marker *)
    else scan (pos + 1)
  in
  scan start_pos

(** Parse all markers in a JPEG file *)
let parse_markers data =
  let len = Bytes.length data in
  let markers = ref [] in
  let pos = ref 0 in

  (* Helper to read marker *)
  let read_marker () =
    if !pos >= len then None
    else if Bytes.get_uint8 data !pos <> 0xFF then
      failwith (Printf.sprintf "Expected marker at position %d" !pos)
    else begin
      (* Skip any padding FF bytes *)
      while !pos < len && Bytes.get_uint8 data !pos = 0xFF do
        incr pos
      done;
      if !pos >= len then None
      else begin
        let marker = Bytes.get_uint8 data !pos in
        incr pos;
        Some marker
      end
    end
  in

  (* Initial FF *)
  if Bytes.get_uint8 data 0 <> 0xFF then failwith "Not a JPEG file (missing FF)";

  while !pos < len do
    match read_marker () with
    | None -> pos := len
    | Some marker ->
        if marker = marker_soi then markers := SOI :: !markers
        else if marker = marker_eoi then begin
          markers := EOI :: !markers;
          pos := len (* Stop parsing *)
        end
        else if marker >= 0xD0 && marker <= 0xD7 then
          (* Restart marker - skip *)
          ()
        else begin
          (* Read segment length *)
          let seg_len = read_u16 data !pos in
          let content_start = !pos + 2 in
          let content_len = seg_len - 2 in

          let segment =
            if marker = marker_sof0 then
              SOF0 (parse_sof data content_start content_len Baseline)
            else if marker = marker_sof2 then
              SOF2 (parse_sof data content_start content_len Progressive)
            else if marker = marker_sof9 then
              SOF9
                (parse_sof data content_start content_len ArithmeticSequential)
            else if marker = marker_sof10 then
              SOF10
                (parse_sof data content_start content_len ArithmeticProgressive)
            else if marker = marker_dht then
              DHT (parse_dht data content_start content_len)
            else if marker = marker_dac then
              DAC (parse_dac data content_start content_len)
            else if marker = marker_dqt then
              DQT (parse_dqt data content_start content_len)
            else if marker = marker_dri then DRI (read_u16 data content_start)
            else if marker = marker_sos then begin
              let header = parse_sos_header data content_start in
              let data_start =
                content_start + 1
                + (Array.length header.scan_components * 2)
                + 3
              in
              let data_end = find_entropy_end data data_start in
              let entropy_data =
                Bytes.sub data data_start (data_end - data_start)
              in
              pos := data_end - seg_len;
              (* Adjust position *)
              SOS (header, entropy_data)
            end
            else if marker = marker_app0 then begin
              match parse_app0 data content_start content_len with
              | Some jfif -> APP0 jfif
              | None ->
                  Unknown (marker, Bytes.sub data content_start content_len)
            end
            else if marker = marker_app1 then
              APP1 (Bytes.sub data content_start content_len)
            else if marker = marker_com then
              COM (Bytes.sub_string data content_start content_len)
            else Unknown (marker, Bytes.sub data content_start content_len)
          in

          markers := segment :: !markers;
          pos := !pos + seg_len
        end
  done;

  List.rev !markers

(** Write a 16-bit big-endian value to buffer *)
let write_u16 buf value =
  Buffer.add_uint8 buf ((value lsr 8) land 0xFF);
  Buffer.add_uint8 buf (value land 0xFF)

(** Write marker segment to buffer *)
let write_marker buf marker_byte =
  Buffer.add_uint8 buf 0xFF;
  Buffer.add_uint8 buf marker_byte

(** Write SOF (common for SOF0/SOF2) *)
let write_sof buf marker_byte frame =
  write_marker buf marker_byte;
  let len = 8 + (Array.length frame.components * 3) in
  write_u16 buf len;
  Buffer.add_uint8 buf frame.precision;
  write_u16 buf frame.height;
  write_u16 buf frame.width;
  Buffer.add_uint8 buf (Array.length frame.components);
  Array.iter
    (fun comp ->
      Buffer.add_uint8 buf comp.component_id;
      Buffer.add_uint8 buf ((comp.h_sampling lsl 4) lor comp.v_sampling);
      Buffer.add_uint8 buf comp.quant_table_id)
    frame.components

(** Write SOF0 *)
let write_sof0 buf frame = write_sof buf marker_sof0 frame

(** Write SOF2 *)
let write_sof2 buf frame = write_sof buf marker_sof2 frame

(** Write SOF9 *)
let write_sof9 buf frame = write_sof buf marker_sof9 frame

(** Write SOF10 *)
let write_sof10 buf frame = write_sof buf marker_sof10 frame

(** Write DHT *)
let write_dht buf tables =
  List.iter
    (fun table ->
      write_marker buf marker_dht;
      let total_symbols = Array.fold_left ( + ) 0 table.counts in
      let len = 2 + 1 + 16 + total_symbols in
      write_u16 buf len;
      Buffer.add_uint8 buf ((table.table_class lsl 4) lor table.table_id);
      Array.iter (fun c -> Buffer.add_uint8 buf c) table.counts;
      Array.iter (fun v -> Buffer.add_uint8 buf v) table.values)
    tables

(** Write DAC (Define Arithmetic Conditioning) *)
let write_dac buf (tables : arithmetic_conditioning list) =
  if tables <> [] then begin
    write_marker buf marker_dac;
    let len = 2 + (List.length tables * 2) in
    write_u16 buf len;
    List.iter
      (fun (table : arithmetic_conditioning) ->
        Buffer.add_uint8 buf ((table.table_class lsl 4) lor table.table_id);
        Buffer.add_uint8 buf table.conditioning_value)
      tables
  end

(** Write DQT with support for 8-bit and 16-bit precision *)
let write_dqt buf tables =
  List.iter
    (fun table ->
      write_marker buf marker_dqt;
      let elem_size = if table.precision = 0 then 1 else 2 in
      let len = 2 + 1 + (64 * elem_size) in
      write_u16 buf len;
      Buffer.add_uint8 buf ((table.precision lsl 4) lor table.table_id);
      if table.precision = 0 then
        Array.iter (fun v -> Buffer.add_uint8 buf v) table.values
      else Array.iter (fun v -> write_u16 buf v) table.values)
    tables

(** Write DRI *)
let write_dri buf interval =
  write_marker buf marker_dri;
  write_u16 buf 4;
  write_u16 buf interval

(** Write SOS header *)
let write_sos_header buf header =
  write_marker buf marker_sos;
  let len = 2 + 1 + (Array.length header.scan_components * 2) + 3 in
  write_u16 buf len;
  Buffer.add_uint8 buf (Array.length header.scan_components);
  Array.iter
    (fun comp ->
      Buffer.add_uint8 buf comp.selector;
      Buffer.add_uint8 buf ((comp.dc_table lsl 4) lor comp.ac_table))
    header.scan_components;
  Buffer.add_uint8 buf header.ss;
  Buffer.add_uint8 buf header.se;
  Buffer.add_uint8 buf ((header.ah lsl 4) lor header.al)

(** Write APP0 JFIF *)
let write_app0 buf jfif =
  write_marker buf marker_app0;
  write_u16 buf 16;
  (* length *)
  Buffer.add_string buf "JFIF\x00";
  Buffer.add_uint8 buf jfif.version_major;
  Buffer.add_uint8 buf jfif.version_minor;
  Buffer.add_uint8 buf jfif.density_units;
  write_u16 buf jfif.x_density;
  write_u16 buf jfif.y_density;
  Buffer.add_uint8 buf jfif.thumbnail_width;
  Buffer.add_uint8 buf jfif.thumbnail_height

(** Write APP1 EXIF *)
let write_app1 buf exif_data =
  write_marker buf marker_app1;
  write_u16 buf (2 + Bytes.length exif_data);
  Buffer.add_bytes buf exif_data

(** Write comment *)
let write_com buf text =
  write_marker buf marker_com;
  write_u16 buf (2 + String.length text);
  Buffer.add_string buf text

(** Write all markers to bytes *)
let write_markers markers =
  let buf = Buffer.create 65536 in

  List.iter
    (fun segment ->
      match segment with
      | SOI -> write_marker buf marker_soi
      | EOI -> write_marker buf marker_eoi
      | SOF0 frame -> write_sof0 buf frame
      | SOF2 frame -> write_sof2 buf frame
      | SOF9 frame -> write_sof9 buf frame
      | SOF10 frame -> write_sof10 buf frame
      | DHT tables -> write_dht buf tables
      | DAC tables -> write_dac buf tables
      | DQT tables -> write_dqt buf tables
      | DRI interval -> write_dri buf interval
      | SOS (header, data) ->
          write_sos_header buf header;
          Buffer.add_bytes buf data
      | APP0 jfif -> write_app0 buf jfif
      | APP1 exif_data -> write_app1 buf exif_data
      | COM text -> write_com buf text
      | Unknown (marker, data) ->
          write_marker buf marker;
          write_u16 buf (2 + Bytes.length data);
          Buffer.add_bytes buf data)
    markers;

  Buffer.to_bytes buf
