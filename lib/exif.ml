(** EXIF metadata parsing and writing *)

(** Common EXIF tag IDs *)
let tag_image_width = 0x0100

let tag_image_height = 0x0101
let tag_bits_per_sample = 0x0102
let tag_compression = 0x0103
let tag_orientation = 0x0112
let tag_x_resolution = 0x011A
let tag_y_resolution = 0x011B
let tag_resolution_unit = 0x0128
let tag_software = 0x0131
let tag_datetime = 0x0132
let tag_exif_ifd = 0x8769
let tag_gps_ifd = 0x8825

(* EXIF sub-IFD tags *)
let tag_exposure_time = 0x829A
let tag_f_number = 0x829D
let tag_iso_speed = 0x8827
let tag_date_time_original = 0x9003
let tag_date_time_digitized = 0x9004
let tag_shutter_speed = 0x9201
let tag_aperture = 0x9202
let tag_focal_length = 0x920A
let tag_color_space = 0xA001
let tag_pixel_x_dimension = 0xA002
let tag_pixel_y_dimension = 0xA003

(** EXIF tag value *)
type tag_value =
  | VByte of int
  | VAscii of string
  | VShort of int
  | VLong of int32
  | VRational of int32 * int32 (* numerator, denominator *)
  | VUndefined of bytes
  | VSLong of int32
  | VSRational of int32 * int32
  | VShortArray of int array
  | VRationalArray of (int32 * int32) array

type ifd_entry = { tag : int; value : tag_value }
(** EXIF IFD entry *)

type t = {
  orientation : int option;
  width : int option;
  height : int option;
  datetime : string option;
  datetime_original : string option;
  software : string option;
  exposure_time : (int * int) option; (* as fraction *)
  f_number : (int * int) option;
  iso : int option;
  focal_length : (int * int) option;
  raw_data : bytes option; (* Original raw EXIF data for preservation *)
}
(** EXIF data structure *)

(** Empty EXIF data *)
let empty =
  {
    orientation = None;
    width = None;
    height = None;
    datetime = None;
    datetime_original = None;
    software = None;
    exposure_time = None;
    f_number = None;
    iso = None;
    focal_length = None;
    raw_data = None;
  }

(** Read 16-bit value with endianness *)
let read_u16 data pos big_endian =
  let b1 = Bytes.get_uint8 data pos in
  let b2 = Bytes.get_uint8 data (pos + 1) in
  if big_endian then (b1 lsl 8) lor b2 else (b2 lsl 8) lor b1

(** Read 32-bit value with endianness *)
let read_u32 data pos big_endian =
  let b1 = Bytes.get_uint8 data pos in
  let b2 = Bytes.get_uint8 data (pos + 1) in
  let b3 = Bytes.get_uint8 data (pos + 2) in
  let b4 = Bytes.get_uint8 data (pos + 3) in
  if big_endian then
    Int32.of_int ((b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4)
  else Int32.of_int ((b4 lsl 24) lor (b3 lsl 16) lor (b2 lsl 8) lor b1)

(** Read a string from data *)
let read_string data pos len =
  let s = Bytes.sub_string data pos len in
  (* Remove null terminator if present *)
  let len = String.length s in
  if len > 0 && s.[len - 1] = '\x00' then String.sub s 0 (len - 1) else s

(** Parse an IFD entry *)
let parse_ifd_entry data pos big_endian tiff_offset =
  let tag = read_u16 data pos big_endian in
  let type_id = read_u16 data (pos + 2) big_endian in
  let count = Int32.to_int (read_u32 data (pos + 4) big_endian) in
  let value_offset = pos + 8 in

  (* Calculate size based on type *)
  let type_size =
    match type_id with
    | 1 | 2 | 7 -> 1 (* BYTE, ASCII, UNDEFINED *)
    | 3 -> 2 (* SHORT *)
    | 4 | 9 -> 4 (* LONG, SLONG *)
    | 5 | 10 -> 8 (* RATIONAL, SRATIONAL *)
    | _ -> 0
  in

  let total_size = type_size * count in
  let actual_offset =
    if total_size <= 4 then value_offset
    else tiff_offset + Int32.to_int (read_u32 data value_offset big_endian)
  in

  (* Parse value based on type *)
  let value =
    if actual_offset < 0 || actual_offset >= Bytes.length data then None
    else
      match type_id with
      | 1 ->
          (* BYTE *)
          if count = 1 then Some (VByte (Bytes.get_uint8 data actual_offset))
          else Some (VUndefined (Bytes.sub data actual_offset count))
      | 2 ->
          (* ASCII *)
          Some (VAscii (read_string data actual_offset count))
      | 3 ->
          (* SHORT *)
          if count = 1 then
            Some (VShort (read_u16 data actual_offset big_endian))
          else
            Some
              (VShortArray
                 (Array.init count (fun i ->
                      read_u16 data (actual_offset + (i * 2)) big_endian)))
      | 4 ->
          (* LONG *)
          Some (VLong (read_u32 data actual_offset big_endian))
      | 5 ->
          (* RATIONAL *)
          if count = 1 then begin
            let num = read_u32 data actual_offset big_endian in
            let den = read_u32 data (actual_offset + 4) big_endian in
            Some (VRational (num, den))
          end
          else
            Some
              (VRationalArray
                 (Array.init count (fun i ->
                      let off = actual_offset + (i * 8) in
                      ( read_u32 data off big_endian,
                        read_u32 data (off + 4) big_endian ))))
      | 7 ->
          (* UNDEFINED *)
          Some
            (VUndefined
               (Bytes.sub data actual_offset
                  (min count (Bytes.length data - actual_offset))))
      | 9 ->
          (* SLONG *)
          Some (VSLong (read_u32 data actual_offset big_endian))
      | 10 ->
          (* SRATIONAL *)
          let num = read_u32 data actual_offset big_endian in
          let den = read_u32 data (actual_offset + 4) big_endian in
          Some (VSRational (num, den))
      | _ -> None
  in

  (tag, value)

(** Parse an IFD (Image File Directory) *)
let parse_ifd data pos big_endian tiff_offset =
  if pos < 0 || pos + 2 > Bytes.length data then ([], 0)
  else begin
    let entry_count = read_u16 data pos big_endian in
    let entries = ref [] in

    for i = 0 to entry_count - 1 do
      let entry_pos = pos + 2 + (i * 12) in
      if entry_pos + 12 <= Bytes.length data then begin
        let tag, value =
          parse_ifd_entry data entry_pos big_endian tiff_offset
        in
        match value with
        | Some v -> entries := { tag; value = v } :: !entries
        | None -> ()
      end
    done;

    (* Next IFD offset *)
    let next_ifd_pos = pos + 2 + (entry_count * 12) in
    let next_ifd =
      if next_ifd_pos + 4 <= Bytes.length data then
        Int32.to_int (read_u32 data next_ifd_pos big_endian)
      else 0
    in

    (List.rev !entries, next_ifd)
  end

(** Get integer value from tag *)
let get_int_value = function
  | VByte n -> Some n
  | VShort n -> Some n
  | VLong n -> Some (Int32.to_int n)
  | VSLong n -> Some (Int32.to_int n)
  | _ -> None

(** Get string value from tag *)
let get_string_value = function VAscii s -> Some s | _ -> None

(** Get rational value from tag *)
let get_rational_value = function
  | VRational (n, d) -> Some (Int32.to_int n, Int32.to_int d)
  | VSRational (n, d) -> Some (Int32.to_int n, Int32.to_int d)
  | _ -> None

(** Find entry by tag *)
let find_entry entries tag = List.find_opt (fun e -> e.tag = tag) entries

(** Find entry and extract value using given extractor *)
let get_entry_value entries tag extractor =
  Option.bind (find_entry entries tag) (fun e -> extractor e.value)

(** Parse EXIF data from APP1 segment *)
let parse data =
  let len = Bytes.length data in

  (* Check for EXIF header *)
  if len < 14 then { empty with raw_data = Some data }
  else if
    not
      (Bytes.get_uint8 data 0 = 0x45
      (* E *)
      && Bytes.get_uint8 data 1 = 0x78
      (* x *)
      && Bytes.get_uint8 data 2 = 0x69
      (* i *)
      && Bytes.get_uint8 data 3 = 0x66
      (* f *)
      && Bytes.get_uint8 data 4 = 0x00
      &&
      (* null *)
      Bytes.get_uint8 data 5 = 0x00)
  then
    (* null *)
    { empty with raw_data = Some data }
  else begin
    let tiff_offset = 6 in

    (* Check byte order *)
    let byte_order = read_u16 data tiff_offset true in
    let big_endian = byte_order = 0x4D4D in
    (* MM = big endian, II = little endian *)

    (* Verify TIFF magic *)
    let magic = read_u16 data (tiff_offset + 2) big_endian in
    if magic <> 42 then { empty with raw_data = Some data }
    else begin
      (* Get IFD0 offset *)
      let ifd0_offset =
        tiff_offset + Int32.to_int (read_u32 data (tiff_offset + 4) big_endian)
      in

      (* Parse IFD0 *)
      let ifd0_entries, _next_ifd =
        parse_ifd data ifd0_offset big_endian tiff_offset
      in

      (* Extract common fields from IFD0 using helper *)
      let orientation =
        get_entry_value ifd0_entries tag_orientation get_int_value
      in
      let width = get_entry_value ifd0_entries tag_image_width get_int_value in
      let height =
        get_entry_value ifd0_entries tag_image_height get_int_value
      in
      let datetime =
        get_entry_value ifd0_entries tag_datetime get_string_value
      in
      let software =
        get_entry_value ifd0_entries tag_software get_string_value
      in

      (* Find and parse EXIF sub-IFD *)
      let exif_ifd_offset =
        get_entry_value ifd0_entries tag_exif_ifd get_int_value
        |> Option.map (fun o -> tiff_offset + o)
      in

      let datetime_original, exposure_time, f_number, iso, focal_length =
        match exif_ifd_offset with
        | Some offset when offset < len ->
            let exif_entries, _ =
              parse_ifd data offset big_endian tiff_offset
            in
            ( get_entry_value exif_entries tag_date_time_original
                get_string_value,
              get_entry_value exif_entries tag_exposure_time get_rational_value,
              get_entry_value exif_entries tag_f_number get_rational_value,
              get_entry_value exif_entries tag_iso_speed get_int_value,
              get_entry_value exif_entries tag_focal_length get_rational_value
            )
        | _ -> (None, None, None, None, None)
      in

      {
        orientation;
        width;
        height;
        datetime;
        datetime_original;
        software;
        exposure_time;
        f_number;
        iso;
        focal_length;
        raw_data = Some data;
      }
    end
  end

(** Write 16-bit value with big-endian byte order *)
let write_u16_be buf value =
  Buffer.add_uint8 buf ((value lsr 8) land 0xFF);
  Buffer.add_uint8 buf (value land 0xFF)

(** Write 32-bit value with big-endian byte order *)
let write_u32_be buf value =
  Buffer.add_uint8 buf ((value lsr 24) land 0xFF);
  Buffer.add_uint8 buf ((value lsr 16) land 0xFF);
  Buffer.add_uint8 buf ((value lsr 8) land 0xFF);
  Buffer.add_uint8 buf (value land 0xFF)

(** Create minimal EXIF data with orientation *)
let create_minimal ?orientation ?software () =
  { empty with orientation; software }

(** Serialize EXIF data to bytes for APP1 segment *)
let to_bytes exif =
  (* If we have raw data, just return it (preserves original EXIF) *)
  match exif.raw_data with
  | Some data -> data
  | None ->
      (* Create minimal EXIF structure *)
      let buf = Buffer.create 256 in

      (* EXIF header *)
      Buffer.add_string buf "Exif\x00\x00";

      (* TIFF header (big-endian) *)
      Buffer.add_string buf "MM";
      (* Big-endian *)
      write_u16_be buf 42;
      (* TIFF magic *)
      write_u32_be buf 8;

      (* IFD0 offset (right after header) *)

      (* Count entries *)
      let entries = ref [] in

      (match exif.orientation with
      | Some o -> entries := (tag_orientation, 3, 1, o) :: !entries
      | None -> ());

      let entry_count = List.length !entries in
      write_u16_be buf entry_count;

      (* Write IFD entries (each is 12 bytes) *)
      List.iter
        (fun (tag, type_id, count, value) ->
          write_u16_be buf tag;
          write_u16_be buf type_id;
          write_u32_be buf count;
          (* Value (if fits in 4 bytes) or offset *)
          if type_id = 3 then begin
            (* SHORT *)
            write_u16_be buf value;
            write_u16_be buf 0 (* padding *)
          end
          else write_u32_be buf value)
        (List.rev !entries);

      (* Next IFD offset (0 = none) *)
      write_u32_be buf 0;

      Buffer.to_bytes buf
