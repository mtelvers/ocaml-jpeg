(** Bit-level I/O for JPEG encoding/decoding *)

type reader = {
  data : bytes;
  mutable pos : int; (* byte position *)
  mutable bit_pos : int;
      (* bit position within current byte (0-7, high bit first) *)
  mutable current : int; (* current byte value *)
}
(** Bit reader for decoding *)

type writer = {
  buffer : Buffer.t;
  mutable bits : int; (* accumulated bits *)
  mutable num_bits : int; (* number of bits in accumulator *)
}
(** Bit writer for encoding *)

(** Create a new bit reader from bytes *)
let create_reader data = { data; pos = 0; bit_pos = 8; current = 0 }

(** Create a new bit writer *)
let create_writer () = { buffer = Buffer.create 4096; bits = 0; num_bits = 0 }

(** Check if reader has more data *)
let has_more reader =
  reader.pos < Bytes.length reader.data || reader.bit_pos < 8

(** Get current byte position *)
let byte_position reader = reader.pos

(** Read next byte, handling byte stuffing (FF00 -> FF) and RST markers *)
let rec read_byte_internal reader =
  if reader.pos >= Bytes.length reader.data then raise End_of_file;
  let b = Bytes.get_uint8 reader.data reader.pos in
  reader.pos <- reader.pos + 1;
  (* Handle byte stuffing: FF followed by 00 means just FF *)
  if b = 0xFF && reader.pos < Bytes.length reader.data then (
    match Bytes.get_uint8 reader.data reader.pos with
    | 0x00 ->
        reader.pos <- reader.pos + 1;
        (* skip the stuffed 00 byte *)
        b
    | c when c >= 0xD0 && c <= 0xD7 ->
        (* RST marker - skip both FF and marker byte, then read next actual byte *)
        reader.pos <- reader.pos + 1;
        read_byte_internal reader
    | 0xFF ->
        (* Multiple FF bytes - this FF is padding, continue *)
        b
    | _ ->
        (* Other marker - back up so we don't consume it *)
        reader.pos <- reader.pos - 1;
        b)
  else b

(** Read a single bit (returns 0 or 1) *)
let read_bit reader =
  if reader.bit_pos >= 8 then begin
    reader.current <- read_byte_internal reader;
    reader.bit_pos <- 0
  end;
  let bit = (reader.current lsr (7 - reader.bit_pos)) land 1 in
  reader.bit_pos <- reader.bit_pos + 1;
  bit

(** Read n bits (n <= 16) and return as integer *)
let read_bits reader n =
  if n = 0 then 0
  else begin
    let result = ref 0 in
    for _ = 1 to n do
      result := (!result lsl 1) lor read_bit reader
    done;
    !result
  end

(** Peek at marker (check for FF xx sequence without consuming) *)
let peek_marker reader =
  if reader.pos < Bytes.length reader.data - 1 then
    match
      ( Bytes.get_uint8 reader.data reader.pos,
        Bytes.get_uint8 reader.data (reader.pos + 1) )
    with
    | 0xFF, c when c <> 0x00 && c <> 0xFF -> Some c
    | _ -> None
  else None

(** Align reader to next byte boundary *)
let align_reader reader = reader.bit_pos <- 8

(** Skip n bytes *)
let skip_bytes reader n =
  reader.pos <- reader.pos + n;
  reader.bit_pos <- 8

(** Read raw bytes without bit stuffing handling *)
let read_raw_bytes reader n =
  if reader.pos + n > Bytes.length reader.data then raise End_of_file;
  let result = Bytes.sub reader.data reader.pos n in
  reader.pos <- reader.pos + n;
  reader.bit_pos <- 8;
  result

(** Read a big-endian 16-bit value *)
let read_u16 reader =
  let b1 = Bytes.get_uint8 reader.data reader.pos in
  let b2 = Bytes.get_uint8 reader.data (reader.pos + 1) in
  reader.pos <- reader.pos + 2;
  reader.bit_pos <- 8;
  (b1 lsl 8) lor b2

(** Write a single bit *)
let write_bit writer bit =
  writer.bits <- (writer.bits lsl 1) lor (bit land 1);
  writer.num_bits <- writer.num_bits + 1;
  if writer.num_bits = 8 then begin
    Buffer.add_uint8 writer.buffer writer.bits;
    (* Byte stuffing: if we wrote FF, add 00 *)
    if writer.bits = 0xFF then Buffer.add_uint8 writer.buffer 0x00;
    writer.bits <- 0;
    writer.num_bits <- 0
  end

(** Write n bits from an integer (MSB first) *)
let write_bits writer value n =
  for i = n - 1 downto 0 do
    write_bit writer ((value lsr i) land 1)
  done

(** Flush remaining bits (pad with 1s as per JPEG spec) *)
let flush_writer writer =
  if writer.num_bits > 0 then begin
    (* Pad with 1 bits *)
    let padded =
      (writer.bits lsl (8 - writer.num_bits))
      lor ((1 lsl (8 - writer.num_bits)) - 1)
    in
    Buffer.add_uint8 writer.buffer padded;
    if padded = 0xFF then Buffer.add_uint8 writer.buffer 0x00;
    writer.bits <- 0;
    writer.num_bits <- 0
  end

(** Get the written bytes *)
let get_bytes writer = Buffer.to_bytes writer.buffer

(** Write a big-endian 16-bit value directly to buffer *)
let write_u16 writer value =
  flush_writer writer;
  Buffer.add_uint8 writer.buffer ((value lsr 8) land 0xFF);
  Buffer.add_uint8 writer.buffer (value land 0xFF)

(** Write raw byte directly to buffer *)
let write_raw_byte writer b =
  flush_writer writer;
  Buffer.add_uint8 writer.buffer b

(** Write raw bytes directly to buffer *)
let write_raw_bytes writer data =
  flush_writer writer;
  Buffer.add_bytes writer.buffer data

(** Write a restart marker (RST0-RST7) *)
let write_rst_marker writer counter =
  flush_writer writer;
  Buffer.add_uint8 writer.buffer 0xFF;
  Buffer.add_uint8 writer.buffer (0xD0 lor (counter land 0x07))
