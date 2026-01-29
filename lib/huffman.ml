(** Huffman encoding/decoding for JPEG *)

type decode_table = {
  max_code : int array; (* max code value for each length (1-16) *)
  val_ptr : int array; (* index into values for each length *)
  values : int array; (* symbol values *)
}
(** Huffman table for decoding *)

type encode_table = {
  codes : int array; (* Huffman codes for each symbol (0-255) *)
  sizes : int array; (* bit lengths for each symbol (0-255) *)
}
(** Huffman table for encoding *)

type table = { decode : decode_table; encode : encode_table }
(** Combined table *)

(** Build decode table from JPEG DHT data *)
let build_decode_table counts values =
  let max_code = Array.make 17 (-1) in
  let val_ptr = Array.make 17 0 in

  let code = ref 0 in
  let ptr = ref 0 in

  for i = 0 to 15 do
    let count = counts.(i) in
    if count > 0 then begin
      val_ptr.(i + 1) <- !ptr;
      for _ = 1 to count do
        incr ptr
      done;
      max_code.(i + 1) <- !code + count - 1;
      code := (!code + count) lsl 1
    end
    else begin
      val_ptr.(i + 1) <- !ptr;
      max_code.(i + 1) <- -1;
      code := !code lsl 1
    end
  done;

  { max_code; val_ptr; values }

(** Build encode table from JPEG DHT data *)
let build_encode_table counts values =
  let codes = Array.make 256 0 in
  let sizes = Array.make 256 0 in

  let code = ref 0 in
  let idx = ref 0 in

  for length = 1 to 16 do
    for _ = 1 to counts.(length - 1) do
      let symbol = values.(!idx) in
      codes.(symbol) <- !code;
      sizes.(symbol) <- length;
      incr code;
      incr idx
    done;
    code := !code lsl 1
  done;

  { codes; sizes }

(** Build complete table from JPEG DHT data *)
let build_table counts values =
  let decode = build_decode_table counts values in
  let encode = build_encode_table counts values in
  { decode; encode }

(** Decode one symbol using bit reader *)
let decode_symbol reader table =
  let dt = table.decode in
  let min_code = Array.make 17 0 in

  (* Precompute minimum code for each length *)
  let c = ref 0 in
  for len = 1 to 16 do
    min_code.(len) <- !c;
    let count =
      if dt.max_code.(len) < 0 then 0 else dt.max_code.(len) - !c + 1
    in
    c := (!c + count) lsl 1
  done;

  (* Read bits until we find a valid code *)
  let code = ref (Bitstream.read_bit reader) in
  let found = ref None in
  let length = ref 1 in

  while !found = None && !length <= 16 do
    if
      dt.max_code.(!length) >= 0
      && !code >= min_code.(!length)
      && !code <= dt.max_code.(!length)
    then begin
      let idx = dt.val_ptr.(!length) + !code - min_code.(!length) in
      if idx >= 0 && idx < Array.length dt.values then
        found := Some dt.values.(idx)
    end;
    if !found = None && !length < 16 then begin
      code := (!code lsl 1) lor Bitstream.read_bit reader;
      incr length
    end
    else incr length
  done;

  match !found with Some v -> v | None -> failwith "Invalid Huffman code"

(** Encode one symbol using bit writer *)
let encode_symbol writer table symbol =
  let et = table.encode in
  if et.sizes.(symbol) = 0 then
    failwith (Printf.sprintf "No Huffman code for symbol %d" symbol)
  else Bitstream.write_bits writer et.codes.(symbol) et.sizes.(symbol)

(** Extend a value with sign (JPEG EXTEND function) *)
let extend value bits =
  if bits = 0 then 0
  else
    let vt = 1 lsl (bits - 1) in
    if value < vt then value - (1 lsl bits) + 1 else value

(** Get the category (number of bits) for a DC/AC coefficient *)
let category value =
  let v = abs value in
  if v = 0 then 0
  else begin
    let rec count_bits n acc =
      if n = 0 then acc else count_bits (n lsr 1) (acc + 1)
    in
    count_bits v 0
  end

(** Encode a value in the given category *)
let encode_value value cat =
  if cat = 0 then 0 else if value >= 0 then value else value + (1 lsl cat) - 1

(** Standard DC luminance Huffman table *)
let std_dc_luminance_counts =
  [| 0; 1; 5; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 0 |]

let std_dc_luminance_values = [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11 |]

(** Standard DC chrominance Huffman table *)
let std_dc_chrominance_counts =
  [| 0; 3; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0 |]

let std_dc_chrominance_values = [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11 |]

(** Standard AC luminance Huffman table *)
let std_ac_luminance_counts =
  [| 0; 2; 1; 3; 3; 2; 4; 3; 5; 5; 4; 4; 0; 0; 1; 125 |]

let std_ac_luminance_values =
  [|
    0x01;
    0x02;
    0x03;
    0x00;
    0x04;
    0x11;
    0x05;
    0x12;
    0x21;
    0x31;
    0x41;
    0x06;
    0x13;
    0x51;
    0x61;
    0x07;
    0x22;
    0x71;
    0x14;
    0x32;
    0x81;
    0x91;
    0xa1;
    0x08;
    0x23;
    0x42;
    0xb1;
    0xc1;
    0x15;
    0x52;
    0xd1;
    0xf0;
    0x24;
    0x33;
    0x62;
    0x72;
    0x82;
    0x09;
    0x0a;
    0x16;
    0x17;
    0x18;
    0x19;
    0x1a;
    0x25;
    0x26;
    0x27;
    0x28;
    0x29;
    0x2a;
    0x34;
    0x35;
    0x36;
    0x37;
    0x38;
    0x39;
    0x3a;
    0x43;
    0x44;
    0x45;
    0x46;
    0x47;
    0x48;
    0x49;
    0x4a;
    0x53;
    0x54;
    0x55;
    0x56;
    0x57;
    0x58;
    0x59;
    0x5a;
    0x63;
    0x64;
    0x65;
    0x66;
    0x67;
    0x68;
    0x69;
    0x6a;
    0x73;
    0x74;
    0x75;
    0x76;
    0x77;
    0x78;
    0x79;
    0x7a;
    0x83;
    0x84;
    0x85;
    0x86;
    0x87;
    0x88;
    0x89;
    0x8a;
    0x92;
    0x93;
    0x94;
    0x95;
    0x96;
    0x97;
    0x98;
    0x99;
    0x9a;
    0xa2;
    0xa3;
    0xa4;
    0xa5;
    0xa6;
    0xa7;
    0xa8;
    0xa9;
    0xaa;
    0xb2;
    0xb3;
    0xb4;
    0xb5;
    0xb6;
    0xb7;
    0xb8;
    0xb9;
    0xba;
    0xc2;
    0xc3;
    0xc4;
    0xc5;
    0xc6;
    0xc7;
    0xc8;
    0xc9;
    0xca;
    0xd2;
    0xd3;
    0xd4;
    0xd5;
    0xd6;
    0xd7;
    0xd8;
    0xd9;
    0xda;
    0xe1;
    0xe2;
    0xe3;
    0xe4;
    0xe5;
    0xe6;
    0xe7;
    0xe8;
    0xe9;
    0xea;
    0xf1;
    0xf2;
    0xf3;
    0xf4;
    0xf5;
    0xf6;
    0xf7;
    0xf8;
    0xf9;
    0xfa;
  |]

(** Standard AC chrominance Huffman table *)
let std_ac_chrominance_counts =
  [| 0; 2; 1; 2; 4; 4; 3; 4; 7; 5; 4; 4; 0; 1; 2; 119 |]

let std_ac_chrominance_values =
  [|
    0x00;
    0x01;
    0x02;
    0x03;
    0x11;
    0x04;
    0x05;
    0x21;
    0x31;
    0x06;
    0x12;
    0x41;
    0x51;
    0x07;
    0x61;
    0x71;
    0x13;
    0x22;
    0x32;
    0x81;
    0x08;
    0x14;
    0x42;
    0x91;
    0xa1;
    0xb1;
    0xc1;
    0x09;
    0x23;
    0x33;
    0x52;
    0xf0;
    0x15;
    0x62;
    0x72;
    0xd1;
    0x0a;
    0x16;
    0x24;
    0x34;
    0xe1;
    0x25;
    0xf1;
    0x17;
    0x18;
    0x19;
    0x1a;
    0x26;
    0x27;
    0x28;
    0x29;
    0x2a;
    0x35;
    0x36;
    0x37;
    0x38;
    0x39;
    0x3a;
    0x43;
    0x44;
    0x45;
    0x46;
    0x47;
    0x48;
    0x49;
    0x4a;
    0x53;
    0x54;
    0x55;
    0x56;
    0x57;
    0x58;
    0x59;
    0x5a;
    0x63;
    0x64;
    0x65;
    0x66;
    0x67;
    0x68;
    0x69;
    0x6a;
    0x73;
    0x74;
    0x75;
    0x76;
    0x77;
    0x78;
    0x79;
    0x7a;
    0x82;
    0x83;
    0x84;
    0x85;
    0x86;
    0x87;
    0x88;
    0x89;
    0x8a;
    0x92;
    0x93;
    0x94;
    0x95;
    0x96;
    0x97;
    0x98;
    0x99;
    0x9a;
    0xa2;
    0xa3;
    0xa4;
    0xa5;
    0xa6;
    0xa7;
    0xa8;
    0xa9;
    0xaa;
    0xb2;
    0xb3;
    0xb4;
    0xb5;
    0xb6;
    0xb7;
    0xb8;
    0xb9;
    0xba;
    0xc2;
    0xc3;
    0xc4;
    0xc5;
    0xc6;
    0xc7;
    0xc8;
    0xc9;
    0xca;
    0xd2;
    0xd3;
    0xd4;
    0xd5;
    0xd6;
    0xd7;
    0xd8;
    0xd9;
    0xda;
    0xe2;
    0xe3;
    0xe4;
    0xe5;
    0xe6;
    0xe7;
    0xe8;
    0xe9;
    0xea;
    0xf2;
    0xf3;
    0xf4;
    0xf5;
    0xf6;
    0xf7;
    0xf8;
    0xf9;
    0xfa;
  |]

(** Build standard tables *)
let std_dc_luminance_table () =
  build_table std_dc_luminance_counts std_dc_luminance_values

let std_dc_chrominance_table () =
  build_table std_dc_chrominance_counts std_dc_chrominance_values

let std_ac_luminance_table () =
  build_table std_ac_luminance_counts std_ac_luminance_values

let std_ac_chrominance_table () =
  build_table std_ac_chrominance_counts std_ac_chrominance_values
