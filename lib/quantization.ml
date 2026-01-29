(** Quantization tables and operations for JPEG *)

(** Zig-zag order index mapping (sequential index -> zig-zag position) *)
let zigzag_order =
  [|
    0;
    1;
    8;
    16;
    9;
    2;
    3;
    10;
    17;
    24;
    32;
    25;
    18;
    11;
    4;
    5;
    12;
    19;
    26;
    33;
    40;
    48;
    41;
    34;
    27;
    20;
    13;
    6;
    7;
    14;
    21;
    28;
    35;
    42;
    49;
    56;
    57;
    50;
    43;
    36;
    29;
    22;
    15;
    23;
    30;
    37;
    44;
    51;
    58;
    59;
    52;
    45;
    38;
    31;
    39;
    46;
    53;
    60;
    61;
    54;
    47;
    55;
    62;
    63;
  |]

(** Inverse zig-zag order (zig-zag position -> sequential index) *)
let zigzag_inverse =
  let inv = Array.make 64 0 in
  for i = 0 to 63 do
    inv.(zigzag_order.(i)) <- i
  done;
  inv

(** Standard JPEG luminance quantization table *)
let std_luminance_table =
  [|
    16;
    11;
    10;
    16;
    24;
    40;
    51;
    61;
    12;
    12;
    14;
    19;
    26;
    58;
    60;
    55;
    14;
    13;
    16;
    24;
    40;
    57;
    69;
    56;
    14;
    17;
    22;
    29;
    51;
    87;
    80;
    62;
    18;
    22;
    37;
    56;
    68;
    109;
    103;
    77;
    24;
    35;
    55;
    64;
    81;
    104;
    113;
    92;
    49;
    64;
    78;
    87;
    103;
    121;
    120;
    101;
    72;
    92;
    95;
    98;
    112;
    100;
    103;
    99;
  |]

(** Standard JPEG chrominance quantization table *)
let std_chrominance_table =
  [|
    17;
    18;
    24;
    47;
    99;
    99;
    99;
    99;
    18;
    21;
    26;
    66;
    99;
    99;
    99;
    99;
    24;
    26;
    56;
    99;
    99;
    99;
    99;
    99;
    47;
    66;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
    99;
  |]

(** Scale a quantization table by quality factor (1-100) *)
let scale_table table quality =
  let scale = if quality < 50 then 5000 / quality else 200 - (quality * 2) in
  Array.map
    (fun v ->
      let scaled = ((v * scale) + 50) / 100 in
      max 1 (min 255 scaled))
    table

(** Get luminance table for given quality *)
let luminance_table quality = scale_table std_luminance_table quality

(** Get chrominance table for given quality *)
let chrominance_table quality = scale_table std_chrominance_table quality

(** Quantize a DCT block (input: 64 floats, output: 64 ints in zig-zag order) *)
let quantize block table =
  Array.init 64 (fun i ->
      let zz_pos = zigzag_order.(i) in
      let value = block.(zz_pos) in
      let quant = Float.of_int table.(zz_pos) in
      (* Round to nearest integer *)
      let rounded =
        if value >= 0.0 then int_of_float ((value /. quant) +. 0.5)
        else int_of_float ((value /. quant) -. 0.5)
      in
      rounded)

(** Dequantize a block (input: 64 ints in zig-zag order, output: 64 floats) *)
let dequantize block table =
  let result = Array.make 64 0.0 in
  for i = 0 to 63 do
    let zz_pos = zigzag_order.(i) in
    result.(zz_pos) <- Float.of_int (block.(i) * table.(zz_pos))
  done;
  result

(** Convert block from natural order to zig-zag order *)
let to_zigzag block = Array.init 64 (fun i -> block.(zigzag_order.(i)))

(** Convert block from zig-zag order to natural order (generic helper) *)
let from_zigzag_generic block =
  Array.init 64 (fun i -> block.(zigzag_inverse.(i)))

(** Convert int block from zig-zag order to natural order *)
let from_zigzag block = from_zigzag_generic block

(** Convert float block from zig-zag order to natural order *)
let from_zigzag_float block = from_zigzag_generic block
