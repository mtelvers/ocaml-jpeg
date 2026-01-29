(** Color space conversions for JPEG *)

(** Clamp value to 0-255 range *)
let clamp v = max 0 (min 255 v)

(** Clamp float to 0-255 and convert to int *)
let clamp_float v = max 0 (min 255 (int_of_float (v +. 0.5)))

(** Maximum value for given precision (8-bit: 255, 12-bit: 4095) *)
let max_value_for_precision precision = if precision = 8 then 255 else 4095

(** Mid value for given precision (8-bit: 128, 12-bit: 2048) *)
let mid_value_for_precision precision = if precision = 8 then 128 else 2048

(** Clamp value to valid range for given precision *)
let clamp_precision precision v =
  max 0 (min (max_value_for_precision precision) v)

(** Clamp float to valid range for given precision *)
let clamp_float_precision precision v =
  max 0 (min (max_value_for_precision precision) (int_of_float (v +. 0.5)))

(** Convert RGB to YCbCr Y = 0.299*R + 0.587*G + 0.114*B Cb = -0.169*R - 0.331*G
    \+ 0.500*B + 128 Cr = 0.500*R - 0.419*G - 0.081*B + 128 *)
let rgb_to_ycbcr r g b =
  let rf = Float.of_int r in
  let gf = Float.of_int g in
  let bf = Float.of_int b in
  let y = (0.299 *. rf) +. (0.587 *. gf) +. (0.114 *. bf) in
  let cb = (-0.168736 *. rf) -. (0.331264 *. gf) +. (0.5 *. bf) +. 128.0 in
  let cr = (0.5 *. rf) -. (0.418688 *. gf) -. (0.081312 *. bf) +. 128.0 in
  (clamp_float y, clamp_float cb, clamp_float cr)

(** Convert YCbCr to RGB R = Y + 1.402*(Cr-128) G = Y - 0.344*(Cb-128) -
    0.714*(Cr-128) B = Y + 1.772*(Cb-128) *)
let ycbcr_to_rgb y cb cr =
  let yf = Float.of_int y in
  let cbf = Float.of_int cb -. 128.0 in
  let crf = Float.of_int cr -. 128.0 in
  let r = yf +. (1.402 *. crf) in
  let g = yf -. (0.344136 *. cbf) -. (0.714136 *. crf) in
  let b = yf +. (1.772 *. cbf) in
  (clamp_float r, clamp_float g, clamp_float b)

(** Convert RGB to YCbCr with given precision (8-bit or 12-bit). For 12-bit,
    input values are 0-4095 and output uses mid=2048. *)
let rgb_to_ycbcr_precision precision r g b =
  let max_val = Float.of_int (max_value_for_precision precision) in
  let mid = Float.of_int (mid_value_for_precision precision) in
  (* Normalize to 0-1 range *)
  let rf = Float.of_int r /. max_val in
  let gf = Float.of_int g /. max_val in
  let bf = Float.of_int b /. max_val in
  (* YCbCr conversion *)
  let y = ((0.299 *. rf) +. (0.587 *. gf) +. (0.114 *. bf)) *. max_val in
  let cb =
    (((-0.168736 *. rf) -. (0.331264 *. gf) +. (0.5 *. bf)) *. max_val) +. mid
  in
  let cr =
    (((0.5 *. rf) -. (0.418688 *. gf) -. (0.081312 *. bf)) *. max_val) +. mid
  in
  ( clamp_float_precision precision y,
    clamp_float_precision precision cb,
    clamp_float_precision precision cr )

(** Convert YCbCr to RGB with given precision *)
let ycbcr_to_rgb_precision precision y cb cr =
  let max_val = Float.of_int (max_value_for_precision precision) in
  let mid = Float.of_int (mid_value_for_precision precision) in
  let yf = Float.of_int y in
  let cbf = Float.of_int cb -. mid in
  let crf = Float.of_int cr -. mid in
  (* Scale factors are the same, but we work in the precision's range *)
  let scale = max_val /. 255.0 in
  let r = yf +. (1.402 *. crf /. scale *. scale) in
  let g =
    yf
    -. (0.344136 *. cbf /. scale *. scale)
    -. (0.714136 *. crf /. scale *. scale)
  in
  let b = yf +. (1.772 *. cbf /. scale *. scale) in
  ( clamp_float_precision precision r,
    clamp_float_precision precision g,
    clamp_float_precision precision b )

(** Level shift: subtract 128 (for encoding, before DCT) *)
let level_shift_down value = value - 128

(** Level shift: add 128 (for decoding, after IDCT) *)
let level_shift_up value = value + 128

(** Level shift a block of samples (subtract 128) *)
let level_shift_block_down block =
  Array.map (fun v -> Float.of_int v -. 128.0) block

(** Level shift a block of samples (add 128) *)
let level_shift_block_up block =
  Array.map (fun v -> clamp (int_of_float (v +. 128.5))) block

(** Level shift with precision: subtract mid value (8-bit: 128, 12-bit: 2048) *)
let level_shift_down_precision precision value =
  value - mid_value_for_precision precision

(** Level shift with precision: add mid value *)
let level_shift_up_precision precision value =
  value + mid_value_for_precision precision

(** Level shift a block with precision (subtract mid value) *)
let level_shift_block_down_precision precision block =
  let mid = Float.of_int (mid_value_for_precision precision) in
  Array.map (fun v -> Float.of_int v -. mid) block

(** Level shift a block with precision (add mid value and clamp) *)
let level_shift_block_up_precision precision block =
  let mid = Float.of_int (mid_value_for_precision precision) in
  let max_val = max_value_for_precision precision in
  Array.map
    (fun v -> max 0 (min max_val (int_of_float (v +. mid +. 0.5))))
    block

(** Convert entire RGB buffer to YCbCr components Input: RGB24 array
    (R,G,B,R,G,B,...) Output: (Y array, Cb array, Cr array) *)
let rgb_buffer_to_ycbcr pixels width height =
  let size = width * height in
  let y_plane = Array.make size 0 in
  let cb_plane = Array.make size 0 in
  let cr_plane = Array.make size 0 in

  for i = 0 to size - 1 do
    let r = pixels.(i * 3) in
    let g = pixels.((i * 3) + 1) in
    let b = pixels.((i * 3) + 2) in
    let y, cb, cr = rgb_to_ycbcr r g b in
    y_plane.(i) <- y;
    cb_plane.(i) <- cb;
    cr_plane.(i) <- cr
  done;

  (y_plane, cb_plane, cr_plane)

(** Convert YCbCr components to RGB buffer Input: Y, Cb, Cr arrays Output: RGB24
    array (R,G,B,R,G,B,...) *)
let ycbcr_to_rgb_buffer y_plane cb_plane cr_plane width height =
  let size = width * height in
  let pixels = Array.make (size * 3) 0 in

  for i = 0 to size - 1 do
    let y = y_plane.(i) in
    let cb = cb_plane.(i) in
    let cr = cr_plane.(i) in
    let r, g, b = ycbcr_to_rgb y cb cr in
    pixels.(i * 3) <- r;
    pixels.((i * 3) + 1) <- g;
    pixels.((i * 3) + 2) <- b
  done;

  pixels

(** Subsample a plane by averaging 2x2 blocks (4:2:0) *)
let subsample_420 plane width height =
  let new_width = (width + 1) / 2 in
  let new_height = (height + 1) / 2 in
  let result = Array.make (new_width * new_height) 0 in

  for y = 0 to new_height - 1 do
    for x = 0 to new_width - 1 do
      let src_x = x * 2 in
      let src_y = y * 2 in
      let sum = ref 0 in
      let count = ref 0 in

      for dy = 0 to 1 do
        for dx = 0 to 1 do
          let sx = src_x + dx in
          let sy = src_y + dy in
          if sx < width && sy < height then begin
            sum := !sum + plane.((sy * width) + sx);
            incr count
          end
        done
      done;

      result.((y * new_width) + x) <- !sum / !count
    done
  done;

  result

(** Subsample a plane horizontally (4:2:2) *)
let subsample_422 plane width height =
  let new_width = (width + 1) / 2 in
  let result = Array.make (new_width * height) 0 in

  for y = 0 to height - 1 do
    for x = 0 to new_width - 1 do
      let src_x = x * 2 in
      let v1 = plane.((y * width) + src_x) in
      let v2 =
        if src_x + 1 < width then plane.((y * width) + src_x + 1) else v1
      in
      result.((y * new_width) + x) <- (v1 + v2) / 2
    done
  done;

  result

(** Upsample a plane by duplicating pixels (4:2:0) *)
let upsample_420 plane orig_width orig_height new_width new_height =
  let result = Array.make (new_width * new_height) 0 in

  for y = 0 to new_height - 1 do
    for x = 0 to new_width - 1 do
      let src_x = min (x / 2) (orig_width - 1) in
      let src_y = min (y / 2) (orig_height - 1) in
      result.((y * new_width) + x) <- plane.((src_y * orig_width) + src_x)
    done
  done;

  result

(** Upsample a plane horizontally (4:2:2) *)
let upsample_422 plane orig_width height new_width =
  let result = Array.make (new_width * height) 0 in

  for y = 0 to height - 1 do
    for x = 0 to new_width - 1 do
      let src_x = min (x / 2) (orig_width - 1) in
      result.((y * new_width) + x) <- plane.((y * orig_width) + src_x)
    done
  done;

  result

(** No subsampling (4:4:4) - identity function for chroma *)
let no_subsample plane _width _height = Array.copy plane

(** Bilinear upsample for 4:2:0 (better quality) *)
let upsample_420_bilinear plane orig_width orig_height new_width new_height =
  let result = Array.make (new_width * new_height) 0 in

  for y = 0 to new_height - 1 do
    for x = 0 to new_width - 1 do
      (* Map to source coordinates *)
      let fx = ((Float.of_int x +. 0.5) /. 2.0) -. 0.5 in
      let fy = ((Float.of_int y +. 0.5) /. 2.0) -. 0.5 in

      let x0 = max 0 (int_of_float fx) in
      let y0 = max 0 (int_of_float fy) in
      let x1 = min (orig_width - 1) (x0 + 1) in
      let y1 = min (orig_height - 1) (y0 + 1) in

      let wx = fx -. Float.of_int x0 in
      let wy = fy -. Float.of_int y0 in
      let wx = max 0.0 (min 1.0 wx) in
      let wy = max 0.0 (min 1.0 wy) in

      let v00 = Float.of_int plane.((y0 * orig_width) + x0) in
      let v01 = Float.of_int plane.((y0 * orig_width) + x1) in
      let v10 = Float.of_int plane.((y1 * orig_width) + x0) in
      let v11 = Float.of_int plane.((y1 * orig_width) + x1) in

      let v =
        (v00 *. (1.0 -. wx) *. (1.0 -. wy))
        +. (v01 *. wx *. (1.0 -. wy))
        +. (v10 *. (1.0 -. wx) *. wy)
        +. (v11 *. wx *. wy)
      in

      result.((y * new_width) + x) <- clamp_float v
    done
  done;

  result

(* === CMYK/YCCK Color Conversions === *)

(** Convert RGB to CMYK. Returns (C, M, Y, K) where each component is in 0-255
    range. Note: JPEG CMYK typically uses inverted values (255-x). *)
let rgb_to_cmyk r g b =
  let r' = Float.of_int r /. 255.0 in
  let g' = Float.of_int g /. 255.0 in
  let b' = Float.of_int b /. 255.0 in
  let k = 1.0 -. max r' (max g' b') in
  if k >= 1.0 then (255, 255, 255, 255) (* Pure black *)
  else
    let c = (1.0 -. r' -. k) /. (1.0 -. k) in
    let m = (1.0 -. g' -. k) /. (1.0 -. k) in
    let y = (1.0 -. b' -. k) /. (1.0 -. k) in
    (* JPEG uses inverted CMYK values *)
    ( clamp_float ((1.0 -. c) *. 255.0),
      clamp_float ((1.0 -. m) *. 255.0),
      clamp_float ((1.0 -. y) *. 255.0),
      clamp_float ((1.0 -. k) *. 255.0) )

(** Convert CMYK to RGB. Input values are in 0-255 range (inverted CMYK as used
    in JPEG). *)
let cmyk_to_rgb c m y k =
  (* Convert from inverted CMYK to standard CMYK *)
  let c' = (255.0 -. Float.of_int c) /. 255.0 in
  let m' = (255.0 -. Float.of_int m) /. 255.0 in
  let y' = (255.0 -. Float.of_int y) /. 255.0 in
  let k' = (255.0 -. Float.of_int k) /. 255.0 in
  (* CMYK to RGB conversion *)
  let r = (1.0 -. c') *. (1.0 -. k') *. 255.0 in
  let g = (1.0 -. m') *. (1.0 -. k') *. 255.0 in
  let b = (1.0 -. y') *. (1.0 -. k') *. 255.0 in
  (clamp_float r, clamp_float g, clamp_float b)

(** Convert RGB to YCCK (YCbCr + K). Returns (Y, Cb, Cr, K) where each component
    is in 0-255 range. YCCK stores CMY as YCbCr and keeps K separate. Adobe uses
    inverted CMYK where 255 = no ink, 0 = full ink. *)
let rgb_to_ycck r g b =
  (* First convert RGB to inverted CMYK *)
  let c, m, y_cmyk, k = rgb_to_cmyk r g b in
  (* Convert CMY (treated as RGB-like values) to YCbCr *)
  let y_val, cb, cr = rgb_to_ycbcr c m y_cmyk in
  (y_val, cb, cr, k)

(** Convert YCCK to RGB. Input (Y, Cb, Cr, K) in 0-255 range. *)
let ycck_to_rgb y cb cr k =
  (* First convert YCbCr back to CMY values *)
  let c, m, y_cmyk = ycbcr_to_rgb y cb cr in
  (* Then convert CMYK to RGB *)
  cmyk_to_rgb c m y_cmyk k

(** Convert RGB buffer to CMYK planes. Input: RGB24 array (R,G,B,R,G,B,...)
    Output: (C array, M array, Y array, K array) *)
let rgb_buffer_to_cmyk pixels width height =
  let size = width * height in
  let c_plane = Array.make size 0 in
  let m_plane = Array.make size 0 in
  let y_plane = Array.make size 0 in
  let k_plane = Array.make size 0 in

  for i = 0 to size - 1 do
    let r = pixels.(i * 3) in
    let g = pixels.((i * 3) + 1) in
    let b = pixels.((i * 3) + 2) in
    let c, m, y, k = rgb_to_cmyk r g b in
    c_plane.(i) <- c;
    m_plane.(i) <- m;
    y_plane.(i) <- y;
    k_plane.(i) <- k
  done;

  (c_plane, m_plane, y_plane, k_plane)

(** Convert RGB buffer to YCCK planes. Input: RGB24 array (R,G,B,R,G,B,...)
    Output: (Y array, Cb array, Cr array, K array) *)
let rgb_buffer_to_ycck pixels width height =
  let size = width * height in
  let y_plane = Array.make size 0 in
  let cb_plane = Array.make size 0 in
  let cr_plane = Array.make size 0 in
  let k_plane = Array.make size 0 in

  for i = 0 to size - 1 do
    let r = pixels.(i * 3) in
    let g = pixels.((i * 3) + 1) in
    let b = pixels.((i * 3) + 2) in
    let y, cb, cr, k = rgb_to_ycck r g b in
    y_plane.(i) <- y;
    cb_plane.(i) <- cb;
    cr_plane.(i) <- cr;
    k_plane.(i) <- k
  done;

  (y_plane, cb_plane, cr_plane, k_plane)

(** Convert CMYK planes to RGB buffer. Input: C, M, Y, K arrays Output: RGB24
    array (R,G,B,R,G,B,...) *)
let cmyk_to_rgb_buffer c_plane m_plane y_plane k_plane width height =
  let size = width * height in
  let pixels = Array.make (size * 3) 0 in

  for i = 0 to size - 1 do
    let c = c_plane.(i) in
    let m = m_plane.(i) in
    let y = y_plane.(i) in
    let k = k_plane.(i) in
    let r, g, b = cmyk_to_rgb c m y k in
    pixels.(i * 3) <- r;
    pixels.((i * 3) + 1) <- g;
    pixels.((i * 3) + 2) <- b
  done;

  pixels

(** Convert YCCK planes to RGB buffer. Input: Y, Cb, Cr, K arrays Output: RGB24
    array (R,G,B,R,G,B,...) *)
let ycck_to_rgb_buffer y_plane cb_plane cr_plane k_plane width height =
  let size = width * height in
  let pixels = Array.make (size * 3) 0 in

  for i = 0 to size - 1 do
    let y = y_plane.(i) in
    let cb = cb_plane.(i) in
    let cr = cr_plane.(i) in
    let k = k_plane.(i) in
    let r, g, b = ycck_to_rgb y cb cr k in
    pixels.(i * 3) <- r;
    pixels.((i * 3) + 1) <- g;
    pixels.((i * 3) + 2) <- b
  done;

  pixels
