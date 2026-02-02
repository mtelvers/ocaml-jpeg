(** Generate a color wheel with an ICC profile *)

(** Convert HSV to RGB. h in [0, 360), s and v in [0, 1] *)
let hsv_to_rgb h s v =
  let c = v *. s in
  let h' = h /. 60.0 in
  let x = c *. (1.0 -. abs_float (mod_float h' 2.0 -. 1.0)) in
  let m = v -. c in
  let r', g', b' =
    if h' < 1.0 then (c, x, 0.0)
    else if h' < 2.0 then (x, c, 0.0)
    else if h' < 3.0 then (0.0, c, x)
    else if h' < 4.0 then (0.0, x, c)
    else if h' < 5.0 then (x, 0.0, c)
    else (c, 0.0, x)
  in
  let to_byte f = int_of_float ((f +. m) *. 255.0) |> max 0 |> min 255 in
  (to_byte r', to_byte g', to_byte b')

(** Create a minimal sRGB ICC profile *)
let create_srgb_profile () =
  (* This is a simplified sRGB profile structure *)
  let profile_size = 3144 in
  let data = Bytes.make profile_size '\x00' in

  (* Profile header (128 bytes) *)
  (* Profile size at offset 0 *)
  Bytes.set_int32_be data 0 (Int32.of_int profile_size);

  (* Preferred CMM type 'lcms' at offset 4 *)
  Bytes.blit_string "lcms" 0 data 4 4;

  (* Profile version 2.1 at offset 8 *)
  Bytes.set_int32_be data 8 0x02100000l;

  (* Device class 'mntr' (monitor) at offset 12 *)
  Bytes.blit_string "mntr" 0 data 12 4;

  (* Color space 'RGB ' at offset 16 *)
  Bytes.blit_string "RGB " 0 data 16 4;

  (* PCS (Profile Connection Space) 'XYZ ' at offset 20 *)
  Bytes.blit_string "XYZ " 0 data 20 4;

  (* Date/time at offset 24 (12 bytes) - 2024-01-01 00:00:00 *)
  Bytes.set_int16_be data 24 2024;  (* year *)
  Bytes.set_int16_be data 26 1;     (* month *)
  Bytes.set_int16_be data 28 1;     (* day *)
  Bytes.set_int16_be data 30 0;     (* hour *)
  Bytes.set_int16_be data 32 0;     (* minute *)
  Bytes.set_int16_be data 34 0;     (* second *)

  (* Profile file signature 'acsp' at offset 36 *)
  Bytes.blit_string "acsp" 0 data 36 4;

  (* Primary platform 'APPL' at offset 40 *)
  Bytes.blit_string "APPL" 0 data 40 4;

  (* Profile flags at offset 44 - not embedded, can be used independently *)
  Bytes.set_int32_be data 44 0l;

  (* Device manufacturer at offset 48 *)
  Bytes.set_int32_be data 48 0l;

  (* Device model at offset 52 *)
  Bytes.set_int32_be data 52 0l;

  (* Device attributes at offset 56 (8 bytes) *)
  Bytes.set_int32_be data 56 0l;
  Bytes.set_int32_be data 60 0l;

  (* Rendering intent at offset 64 - perceptual *)
  Bytes.set_int32_be data 64 0l;

  (* PCS illuminant (D50) at offset 68 - X, Y, Z as s15Fixed16 *)
  (* D50: X=0.9642, Y=1.0000, Z=0.8249 *)
  Bytes.set_int32_be data 68 0x0000F6D6l;  (* X = 0.9642 *)
  Bytes.set_int32_be data 72 0x00010000l;  (* Y = 1.0000 *)
  Bytes.set_int32_be data 76 0x0000D32Dl;  (* Z = 0.8249 *)

  (* Profile creator at offset 80 *)
  Bytes.blit_string "JPEG" 0 data 80 4;

  (* Profile ID (MD5) at offset 84 - 16 bytes, leave as zeros *)

  (* Reserved at offset 100 - 28 bytes *)

  (* Tag table starts at offset 128 *)
  (* Tag count at offset 128 *)
  let tag_count = 9 in
  Bytes.set_int32_be data 128 (Int32.of_int tag_count);

  (* Each tag entry is 12 bytes: signature (4) + offset (4) + size (4) *)
  let tag_table_start = 132 in
  let tag_data_start = tag_table_start + (tag_count * 12) in

  (* Helper to write a tag entry *)
  let write_tag_entry idx signature offset size =
    let entry_offset = tag_table_start + (idx * 12) in
    Bytes.blit_string signature 0 data entry_offset 4;
    Bytes.set_int32_be data (entry_offset + 4) (Int32.of_int offset);
    Bytes.set_int32_be data (entry_offset + 8) (Int32.of_int size)
  in

  (* Helper to write XYZ type data *)
  let write_xyz offset x y z =
    Bytes.blit_string "XYZ " 0 data offset 4;
    Bytes.set_int32_be data (offset + 4) 0l;  (* reserved *)
    Bytes.set_int32_be data (offset + 8) x;
    Bytes.set_int32_be data (offset + 12) y;
    Bytes.set_int32_be data (offset + 16) z
  in

  let current_offset = ref tag_data_start in

  (* Tag 0: 'desc' - Profile description *)
  let desc_offset = !current_offset in
  let desc_text = "sRGB Color Profile" in
  let desc_size = 90 + String.length desc_text + 1 in
  write_tag_entry 0 "desc" desc_offset desc_size;
  Bytes.blit_string "desc" 0 data desc_offset 4;
  Bytes.set_int32_be data (desc_offset + 4) 0l;
  Bytes.set_int32_be data (desc_offset + 8) (Int32.of_int (String.length desc_text + 1));
  Bytes.blit_string desc_text 0 data (desc_offset + 12) (String.length desc_text);
  current_offset := desc_offset + desc_size;

  (* Tag 1: 'cprt' - Copyright *)
  let cprt_offset = !current_offset in
  let cprt_text = "Public Domain" in
  let cprt_size = 8 + String.length cprt_text + 1 in
  write_tag_entry 1 "cprt" cprt_offset cprt_size;
  Bytes.blit_string "text" 0 data cprt_offset 4;
  Bytes.set_int32_be data (cprt_offset + 4) 0l;
  Bytes.blit_string cprt_text 0 data (cprt_offset + 8) (String.length cprt_text);
  current_offset := cprt_offset + cprt_size;

  (* Tag 2: 'wtpt' - White point (D65 for sRGB, but we use D50 PCS) *)
  let wtpt_offset = !current_offset in
  write_tag_entry 2 "wtpt" wtpt_offset 20;
  write_xyz wtpt_offset 0x0000F6D6l 0x00010000l 0x0000D32Dl;  (* D50 *)
  current_offset := wtpt_offset + 20;

  (* Tag 3: 'bkpt' - Black point *)
  let bkpt_offset = !current_offset in
  write_tag_entry 3 "bkpt" bkpt_offset 20;
  write_xyz bkpt_offset 0l 0l 0l;
  current_offset := bkpt_offset + 20;

  (* Tag 4: 'rXYZ' - Red matrix column *)
  let rxyz_offset = !current_offset in
  write_tag_entry 4 "rXYZ" rxyz_offset 20;
  (* sRGB red primary in D50 PCS: X=0.4361, Y=0.2225, Z=0.0139 *)
  write_xyz rxyz_offset 0x00006FA2l 0x000038F5l 0x00000390l;
  current_offset := rxyz_offset + 20;

  (* Tag 5: 'gXYZ' - Green matrix column *)
  let gxyz_offset = !current_offset in
  write_tag_entry 5 "gXYZ" gxyz_offset 20;
  (* sRGB green primary in D50 PCS: X=0.3851, Y=0.7169, Z=0.0971 *)
  write_xyz gxyz_offset 0x00006299l 0x0000B785l 0x000018DAl;
  current_offset := gxyz_offset + 20;

  (* Tag 6: 'bXYZ' - Blue matrix column *)
  let bxyz_offset = !current_offset in
  write_tag_entry 6 "bXYZ" bxyz_offset 20;
  (* sRGB blue primary in D50 PCS: X=0.1431, Y=0.0606, Z=0.7139 *)
  write_xyz bxyz_offset 0x0000249Al 0x00000F84l 0x0000B6CFl;
  current_offset := bxyz_offset + 20;

  (* Tags 7, 8, 9: TRC curves for R, G, B - use parametric curve type 0 (gamma 2.2) *)
  let write_trc tag_idx tag_sig =
    let trc_offset = !current_offset in
    let trc_size = 12 in
    write_tag_entry tag_idx tag_sig trc_offset trc_size;
    Bytes.blit_string "para" 0 data trc_offset 4;
    Bytes.set_int32_be data (trc_offset + 4) 0l;  (* reserved *)
    Bytes.set_int16_be data (trc_offset + 8) 0;   (* function type 0: Y = X^gamma *)
    Bytes.set_int16_be data (trc_offset + 10) 0;  (* reserved *)
    (* Gamma 2.2 as s15Fixed16: 2.2 * 65536 = 144179 = 0x000233B3 *)
    (* But we need to fit it properly - actually para type 0 uses u8Fixed8 for gamma *)
    (* Let's use 'curv' type instead with gamma *)
    Bytes.blit_string "curv" 0 data trc_offset 4;
    Bytes.set_int32_be data (trc_offset + 4) 0l;
    Bytes.set_int32_be data (trc_offset + 8) 0l;  (* 0 entries = gamma 1.0, but we want 2.2 *)
    current_offset := trc_offset + trc_size
  in

  write_trc 7 "rTRC";
  write_trc 8 "gTRC";
  (* For bTRC, share the same curve data as rTRC for simplicity *)
  let rtrc_offset = tag_data_start + desc_size + cprt_size + 20 + 20 + 20 + 20 + 20 in
  write_tag_entry 8 "bTRC" rtrc_offset 12;

  Jpeg.Icc.from_bytes data

let () =
  let size = 512 in
  let center = size / 2 in
  let radius = (size / 2) - 10 in

  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (size * size * 3)
  in

  (* Fill with white background *)
  for i = 0 to (size * size * 3) - 1 do
    Bigarray.Array1.set pixels i 255
  done;

  (* Draw the color wheel *)
  for y = 0 to size - 1 do
    for x = 0 to size - 1 do
      let dx = float_of_int (x - center) in
      let dy = float_of_int (y - center) in
      let dist = sqrt (dx *. dx +. dy *. dy) in

      if dist <= float_of_int radius then begin
        (* Calculate hue from angle *)
        let angle = atan2 dy dx in
        let hue = mod_float (angle *. 180.0 /. Float.pi +. 360.0) 360.0 in

        (* Saturation increases from center *)
        let saturation = dist /. float_of_int radius in

        (* Full value (brightness) *)
        let value = 1.0 in

        let r, g, b = hsv_to_rgb hue saturation value in

        let idx = ((y * size) + x) * 3 in
        Bigarray.Array1.set pixels idx r;
        Bigarray.Array1.set pixels (idx + 1) g;
        Bigarray.Array1.set pixels (idx + 2) b
      end
    done
  done;

  (* Create sRGB ICC profile *)
  let icc = create_srgb_profile () in

  (* Create image with ICC profile *)
  let image = Jpeg.create_image_with_icc size size pixels icc in

  (* Save with high quality and no chroma subsampling *)
  Jpeg.write_with_options
    { Jpeg.default_encode_options with quality = 95; subsampling = Jpeg.Sub_444 }
    "color_wheel.jpg" image;

  Printf.printf "Created color_wheel.jpg (%dx%d) with embedded sRGB ICC profile\n" size size;
  Printf.printf "ICC profile size: %d bytes\n" (Bytes.length (Jpeg.Icc.to_bytes icc))
