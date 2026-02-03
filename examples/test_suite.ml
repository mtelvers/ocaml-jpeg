(** Comprehensive JPEG encoding test suite.

    Generates 5 synthetic 1000x1000 test images, encodes them with every
    supported SOF standard at multiple quality levels, decodes back, measures
    error vs. the original, and cross-validates with cjpeg/djpeg. *)

open Bigarray

let size = 1000

(* ---------- helpers ---------- *)

let create_pixels () =
  Array1.create int8_unsigned c_layout (size * size * 3)

let set_rgb pixels x y r g b =
  let idx = ((y * size) + x) * 3 in
  Array1.set pixels idx (max 0 (min 255 r));
  Array1.set pixels (idx + 1) (max 0 (min 255 g));
  Array1.set pixels (idx + 2) (max 0 (min 255 b))

let get_rgb pixels x y =
  let idx = ((y * size) + x) * 3 in
  (Array1.get pixels idx, Array1.get pixels (idx + 1), Array1.get pixels (idx + 2))

let write_ppm filename pixels =
  let oc = open_out_bin filename in
  Printf.fprintf oc "P6\n%d %d\n255\n" size size;
  for i = 0 to (size * size * 3) - 1 do
    output_char oc (Char.chr (Array1.get pixels i))
  done;
  close_out oc

let image_of_pixels pixels = Jpeg.create_image size size pixels

(* ---------- HSV -> RGB ---------- *)

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

(* ---------- image generators ---------- *)

let gen_checkerboard () =
  let p = create_pixels () in
  let block = 50 in (* 20x20 grid of 50-pixel squares *)
  for y = 0 to size - 1 do
    for x = 0 to size - 1 do
      let white = ((x / block) + (y / block)) mod 2 = 0 in
      let v = if white then 255 else 0 in
      set_rgb p x y v v v
    done
  done;
  p

let gen_greyscale_fade () =
  let p = create_pixels () in
  for y = 0 to size - 1 do
    for x = 0 to size - 1 do
      (* diagonal fade: 0 at top-left, 255 at bottom-right *)
      let v = ((x + y) * 255) / (2 * (size - 1)) in
      set_rgb p x y v v v
    done
  done;
  p

let gen_colour_wheel () =
  let p = create_pixels () in
  let center = size / 2 in
  let radius = 450.0 in (* 900 px diameter *)
  (* fill background black *)
  for i = 0 to Array1.dim p - 1 do Array1.set p i 0 done;
  for y = 0 to size - 1 do
    for x = 0 to size - 1 do
      let dx = float_of_int (x - center) in
      let dy = float_of_int (y - center) in
      let dist = sqrt (dx *. dx +. dy *. dy) in
      if dist <= radius then begin
        let angle = atan2 dy dx in
        let hue = mod_float (angle *. 180.0 /. Float.pi +. 360.0) 360.0 in
        let sat = dist /. radius in
        let r, g, b = hsv_to_rgb hue sat 1.0 in
        set_rgb p x y r g b
      end
    done
  done;
  p

let gen_colour_gradient () =
  let p = create_pixels () in
  for y = 0 to size - 1 do
    for x = 0 to size - 1 do
      let r = (x * 255) / (size - 1) in
      let g = (y * 255) / (size - 1) in
      let b = 255 - (((x + y) * 255) / (2 * (size - 1))) in
      set_rgb p x y r g b
    done
  done;
  p

let gen_mandelbrot () =
  let p = create_pixels () in
  let max_iter = 1000 in
  (* Zoomed region: real -0.55 to -0.56, imag -0.55 to -0.56 *)
  let re_min = -0.56 and re_max = -0.55 in
  let im_min = -0.56 and im_max = -0.55 in
  for py = 0 to size - 1 do
    for px = 0 to size - 1 do
      let cr = re_min +. (float_of_int px /. float_of_int (size - 1)) *. (re_max -. re_min) in
      let ci = im_min +. (float_of_int py /. float_of_int (size - 1)) *. (im_max -. im_min) in
      let zr = ref 0.0 and zi = ref 0.0 in
      let iter = ref 0 in
      while !iter < max_iter && !zr *. !zr +. !zi *. !zi <= 4.0 do
        let tmp = !zr *. !zr -. !zi *. !zi +. cr in
        zi := 2.0 *. !zr *. !zi +. ci;
        zr := tmp;
        incr iter
      done;
      if !iter = max_iter then
        set_rgb p px py 0 0 0
      else begin
        (* smooth colouring *)
        let t = float_of_int !iter /. float_of_int max_iter in
        let r = int_of_float (255.0 *. (0.5 +. 0.5 *. cos (6.2832 *. t +. 0.0))) in
        let g = int_of_float (255.0 *. (0.5 +. 0.5 *. cos (6.2832 *. t +. 2.094))) in
        let b = int_of_float (255.0 *. (0.5 +. 0.5 *. cos (6.2832 *. t +. 4.189))) in
        set_rgb p px py r g b
      end
    done
  done;
  p

(* ---------- error metrics ---------- *)

type metrics = {
  max_err : int;
  mse : float;
  psnr : float;
  mean_abs : float;
}

let compute_metrics orig decoded =
  let max_err = ref 0 in
  let sum_sq = ref 0.0 in
  let sum_abs = ref 0.0 in
  let n = size * size * 3 in
  for y = 0 to size - 1 do
    for x = 0 to size - 1 do
      let r1, g1, b1 = get_rgb orig x y in
      let r2, g2, b2 = get_rgb decoded x y in
      let process c1 c2 =
        let d = abs (c1 - c2) in
        if d > !max_err then max_err := d;
        sum_sq := !sum_sq +. float_of_int (d * d);
        sum_abs := !sum_abs +. float_of_int d
      in
      process r1 r2; process g1 g2; process b1 b2
    done
  done;
  let mse = !sum_sq /. float_of_int n in
  let psnr =
    if mse = 0.0 then infinity
    else 10.0 *. log10 (255.0 *. 255.0 /. mse)
  in
  { max_err = !max_err; mse; psnr; mean_abs = !sum_abs /. float_of_int n }

(* ---------- SOF configurations ---------- *)

type sof_config = {
  label : string;
  encoding_mode : Jpeg.encoding_mode;
  entropy_coding : Jpeg.entropy_coding;
  qualities : int list;
}

let sof_configs = [
  { label = "SOF0  (Baseline Huffman)";
    encoding_mode = Jpeg.Baseline; entropy_coding = Jpeg.Huffman;
    qualities = [25; 50; 75; 90; 95; 100] };
  { label = "SOF2  (Progressive Huffman)";
    encoding_mode = Jpeg.Progressive; entropy_coding = Jpeg.Huffman;
    qualities = [25; 50; 75; 90; 95; 100] };
  { label = "SOF3  (Lossless Huffman)";
    encoding_mode = Jpeg.Lossless; entropy_coding = Jpeg.Huffman;
    qualities = [100] };
  { label = "SOF9  (Baseline Arithmetic)";
    encoding_mode = Jpeg.Baseline; entropy_coding = Jpeg.Arithmetic;
    qualities = [25; 50; 75; 90; 95; 100] };
  { label = "SOF10 (Progressive Arithmetic)";
    encoding_mode = Jpeg.Progressive; entropy_coding = Jpeg.Arithmetic;
    qualities = [25; 50; 75; 90; 95; 100] };
  { label = "SOF11 (Lossless Arithmetic)";
    encoding_mode = Jpeg.Lossless; entropy_coding = Jpeg.Arithmetic;
    qualities = [100] };
]

(* ---------- external tool availability ---------- *)

let has_command cmd =
  Sys.command (Printf.sprintf "command -v %s >/dev/null 2>&1" cmd) = 0

let have_djpeg = has_command "djpeg"
let have_cjpeg = has_command "cjpeg"

(* ---------- external validation with cjpeg / djpeg ---------- *)

let read_ppm_pixels filename =
  let ic = open_in_bin filename in
  let _magic = input_line ic in (* P6 *)
  (* skip comment lines *)
  let rec read_dim () =
    let line = input_line ic in
    if String.length line > 0 && line.[0] = '#' then read_dim ()
    else line
  in
  let dims = read_dim () in
  let _maxval = input_line ic in
  let parts = String.split_on_char ' ' dims in
  let w = int_of_string (List.nth parts 0) in
  let h = int_of_string (List.nth parts 1) in
  let pixels = Array1.create int8_unsigned c_layout (w * h * 3) in
  for i = 0 to (w * h * 3) - 1 do
    Array1.set pixels i (input_byte ic)
  done;
  close_in ic;
  (w, h, pixels)

let external_validate jpeg_file orig_pixels image_label sof_label quality is_lossless =
  (* Use djpeg to decode our JPEG *)
  if have_djpeg then begin
    let djpeg_ppm = jpeg_file ^ ".djpeg.ppm" in
    let rc = Sys.command
        (Printf.sprintf "djpeg -outfile %s %s 2>/dev/null" djpeg_ppm jpeg_file) in
    if rc = 0 && Sys.file_exists djpeg_ppm then begin
      (try
         let _w, _h, ext_pixels = read_ppm_pixels djpeg_ppm in
         let m = compute_metrics orig_pixels ext_pixels in
         Printf.printf "    djpeg decode  %-18s q=%-3d | max=%3d  MSE=%.2f  PSNR=%.1f dB\n%!"
           image_label quality m.max_err m.mse m.psnr
       with e ->
         Printf.printf "    djpeg decode  %-18s q=%-3d | READ ERROR: %s\n%!"
           image_label quality (Printexc.to_string e))
    end else
      Printf.printf "    djpeg decode  %-18s q=%-3d | SKIPPED (djpeg can't decode %s)\n%!"
        image_label quality sof_label
  end;
  (* Use cjpeg to encode the original PPM, then our library to decode it.
     Only meaningful for lossy DCT modes — cjpeg produces baseline Huffman. *)
  if have_cjpeg && not is_lossless then begin
    let ppm_file = Printf.sprintf "test_images/%s.ppm" image_label in
    let cjpeg_file = jpeg_file ^ ".cjpeg.jpg" in
    let rc2 = Sys.command
        (Printf.sprintf "cjpeg -quality %d -outfile %s %s 2>/dev/null"
           quality cjpeg_file ppm_file) in
    if rc2 = 0 && Sys.file_exists cjpeg_file then begin
      (try
         let cjpeg_image = Jpeg.read cjpeg_file in
         let m = compute_metrics orig_pixels cjpeg_image.pixels in
         Printf.printf "    cjpeg->decode %-18s q=%-3d | max=%3d  MSE=%.2f  PSNR=%.1f dB\n%!"
           image_label quality m.max_err m.mse m.psnr
       with e ->
         Printf.printf "    cjpeg->decode %-18s q=%-3d | ERROR: %s\n%!"
           image_label quality (Printexc.to_string e))
    end
  end

(* ---------- main ---------- *)

let () =
  (* ensure output directory *)
  if not (Sys.file_exists "test_images") then
    Sys.mkdir "test_images" 0o755;

  (* report external tool availability *)
  Printf.printf "External tools: djpeg %s, cjpeg %s\n%!"
    (if have_djpeg then "found" else "NOT FOUND (skipping djpeg validation)")
    (if have_cjpeg then "found" else "NOT FOUND (skipping cjpeg validation)");

  (* generate all test images *)
  Printf.printf "Generating 1000x1000 test images...\n%!";
  let images = [
    ("checkerboard",     gen_checkerboard ());
    ("greyscale_fade",   gen_greyscale_fade ());
    ("colour_wheel",     gen_colour_wheel ());
    ("colour_gradient",  gen_colour_gradient ());
    ("mandelbrot",       gen_mandelbrot ());
  ] in

  (* save original PPM files *)
  List.iter (fun (name, pixels) ->
    let ppm_file = Printf.sprintf "test_images/%s.ppm" name in
    write_ppm ppm_file pixels;
    Printf.printf "  Saved %s\n%!" ppm_file
  ) images;

  Printf.printf "\n";

  (* header *)
  Printf.printf "========================================================================================================\n";
  Printf.printf " JPEG Encoding Test Suite — All SOF Standards\n";
  Printf.printf "========================================================================================================\n";
  Printf.printf " %-36s | %-18s | q   | size KB | max  | MSE      | PSNR dB  | MAE\n" "SOF" "Image";
  Printf.printf "--------------------------------------------------------------------------------------------------------\n%!";

  (* test every combination *)
  List.iter (fun sof ->
    List.iter (fun quality ->
      List.iter (fun (image_label, orig_pixels) ->
        let opts = {
          Jpeg.default_encode_options with
          encoding_mode = sof.encoding_mode;
          entropy_coding = sof.entropy_coding;
          quality;
          subsampling = Jpeg.Sub_444;
          predictor = 4; (* best for lossless *)
        } in

        (* short tag for filenames *)
        let sof_tag = match sof.encoding_mode, sof.entropy_coding with
          | Jpeg.Baseline,    Jpeg.Huffman    -> "sof0"
          | Jpeg.Progressive, Jpeg.Huffman    -> "sof2"
          | Jpeg.Lossless,    Jpeg.Huffman    -> "sof3"
          | Jpeg.Baseline,    Jpeg.Arithmetic -> "sof9"
          | Jpeg.Progressive, Jpeg.Arithmetic -> "sof10"
          | Jpeg.Lossless,    Jpeg.Arithmetic -> "sof11"
        in
        let jpeg_file =
          Printf.sprintf "test_images/%s_%s_q%d.jpg" image_label sof_tag quality
        in

        (* encode *)
        let image = image_of_pixels orig_pixels in
        (try
           Jpeg.write_with_options opts jpeg_file image;

           let file_size = (Unix.stat jpeg_file).Unix.st_size in

           (* decode with our library *)
           let decoded = Jpeg.read jpeg_file in
           let m = compute_metrics orig_pixels decoded.pixels in

           Printf.printf " %-36s | %-18s | %3d | %7.1f | %4d | %8.2f | %8.1f | %.3f\n%!"
             sof.label image_label quality
             (float_of_int file_size /. 1024.0)
             m.max_err m.mse m.psnr m.mean_abs;

           (* save decoded PPM *)
           let dec_ppm = Printf.sprintf "test_images/%s_%s_q%d_decoded.ppm"
               image_label sof_tag quality in
           write_ppm dec_ppm decoded.pixels;

           (* external validation *)
           let is_lossless = sof.encoding_mode = Jpeg.Lossless in
           external_validate jpeg_file orig_pixels image_label sof.label quality is_lossless

         with e ->
           Printf.printf " %-36s | %-18s | %3d | ENCODE ERROR: %s\n%!"
             sof.label image_label quality (Printexc.to_string e))
      ) images
    ) sof.qualities;
    Printf.printf "--------------------------------------------------------------------------------------------------------\n%!"
  ) sof_configs;

  Printf.printf "\n";
  Printf.printf "========================================================================================================\n";
  Printf.printf " Summary\n";
  Printf.printf "========================================================================================================\n";
  Printf.printf " All PPM originals and JPEG files saved in test_images/\n";
  Printf.printf " Decoded PPM files saved alongside for visual comparison.\n";
  Printf.printf " djpeg decode results show error when a third-party decoder reads our JPEGs.\n";
  Printf.printf " cjpeg->decode results show error when we decode JPEGs produced by libjpeg.\n";
  Printf.printf "========================================================================================================\n%!"
