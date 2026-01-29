(** Display JPEG file information

    Usage: jpeg_info <file.jpg> [file2.jpg ...] *)

let show_info filename =
  Printf.printf "File: %s\n" filename;
  try
    let data =
      let ic = open_in_bin filename in
      let len = in_channel_length ic in
      let data = Bytes.create len in
      really_input ic data 0 len;
      close_in ic;
      data
    in
    Printf.printf "  File size: %d bytes\n" (Bytes.length data);

    (* Parse markers for detailed info *)
    let markers = Jpeg.Markers.parse_markers data in

    List.iter
      (fun marker ->
        match marker with
        | Jpeg.Markers.SOI -> Printf.printf "  [SOI] Start of Image\n"
        | Jpeg.Markers.EOI -> Printf.printf "  [EOI] End of Image\n"
        | Jpeg.Markers.SOF0 frame ->
            Printf.printf "  [SOF0] Baseline DCT\n";
            Printf.printf "    Dimensions: %dx%d\n" frame.Jpeg.Markers.width
              frame.Jpeg.Markers.height;
            Printf.printf "    Precision: %d bits\n"
              frame.Jpeg.Markers.precision;
            Printf.printf "    Components: %d\n"
              (Array.length frame.Jpeg.Markers.components);
            Array.iteri
              (fun i (comp : Jpeg.Markers.component_info) ->
                Printf.printf
                  "      [%d] id=%d, sampling=%dx%d, quant_table=%d\n" i
                  comp.component_id comp.h_sampling comp.v_sampling
                  comp.quant_table_id)
              frame.Jpeg.Markers.components
        | Jpeg.Markers.SOF2 frame ->
            Printf.printf "  [SOF2] Progressive DCT\n";
            Printf.printf "    Dimensions: %dx%d\n" frame.Jpeg.Markers.width
              frame.Jpeg.Markers.height;
            Printf.printf "    Precision: %d bits\n"
              frame.Jpeg.Markers.precision;
            Printf.printf "    Components: %d\n"
              (Array.length frame.Jpeg.Markers.components);
            Array.iteri
              (fun i (comp : Jpeg.Markers.component_info) ->
                Printf.printf
                  "      [%d] id=%d, sampling=%dx%d, quant_table=%d\n" i
                  comp.component_id comp.h_sampling comp.v_sampling
                  comp.quant_table_id)
              frame.Jpeg.Markers.components
        | Jpeg.Markers.DHT tables ->
            Printf.printf "  [DHT] Huffman Tables: %d\n" (List.length tables);
            List.iter
              (fun (t : Jpeg.Markers.huffman_table) ->
                let total = Array.fold_left ( + ) 0 t.counts in
                Printf.printf "    Class=%d (DC/AC), ID=%d, %d symbols\n"
                  t.table_class t.table_id total)
              tables
        | Jpeg.Markers.DQT tables ->
            Printf.printf "  [DQT] Quantization Tables: %d\n"
              (List.length tables);
            List.iter
              (fun (t : Jpeg.Markers.quant_table) ->
                Printf.printf "    ID=%d, precision=%d-bit\n" t.table_id
                  (if t.precision = 0 then 8 else 16))
              tables
        | Jpeg.Markers.DRI interval ->
            Printf.printf "  [DRI] Restart Interval: %d MCUs\n" interval
        | Jpeg.Markers.SOS (header, data) ->
            Printf.printf "  [SOS] Scan Data\n";
            Printf.printf "    Components: %d\n"
              (Array.length header.Jpeg.Markers.scan_components);
            Printf.printf "    Spectral: %d-%d\n" header.Jpeg.Markers.ss
              header.Jpeg.Markers.se;
            Printf.printf "    Data size: %d bytes\n" (Bytes.length data)
        | Jpeg.Markers.APP0 jfif ->
            Printf.printf "  [APP0] JFIF v%d.%d\n"
              jfif.Jpeg.Markers.version_major jfif.Jpeg.Markers.version_minor;
            Printf.printf "    Density: %dx%d (units=%d)\n"
              jfif.Jpeg.Markers.x_density jfif.Jpeg.Markers.y_density
              jfif.Jpeg.Markers.density_units
        | Jpeg.Markers.APP1 data -> (
            Printf.printf "  [APP1] EXIF/XMP (%d bytes)\n" (Bytes.length data);
            let exif = Jpeg.Exif.parse data in
            (match exif.Jpeg.Exif.orientation with
            | Some o -> Printf.printf "    Orientation: %d\n" o
            | None -> ());
            (match exif.Jpeg.Exif.datetime with
            | Some d -> Printf.printf "    DateTime: %s\n" d
            | None -> ());
            match exif.Jpeg.Exif.software with
            | Some s -> Printf.printf "    Software: %s\n" s
            | None -> ())
        | Jpeg.Markers.COM text -> Printf.printf "  [COM] Comment: %s\n" text
        | Jpeg.Markers.Unknown (marker, data) ->
            Printf.printf "  [0x%02X] Unknown (%d bytes)\n" marker
              (Bytes.length data))
      markers;

    (* Also decode to verify *)
    let image = Jpeg.read_bytes data in
    Printf.printf "  Decoded OK: %dx%d RGB\n" image.Jpeg.width image.Jpeg.height;
    Printf.printf "\n"
  with e -> Printf.printf "  ERROR: %s\n\n" (Printexc.to_string e)

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.printf "Usage: %s <file.jpg> [file2.jpg ...]\n" Sys.argv.(0);
    exit 1
  end;

  for i = 1 to Array.length Sys.argv - 1 do
    show_info Sys.argv.(i)
  done
