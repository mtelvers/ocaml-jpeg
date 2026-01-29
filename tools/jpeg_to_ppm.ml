(** Convert JPEG to PPM format (viewable with most image viewers)

    Usage: jpeg_to_ppm input.jpg [output.ppm]

    If output is not specified, writes to stdout or input.ppm *)

let write_ppm oc image =
  let width = image.Jpeg.width in
  let height = image.Jpeg.height in

  (* PPM header *)
  Printf.fprintf oc "P6\n%d %d\n255\n" width height;

  (* Pixel data (binary RGB) *)
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let r, g, b = Jpeg.get_pixel image x y in
      output_char oc (Char.chr r);
      output_char oc (Char.chr g);
      output_char oc (Char.chr b)
    done
  done

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s input.jpg [output.ppm]\n" Sys.argv.(0);
    exit 1
  end;

  let input_file = Sys.argv.(1) in
  let output_file =
    if Array.length Sys.argv >= 3 then Sys.argv.(2)
    else Filename.remove_extension input_file ^ ".ppm"
  in

  Printf.eprintf "Reading: %s\n%!" input_file;
  let image = Jpeg.read input_file in
  Printf.eprintf "Size: %dx%d\n%!" image.Jpeg.width image.Jpeg.height;

  Printf.eprintf "Writing: %s\n%!" output_file;
  let oc = open_out_bin output_file in
  write_ppm oc image;
  close_out oc;

  Printf.eprintf "Done!\n%!"
