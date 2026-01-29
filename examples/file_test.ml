(** Test file I/O *)

let () =
  let width = 32 in
  let height = 32 in
  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in

  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let idx = ((y * width) + x) * 3 in
      Bigarray.Array1.set pixels idx 255;
      Bigarray.Array1.set pixels (idx + 1) 128;
      Bigarray.Array1.set pixels (idx + 2) 64
    done
  done;

  let image = Jpeg.create_image width height pixels in

  (* Write to file *)
  let filename = "/tmp/test.jpg" in
  Jpeg.write ~quality:90 filename image;
  Printf.printf "Wrote %s\n" filename;

  (* Read back *)
  let loaded = Jpeg.read filename in
  Printf.printf "Loaded: %dx%d\n" loaded.Jpeg.width loaded.Jpeg.height;

  (* Verify pixel *)
  let r, g, b = Jpeg.get_pixel loaded 16 16 in
  Printf.printf "Center pixel: R=%d G=%d B=%d\n" r g b;

  (* Clean up *)
  Sys.remove filename;
  Printf.printf "File I/O test passed!\n"
