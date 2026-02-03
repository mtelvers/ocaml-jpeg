let test pred w h =
  let pixels = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (w * h * 3) in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let idx = ((y * w) + x) * 3 in
      Bigarray.Array1.set pixels idx (x * 255 / max 1 (w-1));
      Bigarray.Array1.set pixels (idx + 1) (y * 255 / max 1 (h-1));
      Bigarray.Array1.set pixels (idx + 2) ((x+y) * 127 / max 1 (w+h-2))
    done
  done;
  let image = Jpeg.create_image w h pixels in
  let options = {
    Jpeg.quality = 100; subsampling = Jpeg.Sub_444; color_mode = Jpeg.Color;
    encoding_mode = Jpeg.Lossless; restart_interval = 0;
    precision = Jpeg.Precision_8; entropy_coding = Jpeg.Arithmetic;
    predictor = pred; point_transform = 0;
  } in
  let encoded = Jpeg.write_bytes_with_options options image in
  let decoded = Jpeg.read_bytes encoded in
  let diff_count = ref 0 in
  let first_diff = ref None in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let idx = ((y * w) + x) * 3 in
      for c = 0 to 2 do
        if Bigarray.Array1.get image.pixels (idx+c) <> Bigarray.Array1.get decoded.pixels (idx+c) then begin
          if !first_diff = None then first_diff := Some(x,y,c);
          incr diff_count
        end
      done
    done
  done;
  if !diff_count > 0 then
    (match !first_diff with
    | Some (x,y,c) -> Printf.printf "  %dx%d pred %d: FAIL %d diffs (first at (%d,%d) c%d)\n" w h pred !diff_count x y c
    | None -> ())
  else Printf.printf "  %dx%d pred %d: OK (%d bytes)\n" w h pred (Bytes.length encoded)

let () =
  Printf.printf "Smooth gradient with arithmetic lossless:\n";
  let sizes = [(8,8); (16,16); (32,32); (50,50); (64,64); (100,100)] in
  List.iter (fun (w,h) ->
    for pred = 1 to 7 do test pred w h done;
    Printf.printf "\n"
  ) sizes
