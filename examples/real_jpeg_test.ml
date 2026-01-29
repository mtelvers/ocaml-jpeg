(** Test with real JPEG files created by ImageMagick *)

let test_file filename =
  Printf.printf "Testing: %s\n" filename;
  try
    let image = Jpeg.read filename in
    Printf.printf "  Size: %dx%d\n" image.Jpeg.width image.Jpeg.height;

    (* Sample some pixels *)
    let r, g, b = Jpeg.get_pixel image 0 0 in
    Printf.printf "  Top-left pixel: R=%d G=%d B=%d\n" r g b;

    let cx = image.Jpeg.width / 2 in
    let cy = image.Jpeg.height / 2 in
    let r, g, b = Jpeg.get_pixel image cx cy in
    Printf.printf "  Center pixel: R=%d G=%d B=%d\n" r g b;

    (* Re-encode and decode *)
    let reencoded = Jpeg.write_bytes ~quality:90 image in
    let redecoded = Jpeg.read_bytes reencoded in
    Printf.printf "  Re-encoded size: %d bytes\n" (Bytes.length reencoded);
    Printf.printf "  Re-decoded dimensions: %dx%d\n" redecoded.Jpeg.width
      redecoded.Jpeg.height;

    (* Compare original and re-encoded *)
    let max_diff = ref 0 in
    for y = 0 to min (image.Jpeg.height - 1) (redecoded.Jpeg.height - 1) do
      for x = 0 to min (image.Jpeg.width - 1) (redecoded.Jpeg.width - 1) do
        let r1, g1, b1 = Jpeg.get_pixel image x y in
        let r2, g2, b2 = Jpeg.get_pixel redecoded x y in
        let diff = max (abs (r1 - r2)) (max (abs (g1 - g2)) (abs (b1 - b2))) in
        if diff > !max_diff then max_diff := diff
      done
    done;
    Printf.printf "  Max pixel difference after re-encode: %d\n" !max_diff;
    Printf.printf "  [PASS]\n\n";
    true
  with e ->
    Printf.printf "  [FAIL] %s\n\n" (Printexc.to_string e);
    Printexc.print_backtrace stdout;
    false

let () =
  let test_dir = "test_images" in
  let files = Sys.readdir test_dir in
  let jpg_files =
    Array.to_list files
    |> List.filter (fun f -> Filename.check_suffix f ".jpg")
    |> List.map (fun f -> Filename.concat test_dir f)
  in

  Printf.printf "Found %d JPEG files to test\n\n" (List.length jpg_files);

  let results = List.map test_file jpg_files in
  let passed = List.filter Fun.id results |> List.length in
  let failed = List.length results - passed in

  Printf.printf "========================================\n";
  Printf.printf "Results: %d passed, %d failed\n" passed failed;

  if failed > 0 then exit 1
