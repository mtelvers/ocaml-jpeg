(** Demo of arithmetic coding JPEG generation *)

let () =
  let width = 64 in
  let height = 64 in

  (* Create a test gradient image *)
  let pixels =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (width * height * 3)
  in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let idx = ((y * width) + x) * 3 in
      Bigarray.Array1.set pixels idx (x * 4);
      Bigarray.Array1.set pixels (idx + 1) (y * 4);
      Bigarray.Array1.set pixels (idx + 2) 128
    done
  done;
  let image = Jpeg.create_image width height pixels in

  (* Generate SOF9 (baseline arithmetic) *)
  let sof9_options =
    {
      Jpeg.default_encode_options with
      entropy_coding = Jpeg.Arithmetic;
      encoding_mode = Jpeg.Baseline;
      quality = 85;
    }
  in
  Jpeg.write_with_options sof9_options "test_images/arith_baseline.jpg" image;
  Printf.printf "Generated: test_images/arith_baseline.jpg (SOF9)\n%!";

  (* Generate SOF10 (progressive arithmetic) *)
  let sof10_options =
    {
      Jpeg.default_encode_options with
      entropy_coding = Jpeg.Arithmetic;
      encoding_mode = Jpeg.Progressive;
      quality = 85;
    }
  in
  Jpeg.write_with_options sof10_options "test_images/arith_progressive.jpg"
    image;
  Printf.printf "Generated: test_images/arith_progressive.jpg (SOF10)\n%!";

  (* Also generate Huffman versions for comparison *)
  let huff_baseline =
    {
      Jpeg.default_encode_options with
      entropy_coding = Jpeg.Huffman;
      encoding_mode = Jpeg.Baseline;
      quality = 85;
    }
  in
  Jpeg.write_with_options huff_baseline "test_images/huffman_baseline.jpg" image;
  Printf.printf "Generated: test_images/huffman_baseline.jpg (SOF0)\n%!";

  let huff_progressive =
    {
      Jpeg.default_encode_options with
      entropy_coding = Jpeg.Huffman;
      encoding_mode = Jpeg.Progressive;
      quality = 85;
    }
  in
  Jpeg.write_with_options huff_progressive "test_images/huffman_progressive.jpg"
    image;
  Printf.printf "Generated: test_images/huffman_progressive.jpg (SOF2)\n%!";

  (* Print file sizes for comparison *)
  Printf.printf "\nFile size comparison:\n";
  let files =
    [
      "test_images/arith_baseline.jpg";
      "test_images/arith_progressive.jpg";
      "test_images/huffman_baseline.jpg";
      "test_images/huffman_progressive.jpg";
    ]
  in
  List.iter
    (fun f ->
      let ic = open_in_bin f in
      let size = in_channel_length ic in
      close_in ic;
      Printf.printf "  %s: %d bytes\n" f size)
    files
