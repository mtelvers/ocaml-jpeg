# jpeg - Pure OCaml JPEG Library

A native OCaml library for reading and writing JPEG/JFIF files, with no external C dependencies. Supports baseline, progressive, arithmetic coding, and lossless modes.

## Features

- **Pure OCaml** - No C bindings or external dependencies
- **Read & Write** - Full support for encoding and decoding
- **Baseline JPEG** - SOF0 (baseline sequential DCT)
- **Progressive JPEG** - SOF2/SOF10 (multi-scan for incremental display)
- **Arithmetic coding** - SOF9/SOF10/SOF11 (MQ-Coder per ITU-T T.81)
- **Lossless JPEG** - SOF3/SOF11 (pixel-perfect compression, 8 predictor modes)
- **8-bit and 12-bit precision**
- **Color support** - Grayscale, YCbCr, CMYK, and YCCK
- **Chroma subsampling** - 4:4:4, 4:2:2, and 4:2:0
- **EXIF metadata** - Parse and preserve EXIF data
- **ICC profiles** - Multi-chunk APP2 support
- **Restart markers** - Configurable DRI/RST intervals
- **Quality control** - Configurable compression quality (1-100)

## Installation

### From source

```bash
git clone https://github.com/mtelvers/ocaml-jpeg.git
cd ocaml-jpeg
opam install . --deps-only
dune build
```

### Using opam (when published)

```bash
opam install jpeg
```

## Usage

### Reading a JPEG file

```ocaml
let image = Jpeg.read "photo.jpg"
let () = Printf.printf "Size: %dx%d\n" image.width image.height

(* Access pixels *)
let (r, g, b) = Jpeg.get_pixel image 0 0
```

### Writing a JPEG file

```ocaml
(* Create an image *)
let width = 256
let height = 256
let pixels = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (width * height * 3)

(* Fill with red *)
for i = 0 to width * height - 1 do
  Bigarray.Array1.set pixels (i * 3) 255;      (* R *)
  Bigarray.Array1.set pixels (i * 3 + 1) 0;    (* G *)
  Bigarray.Array1.set pixels (i * 3 + 2) 0     (* B *)
done;

let image = Jpeg.create_image width height pixels

(* Write with quality 85 *)
Jpeg.write ~quality:85 "output.jpg" image
```

### In-memory encoding/decoding

```ocaml
(* Encode to bytes *)
let jpeg_data : bytes = Jpeg.write_bytes ~quality:90 image

(* Decode from bytes *)
let image = Jpeg.read_bytes jpeg_data
```

### Advanced encoding options

```ocaml
let options = { Jpeg.default_encode_options with
  quality = 90;
  encoding_mode = Progressive;
  entropy_coding = Arithmetic;
  subsampling = Sub_444;
} in
Jpeg.write_with_options options "output.jpg" image
```

### Lossless encoding

```ocaml
let options = { Jpeg.default_encode_options with
  encoding_mode = Lossless;
  predictor = 1;  (* 0 = auto, 1-7 = specific predictor *)
} in
Jpeg.write_with_options options "lossless.jpg" image
```

### Working with EXIF metadata

```ocaml
(* Read EXIF from an image *)
let image = Jpeg.read "photo.jpg" in
match image.exif with
| Some exif ->
  (match exif.Jpeg.Exif.orientation with
   | Some o -> Printf.printf "Orientation: %d\n" o
   | None -> ());
  (match exif.Jpeg.Exif.datetime with
   | Some d -> Printf.printf "Date: %s\n" d
   | None -> ())
| None -> print_endline "No EXIF data"

(* Create image with EXIF *)
let exif = Jpeg.Exif.create_minimal ~orientation:1 ~software:"My App" ()
let image = Jpeg.create_image_with_exif width height pixels exif
```

### Working with ICC profiles

```ocaml
(* Read ICC profile from an image *)
let image = Jpeg.read "photo.jpg" in
match image.icc_profile with
| Some icc -> Printf.printf "ICC profile: %d bytes\n" (Bytes.length (Jpeg.Icc.to_bytes icc))
| None -> print_endline "No ICC profile"

(* Create image with ICC profile *)
let icc = Jpeg.Icc.from_bytes icc_data in
let image = Jpeg.create_image_with_icc width height pixels icc
```

## API Reference

### Types

```ocaml
type pixel_format =
  | RGB24   (* 3 bytes per pixel: R, G, B *)
  | CMYK32  (* 4 bytes per pixel: C, M, Y, K *)

type pixel_data =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type image = {
  width : int;
  height : int;
  pixels : pixel_data;
  pixel_format : pixel_format;
  exif : Exif.t option;
  icc_profile : Icc.t option;
}

type encode_options = {
  quality : int;              (* 1-100 *)
  subsampling : subsampling;  (* Sub_444, Sub_422, Sub_420 *)
  color_mode : color_mode;    (* Color, Grayscale, CMYK, YCCK *)
  encoding_mode : encoding_mode;  (* Baseline, Progressive, Lossless *)
  restart_interval : int;     (* MCUs between RST markers, 0 = disabled *)
  precision : precision;      (* Precision_8 or Precision_12 *)
  entropy_coding : entropy_coding;  (* Huffman or Arithmetic *)
  predictor : int;            (* For lossless: 0 = auto, 1-7 = specific *)
  point_transform : int;      (* For lossless: 0 = none *)
}
```

### Functions

| Function | Description |
|----------|-------------|
| `read : string -> image` | Read JPEG from file |
| `read_bytes : bytes -> image` | Decode JPEG from memory |
| `write : ?quality:int -> string -> image -> unit` | Write JPEG to file (baseline, Huffman) |
| `write_bytes : ?quality:int -> image -> bytes` | Encode JPEG to memory (baseline, Huffman) |
| `write_with_options : encode_options -> string -> image -> unit` | Write JPEG with full encoding options |
| `write_bytes_with_options : encode_options -> image -> bytes` | Encode JPEG to memory with full options |
| `create_image : int -> int -> pixel_data -> image` | Create RGB image |
| `create_image_with_exif : int -> int -> pixel_data -> Exif.t -> image` | Create RGB image with EXIF |
| `create_image_with_icc : int -> int -> pixel_data -> Icc.t -> image` | Create RGB image with ICC profile |
| `create_image_with_metadata : int -> int -> pixel_data -> Exif.t -> Icc.t -> image` | Create RGB image with EXIF and ICC |
| `create_cmyk_image : int -> int -> pixel_data -> image` | Create CMYK image |
| `get_pixel : image -> int -> int -> int * int * int` | Get RGB at (x, y) |
| `set_pixel : image -> int -> int -> int -> int -> int -> unit` | Set RGB at (x, y) |
| `get_cmyk_pixel : image -> int -> int -> int * int * int * int` | Get CMYK at (x, y) |
| `set_cmyk_pixel : image -> int -> int -> int -> int -> int -> int -> unit` | Set CMYK at (x, y) |
| `default_encode_options : encode_options` | Default options (Q75, baseline, Huffman, 4:2:0) |

## Command-Line Tools

The library includes several utility tools:

### jpeg_info - Display JPEG structure

```bash
dune exec tools/jpeg_info.exe -- photo.jpg

# Output:
# File: photo.jpg
#   File size: 12345 bytes
#   [SOI] Start of Image
#   [APP0] JFIF v1.1
#   [SOF0] Baseline DCT
#     Dimensions: 640x480
#     Components: 3
#   ...
```

### jpeg_to_ppm - Convert JPEG to PPM

```bash
dune exec tools/jpeg_to_ppm.exe -- input.jpg output.ppm
```

### jpeg_viewer - Interactive viewer (requires X11)

```bash
dune exec tools/jpeg_viewer.exe -- image1.jpg image2.jpg ...

# Controls:
#   Space/Enter - Next image
#   P/Backspace - Previous image
#   R - Reload
#   Q/Escape - Quit
```

## Internal Modules

For advanced usage, the following modules are exposed:

| Module | Description |
|--------|-------------|
| `Jpeg.Bitstream` | Bit-level I/O with byte stuffing |
| `Jpeg.Markers` | JPEG marker parsing and writing |
| `Jpeg.Huffman` | Huffman encoding/decoding |
| `Jpeg.Dct` | DCT/IDCT transforms |
| `Jpeg.Quantization` | Quantization tables and zig-zag ordering |
| `Jpeg.Color` | RGB/YCbCr/CMYK/YCCK color space conversions |
| `Jpeg.Exif` | EXIF metadata parsing |
| `Jpeg.Arithmetic` | MQ-Coder arithmetic coding (ITU-T T.81) |
| `Jpeg.Icc` | ICC color profile handling |
| `Jpeg.Predictor` | Lossless predictors (ITU-T T.81 Table H.1) |

## Limitations

The following are **not** currently supported:

- 16-bit precision
- Hierarchical JPEG (SOF5, SOF6, SOF7, SOF13, SOF14, SOF15)

## Running Tests

```bash
dune runtest
```

## License

MIT License - see LICENSE file for details.

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

### Development Setup

```bash
git clone https://github.com/mtelvers/ocaml-jpeg.git
cd ocaml-jpeg
opam install . --deps-only --with-test
dune build
dune runtest
```

## Acknowledgments

- JPEG standard: ITU-T T.81 / ISO/IEC 10918-1
- DCT algorithm based on the AA&N (Arai, Agui, Nakajima) method
- Standard Huffman tables from the JPEG specification
- Arithmetic coding based on the MQ-Coder specified in ITU-T T.81
