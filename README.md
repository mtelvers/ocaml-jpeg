# jpeg - Pure OCaml JPEG Library

A native OCaml library for reading and writing baseline sequential JPEG/JFIF files, with no external C dependencies.

## Features

- **Pure OCaml** - No C bindings or external dependencies
- **Read & Write** - Full support for encoding and decoding
- **Baseline JPEG** - SOF0 (baseline sequential DCT)
- **8-bit precision** - Standard 8 bits per sample
- **Color support** - Grayscale (1 component) and YCbCr (3 components)
- **Chroma subsampling** - 4:4:4, 4:2:2, and 4:2:0
- **EXIF metadata** - Parse and preserve EXIF data
- **Quality control** - Configurable compression quality (1-100)

## Installation

### From source

```bash
git clone https://github.com/example/jpeg.git
cd jpeg
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

## API Reference

### Types

```ocaml
type pixel_data =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type image = {
  width : int;
  height : int;
  pixels : pixel_data;  (* RGB24: R,G,B,R,G,B,... *)
  exif : Exif.t option;
}
```

### Functions

| Function | Description |
|----------|-------------|
| `read : string -> image` | Read JPEG from file |
| `read_bytes : bytes -> image` | Decode JPEG from memory |
| `write : ?quality:int -> string -> image -> unit` | Write JPEG to file |
| `write_bytes : ?quality:int -> image -> bytes` | Encode JPEG to memory |
| `create_image : int -> int -> pixel_data -> image` | Create image from RGB data |
| `get_pixel : image -> int -> int -> int * int * int` | Get RGB at (x, y) |
| `set_pixel : image -> int -> int -> int -> int -> int -> unit` | Set RGB at (x, y) |

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
| `Jpeg.Color` | RGB ↔ YCbCr conversion |
| `Jpeg.Exif` | EXIF metadata parsing |

## Limitations

The following are **not** currently supported:

- Progressive JPEG (SOF2)
- Arithmetic coding
- 12-bit or 16-bit precision
- CMYK color space
- Lossless JPEG

## Running Tests

```bash
# Unit tests
dune runtest

# Test with real JPEG files
dune exec examples/real_jpeg_test.exe
```

## Benchmarks

Typical performance on a modern system:

| Operation | Image Size | Time |
|-----------|------------|------|
| Decode | 256x256 | ~5ms |
| Encode Q85 | 256x256 | ~8ms |
| Decode | 1024x768 | ~50ms |
| Encode Q85 | 1024x768 | ~80ms |

*Note: This is a pure OCaml implementation prioritizing correctness and portability over raw speed.*

## License

MIT License - see LICENSE file for details.

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

### Development Setup

```bash
git clone https://github.com/example/jpeg.git
cd jpeg
opam install . --deps-only --with-test
dune build
dune runtest
```

## Acknowledgments

- JPEG standard: ITU-T T.81 / ISO/IEC 10918-1
- DCT algorithm based on the AA&N (Arai, Agui, Nakajima) method
- Standard Huffman tables from the JPEG specification
