(** Pure OCaml JPEG library for reading and writing baseline sequential
    JPEG/JFIF files.

    Supported features:
    - Baseline sequential DCT JPEG (SOF0)
    - 8-bit precision
    - Grayscale and YCbCr color (1 or 3 components)
    - Standard Huffman coding
    - All common sampling factors (4:4:4, 4:2:2, 4:2:0)
    - EXIF metadata parsing and preservation *)

(** {1 Internal Modules}
    These modules are exposed for advanced use cases. *)

module Bitstream = Bitstream
module Markers = Markers
module Huffman = Huffman
module Dct = Dct
module Quantization = Quantization
module Color = Color
module Exif = Exif

(** {1 Types} *)

type pixel_data =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** Pixel data stored as RGB24 format in a bigarray. The data is laid out as
    [R,G,B,R,G,B,...] with one byte per channel. *)

type image = {
  width : int;
  height : int;
  pixels : pixel_data;  (** RGB24 format: R,G,B,R,G,B,... *)
  exif : Exif.t option;
}
(** JPEG image with pixel data and optional EXIF metadata. *)

(** {1 Reading JPEG} *)

val read : string -> image
(** [read filename] reads a JPEG image from the given file path.
    @raise Failure if the file cannot be read or is not a valid baseline JPEG.
*)

val read_bytes : bytes -> image
(** [read_bytes data] decodes a JPEG image from bytes in memory.
    @raise Failure if the data is not a valid baseline JPEG. *)

(** {1 Writing JPEG} *)

val write : ?quality:int -> string -> image -> unit
(** [write filename image ~quality] writes an image to the given file path as
    JPEG.
    @param quality
      Compression quality from 1 (worst) to 100 (best). Default is 75.
    @raise Failure if the file cannot be written. *)

val write_bytes : ?quality:int -> image -> bytes
(** [write_bytes image ~quality] encodes an image to JPEG bytes in memory.
    @param quality
      Compression quality from 1 (worst) to 100 (best). Default is 75. *)

(** {1 Image Creation} *)

val create_image : int -> int -> pixel_data -> image
(** [create_image width height pixels] creates a new image from raw RGB24 pixel
    data. The [pixels] bigarray must have length [width * height * 3]. *)

val create_image_with_exif : int -> int -> pixel_data -> Exif.t -> image
(** [create_image_with_exif width height pixels exif] creates a new image with
    EXIF metadata. *)

(** {1 Pixel Access} *)

val get_pixel : image -> int -> int -> int * int * int
(** [get_pixel image x y] returns the (r, g, b) values at position (x, y). *)

val set_pixel : image -> int -> int -> int -> int -> int -> unit
(** [set_pixel image x y r g b] sets the pixel at position (x, y) to (r, g, b).
*)
