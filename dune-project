(lang dune 3.0)
(name jpeg)

(generate_opam_files true)

(source (github mtelvers/ocaml-jpeg))
(license MIT)
(authors "Mark Elvers <mark.elvers@tunbury.org>")
(maintainers "Mark Elvers <mark.elvers@tunbury.org>")

(package
 (name jpeg)
 (synopsis "Pure OCaml JPEG library")
 (description "A native OCaml library for reading and writing JPEG/JFIF files with no external C dependencies")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  (alcotest :with-test)
  (graphics :with-test)))
