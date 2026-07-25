(lang dune 3.21)

(name camlcast)

(generate_opam_files true)

(source
 (github pharick/camlcast))

(license MIT)

(authors "pharick <artemforkunov@gmail.com>")

(maintainers "pharick <artemforkunov@gmail.com>")

(package
 (name camlcast)
 (synopsis "A raycasting engine in OCaml on SDL2")
 (description
  "A first-person raycaster with arbitrary-angle wall segments, an inclined floor and either an inclined ceiling or an open sky, per-wall heights, procedural textures, see-through walls, wall decals, billboarded sprites, distance fog, mouse look with pitch, and a resizable/fullscreen window, built on the tsdl bindings.")
 (depends
  ; Array.find_index, which World uses to resolve a name to an index.
  (ocaml (>= 5.1))
  tsdl
  (alcotest :with-test)
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat :with-dev-setup)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
