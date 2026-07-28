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
  "A first-person raycaster with arbitrary-angle wall segments, an inclined floor and either an inclined ceiling or an open sky, per-wall heights, textures generated in code or read from PNG and JPEG, see-through walls, wall decals, billboarded sprites, distance fog, mouse look with pitch, and a resizable/fullscreen window, built on the tsdl bindings.")
 (depends
  ; Array.find_index, which World uses to resolve a name to an index.
  (ocaml (>= 5.1))
  tsdl
  ; PNG and JPEG decoding for Surface, Image.load and Texture.load. It brings
  ; SDL2_image and its codecs with it, which is the bulk of what a distributable
  ; bundle has to carry.
  tsdl-image
  (alcotest :with-test)
  (ocaml-lsp-server :with-dev-setup)
  ; Pinned, and pinned to the same version .ocamlformat names: the formatting
  ; is a build check, and a check whose tool floats is one that fails on a
  ; machine that did nothing wrong.
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.29.0)))))

; The demos are a package of their own, and the split is the whole of what keeps
; the engine installable. camlcast is a library: it holds no content, reads no
; file and has nothing to put in a prefix's share. camlcast-demo is a program,
; and a program that reads its art off the disk has to carry that art wherever
; it is installed — which the root dune does, into share/camlcast-demo.
;
; Together in one package, either the engine drags the pictures along or the
; executable arrives without them. Apart, each is exactly what it needs to be,
; and `dune exec camlcast-demo` still resolves the name because a public name is
; a public name whatever package it belongs to.

(package
 (name camlcast-demo)
 (synopsis "One small world per camlcast engine feature")
 (description
  "A launcher for twenty-odd demos, one per engine feature, plus a five-room showcase level with all of them at once. Each is a single file under demo/, short enough to read in a sitting. Installed, it carries the art the two demos that read from disk need; the way to a copy that runs on a machine with neither OCaml nor SDL is tools/bundle-*.sh.")
 (depends
  (ocaml (>= 5.1))
  (camlcast (= :version))))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
