(lang dune 3.21)

(name camlcast)

; A stanza may only use what it names. Left implicit, a library's dependencies
; are visible to everything that depends on it, so a module can come to lean on
; something no dune file in its directory mentions — and the dependency is then
; real but invisible, removable only by accident. This repo cares about that
; more than most: loom/dune's empty (libraries) is the rule that keeps the
; reconciler from knowing what a pixel is, and test/dune keeps three suites
; apart on the same principle. A boundary the build enforces one directory at a
; time should not be porous the moment it is crossed transitively.
;
; It cost one line to turn on: test_loom reaches QCheck2 through qcheck-alcotest
; and now says qcheck-core out loud.
;
; It also costs a compiler. Dune says what it means here through -H, the hidden
; include the compiler grew in 5.2: a transitive dependency stays readable to
; the typechecker and unnameable in the source, which is the distinction the
; paragraph above is about. Without -H there is no such flag to pass, so dune
; drops those directories rather than hiding them — and a type that only passes
; through one stops resolving. Alcotest.testable takes an 'a Fmt.t, and
; camlcast.mli aliases loom's Element — neither of which any file here names,
; and both of which stop compiling. Hence the floor below.

(implicit_transitive_deps false)

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
  ; -H, and so (implicit_transitive_deps false) above meaning what it says. The
  ; code itself asks for less — Array.find_index, which World uses to resolve a
  ; name to an index, is the newest thing in the tree and arrived in 5.1 — so
  ; what binds here is the build's own rule rather than a line of OCaml. CI
  ; builds and tests at this version as well as at the one development happens
  ; on, so it is a floor that is checked rather than one merely asserted.
  (ocaml (>= 5.2))
  tsdl
  ; PNG and JPEG decoding for Bitmap, Image.load and Texture.load. It brings
  ; SDL2_image and its codecs with it, which is the bulk of what a distributable
  ; bundle has to carry.
  tsdl-image
  (alcotest :with-test)
  ; Properties, where the engine's own suites are examples. The two things the
  ; declarative layer adds have invariants crisp enough to state outright —
  ; mounting A and then B leaves the same instances as mounting B directly, an
  ; auto-wound outline encloses positive area whatever order its points arrive
  ; in — and those are worth checking against generated input rather than
  ; against the handful of cases anyone thinks to write down.
  (qcheck-alcotest :with-test)
  ; QCheck2, where the generators and shrinkers themselves live. qcheck-alcotest
  ; brings it either way; it is named because (implicit_transitive_deps false)
  ; means test/dune has to name it, and a library a dune file names is one the
  ; package should ask for rather than inherit.
  (qcheck-core :with-test)
  ; The declarative layer assembles a World every frame where the old API built
  ; one and kept it, so "is that too slow?" becomes a question this repo has to
  ; be able to answer. It could not before: there is no benchmark here at all,
  ; and the only performance numbers anywhere are a comment in demo/dust.ml.
  ; bechamel needs nothing but fmt.
  (bechamel :with-test)
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
  (ocaml (>= 5.2))
  (camlcast (= :version))
  ; The demos' own suites are in test/ and belong to this package, so
  ; `-p camlcast-demo @runtest` needs the framework they are written in.
  (alcotest :with-test)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
