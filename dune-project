(lang dune 3.21)

(name camlcast)

; A stanza may only use what it names. By default a library's dependencies are
; visible to everything that depends on it, so a module can come to lean on
; something no dune file in its directory mentions — and this repo keeps its
; boundaries in those dune files: loom/dune's empty (libraries) is what stops
; the reconciler from knowing what a pixel is, and test/dune holds its suites
; apart on the same principle. Closing the transitive hole costs one line in
; test/dune, where test_loom names the qcheck-core it reaches QCheck2 through
; rather than inheriting it from qcheck-alcotest, and a compiler version: dune
; implements the rule with -H, the hidden include added in 5.2, which leaves a
; transitive dependency readable to the typechecker and unnameable in the
; source. Without -H dune drops those directories rather than hiding them, and
; a type that only passes through one stops resolving — Alcotest.testable takes
; an 'a Fmt.t, and camlcast.mli aliases loom's Element. Hence the floor below.

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
  ; Needed for -H, which (implicit_transitive_deps false) above depends on.
  ; The code itself asks for less: Array.find_index, which World uses to
  ; resolve a name to an index, is the newest thing in the tree and arrived in
  ; 5.1. The binding constraint is the build's own rule, not a line of OCaml.
  ; CI builds and tests at this version as well as the development one, so the
  ; floor is checked rather than merely asserted.
  (ocaml (>= 5.2))
  tsdl
  ; PNG and JPEG decoding for Bitmap, Image.load and Texture.load. It brings
  ; SDL2_image and its codecs with it, which is the bulk of what a distributable
  ; bundle has to carry.
  tsdl-image
  (alcotest :with-test)
  ; Property tests, where the engine's own suites are example-based. The
  ; engine's behaviour is specific enough that cases are the honest way to check
  ; it. What the declarative layer adds has invariants instead — rules that hold
  ; for every description it can be handed — and a rule of that shape is better
  ; checked against generated input than against however many hand-written cases
  ; someone thought of.
  (qcheck-alcotest :with-test)
  ; QCheck2, where the generators and shrinkers live. qcheck-alcotest brings
  ; it either way; it is listed because (implicit_transitive_deps false) makes
  ; test/dune name it, and a library a dune file names is one the package
  ; should ask for rather than inherit.
  (qcheck-core :with-test)
  ; For the benchmark in bench/, which measures what assembling a World every
  ; frame costs against what drawing the frame costs. Not :with-dev-setup,
  ; though that is what it is: CI's floor job installs --with-test alone and
  ; then runs `dune build`, which compiles bench/ along with everything else.
  (bechamel :with-test)
  (ocaml-lsp-server :with-dev-setup)
  ; Pinned to the same version .ocamlformat names: formatting is a build
  ; check, and a floating tool version makes the check fail on a machine whose
  ; code is unchanged.
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.29.0)))))

; The demos are a package of their own; the split is what keeps the engine
; installable. camlcast is a library: it holds no content, reads no file and
; has nothing to put in a prefix's share. camlcast-demo is a program, and a
; program that reads its art off the disk has to carry that art wherever it is
; installed — which the root dune does, into share/camlcast-demo.
;
; In one package, either the engine drags the pictures along or the executable
; arrives without them. Split, neither does. `dune exec camlcast-demo` still
; resolves the name: public names resolve regardless of package.

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

; The rewriter is a package of its own, and the split is what keeps the engine's
; dependency list where it was. camlcast does not depend on ppx_camlcast and
; never will: a description built without the rewriter is the description this
; engine always built, one word per element larger and carrying no position.
; The dependency runs the other way round only at the source level -- what the
; rewriter emits is a call to Camlcast.Element.at, resolved in the game being
; preprocessed -- so ppx_camlcast does not depend on camlcast either.

(package
 (name ppx_camlcast)
 (synopsis "Tells a camlcast description where it was written")
 (description
  "A ppx rewriter that wraps each of Camlcast.P's element constructors in Element.at, so a description carries the source position it was written at. That is what lets a tool outside find the expression behind a wall the player is looking at. It does nothing unless CAMLCAST_POSITIONS is set, which is how a release build keeps source paths out of the binary.")
 (depends
  (ocaml (>= 5.2))
  ppxlib))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
