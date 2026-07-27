open Raycaster
open Support

(* Two throwaway patterns: one whose value can be read off by eye, one that
   exercises the masked generator. The named patterns of any particular game are
   its own content and are tested where they live. *)

let checker =
  Texture.generate (fun ~u ~v -> if ((u / 16) + (v / 16)) land 1 = 0 then 240 else 170)

let holes =
  Texture.generate_masked (fun ~u ~v ->
      if u mod 16 < 5 || v mod 16 < 5 then (200, 255) else (0, 0))

(* Every texel is uploaded straight into a byte of an ARGB pixel, so anything
   outside 0..255 would wrap round into a different colour. The generators clamp
   rather than trusting the caller. *)
let texels_are_bytes () =
  let wild = Texture.generate (fun ~u ~v -> (u * 40) - (v * 30) - 400) in
  let worst = ref 128 in
  for v = 0 to Texture.size - 1 do
    for u = 0 to Texture.size - 1 do
      let texel = Texture.sample wild ~u ~v in
      if texel < 0 || texel > 255 then worst := texel
    done
  done;
  Alcotest.(check bool)
    (Printf.sprintf "a generator that overshoots is clamped (saw %d)" !worst)
    true
    (!worst >= 0 && !worst <= 255);
  Alcotest.(check int) "the low end lands on black" 0 (Texture.sample wild ~u:0 ~v:63);
  Alcotest.(check int) "the high end on white" 255 (Texture.sample wild ~u:63 ~v:0)

(* checker is the one pattern whose layout can be read off by eye, so it is
   also the one that pins down which way round sample's u and v go. *)
let sampling_is_row_major () =
  let light = Texture.sample checker ~u:0 ~v:0 in
  Alcotest.(check int)
    "one square across is the other shade"
    (Texture.sample checker ~u:0 ~v:16)
    (Texture.sample checker ~u:16 ~v:0);
  Alcotest.(check bool)
    "and differs from the first" true
    (Texture.sample checker ~u:16 ~v:0 <> light);
  Alcotest.(check int)
    "two squares diagonally is back to the first shade" light
    (Texture.sample checker ~u:16 ~v:16)

(* Ray.offset reaches 1.0 exactly when a ray strikes a corner; without the
   clamp that would index one past the last column. *)
let offsets_map_into_the_texture () =
  Alcotest.(check int) "the left edge" 0 (Texture.column_of_offset checker 0.);
  Alcotest.(check int)
    "just inside the right edge" (Texture.size - 1)
    (Texture.column_of_offset checker 0.999);
  Alcotest.(check int)
    "a corner hit clamps" (Texture.size - 1)
    (Texture.column_of_offset checker 1.0);
  Alcotest.(check int)
    "and so does anything below zero" 0
    (Texture.column_of_offset checker (-0.5));
  Alcotest.(check int)
    "the middle of the face is the middle of the texture" (Texture.size / 2)
    (Texture.column_of_offset checker 0.5);
  (* The offset is a fraction of one world cell and the texture is however many
     texels it says it is, so a denser pattern lands further along for the same
     hit. This is why column_of_offset takes the pattern rather than reading a
     module constant. *)
  let dense = Texture.generate ~size:256 (fun ~u:_ ~v:_ -> 128) in
  Alcotest.(check int)
    "a denser pattern spreads the same face over more columns" 128
    (Texture.column_of_offset dense 0.5)

let generate_masked_flags_transparency () =
  Alcotest.(check bool) "a solid pattern is opaque" true checker.Texture.opaque;
  Alcotest.(check bool)
    "one with holes in it is not" false holes.Texture.opaque;
  Alcotest.(check int) "a bar texel is solid" 255 (Texture.alpha holes ~u:0 ~v:0);
  Alcotest.(check int)
    "a gap is clear" 0
    (Texture.alpha holes ~u:10 ~v:10);
  (* Masked and solid have to agree on the flag, or the renderer would route a
     fully solid masked pattern into the translucent pass for nothing. *)
  let solid_but_masked = Texture.generate_masked (fun ~u:_ ~v:_ -> (100, 255)) in
  Alcotest.(check bool)
    "a masked pattern with no holes is still opaque" true
    solid_but_masked.Texture.opaque

let noise_stays_in_band () =
  let low = ref 255 and high = ref 0 in
  for v = 0 to Texture.size - 1 do
    for u = 0 to Texture.size - 1 do
      let n = Texture.noise ~size:Texture.size ~seed:3 ~cell:8 ~u ~v in
      if n < !low then low := n;
      if n > !high then high := n
    done
  done;
  Alcotest.(check bool)
    (Printf.sprintf "every sample is a byte (%d .. %d)" !low !high)
    true
    (!low >= 0 && !high <= 255);
  Alcotest.(check bool)
    "and the field actually varies" true
    (!high - !low > 32)

(* The reason noise is in the engine rather than in a caller. A wall's pattern
   repeats once per world unit, so a field whose lattice did not close on itself
   would put a hard seam down every wall in the game — one per unit. The value
   one texel before the wrap has to continue smoothly into the value at zero. *)
let noise_wraps_without_a_seam ~size () =
  let n ~u ~v = Texture.noise ~size ~seed:1 ~cell:16 ~u ~v in
  let last = size - 1 in
  let jump a b = abs (a - b) in
  let worst_u = ref 0 and worst_v = ref 0 in
  for k = 0 to last do
    worst_u := Int.max !worst_u (jump (n ~u:last ~v:k) (n ~u:0 ~v:k));
    worst_v := Int.max !worst_v (jump (n ~u:k ~v:last) (n ~u:k ~v:0))
  done;
  (* One texel of a 16-texel cell is a sixteenth of the way between two lattice
     values, and the smoothstep makes the ends flatter still, so the step across
     the seam must be no worse than a step anywhere else. *)
  let interior =
    let worst = ref 0 in
    for v = 0 to last do
      for u = 0 to last - 1 do
        worst := Int.max !worst (jump (n ~u ~v) (n ~u:(u + 1) ~v))
      done
    done;
    !worst
  in
  Alcotest.(check bool)
    (Printf.sprintf "the u seam (%d) is no worse than the interior (%d)"
       !worst_u interior)
    true
    (!worst_u <= interior);
  Alcotest.(check bool)
    (Printf.sprintf "and so is the v seam (%d)" !worst_v)
    true
    (!worst_v <= interior)

let noise_seeds_are_independent () =
  let fingerprint seed =
    List.map
      (fun (u, v) -> Texture.noise ~size:Texture.size ~seed ~cell:8 ~u ~v)
      [ (0, 0); (5, 11); (32, 32); (63, 1) ]
  in
  let fingerprints = List.map fingerprint [ 0; 1; 2; 3 ] in
  Alcotest.(check int)
    "four seeds give four different fields" 4
    (List.length (List.sort_uniq compare fingerprints));
  (* Octaves are summed, so a field that is zero at the origin for every seed
     would put the same dark spot in the corner of every pattern built from it. *)
  Alcotest.(check bool)
    "and none of them is pinned to zero at the origin" true
    (List.exists (fun f -> List.hd f <> 0) fingerprints)

(* A lattice that did not divide the texture could not wrap, so it is refused
   rather than quietly producing the seam it exists to avoid. *)
let noise_refuses_a_lattice_that_cannot_wrap () =
  List.iter
    (fun cell ->
      Alcotest.check_raises
        (Printf.sprintf "cell = %d" cell)
        (Invalid_argument "Texture.noise: cell must divide the pattern size")
        (fun () ->
          ignore (Texture.noise ~size:Texture.size ~seed:0 ~cell ~u:0 ~v:0)))
    [ 0; -4; 7; 48 ];
  (* And the divisor is the size actually being built, not the default one: 48
     does not divide 64 — it is in the list above — but it does divide 96. *)
  let accepted =
    try
      ignore (Texture.noise ~size:96 ~seed:0 ~cell:48 ~u:0 ~v:0);
      true
    with Invalid_argument _ -> false
  in
  Alcotest.(check bool)
    "48 is refused at 64 and accepted at 96" true accepted

(* A pattern is a texel density — so many texels per world cell — rather than a
   resolution, so it may be denser than the default without anything else in the
   engine changing. *)
let patterns_carry_their_own_size () =
  Alcotest.(check int) "the default" 64 checker.Texture.size;
  (* One texel marked, far enough into the pattern that reading it at the wrong
     stride lands somewhere else entirely: at a stride of 64 rather than 128,
     (5, 3) would be flat index 197, which is (69, 1) and not marked. *)
  let dense =
    Texture.generate ~size:128 (fun ~u ~v -> if u = 5 && v = 3 then 200 else 10)
  in
  Alcotest.(check int) "and one that says otherwise" 128 dense.Texture.size;
  Alcotest.(check int)
    "which has that many texels" (128 * 128)
    (Array.length dense.Texture.texels);
  Alcotest.(check int)
    "and is sampled at its own stride" 200
    (Texture.sample dense ~u:5 ~v:3)

(* The three primaries pin the Rec. 601 luma weights a colour file is reduced by;
   the grey ramp pins the row-major order without the weights in the way, since
   the luminance of a grey is itself. Values are literals in tools/make_art.py
   and here, and the encoder there shares no code with the decoder here. *)
let load_reduces_colour_to_brightness () =
  match Texture.load "fixtures/tile.png" with
  | Error (`Msg m) -> Alcotest.fail m
  | Ok t ->
      Alcotest.(check int) "the size comes from the file" 8 t.Texture.size;
      Alcotest.(check bool) "and it is solid throughout" true t.Texture.opaque;
      Alcotest.(check int) "green weighs most" 149 (Texture.sample t ~u:0 ~v:0);
      Alcotest.(check int) "red next" 76 (Texture.sample t ~u:1 ~v:0);
      Alcotest.(check int) "blue least" 29 (Texture.sample t ~u:2 ~v:0);
      Alcotest.(check int) "and white is white" 255 (Texture.sample t ~u:3 ~v:0);
      Alcotest.(check int)
        "a grey texel is its own brightness" 32
        (Texture.sample t ~u:0 ~v:1);
      Alcotest.(check int)
        "one along the row" 60
        (Texture.sample t ~u:7 ~v:1);
      Alcotest.(check int) "every texel is solid" 255 (Texture.alpha t ~u:5 ~v:5)

(* Alpha survives loading, which is what makes a loaded pattern usable as a
   grille or a window: Material.opaque reads exactly this flag to decide whether
   a wall goes through the renderer's translucent pass. *)
let load_keeps_transparency () =
  match Texture.load "fixtures/holes.png" with
  | Error (`Msg m) -> Alcotest.fail m
  | Ok t ->
      Alcotest.(check bool) "a file with holes is not opaque" false
        t.Texture.opaque;
      Alcotest.(check int) "a solid texel" 255 (Texture.alpha t ~u:0 ~v:0);
      Alcotest.(check int) "a clear one" 0 (Texture.alpha t ~u:6 ~v:6)

(* A pattern tiles a square world cell, so a rectangle would be stretched across
   it rather than repeated in it. That is a picture nobody asked for, so it is
   refused and the message says what was wrong with it. *)
let load_refuses_a_rectangle () =
  match Texture.load "fixtures/swatch.png" with
  | Ok _ -> Alcotest.fail "a 4x3 file was accepted as a pattern"
  | Error (`Msg m) ->
      Alcotest.(check bool)
        (Printf.sprintf "the message says what shape it was: %s" m)
        true (mentions m "4x3")

let load_reports_a_missing_file () =
  match Texture.load "fixtures/nothing-here.png" with
  | Ok _ -> Alcotest.fail "a file that is not there loaded"
  | Error (`Msg _) -> ()

let () =
  Alcotest.run "Texture"
    [
      ( "pixels",
        [
          case "texels are bytes" texels_are_bytes;
          case "sampling is row major" sampling_is_row_major;
          case "offsets map into the texture" offsets_map_into_the_texture;
          case "patterns carry their own size" patterns_carry_their_own_size;
          case "generate_masked flags transparency"
            generate_masked_flags_transparency;
        ] );
      ( "noise",
        [
          case "stays in band" noise_stays_in_band;
          case "wraps without a seam"
            (noise_wraps_without_a_seam ~size:Texture.size);
          (* And at a size that is not the default, which is the case a wrapping
             lattice left behind at 64 would fail. *)
          case "wraps without a seam at 128" (noise_wraps_without_a_seam ~size:128);
          case "seeds are independent" noise_seeds_are_independent;
          case "refuses a lattice that cannot wrap"
            noise_refuses_a_lattice_that_cannot_wrap;
        ] );
      ( "loading",
        [
          case "reduces colour to brightness" load_reduces_colour_to_brightness;
          case "keeps transparency" load_keeps_transparency;
          case "refuses a rectangle" load_refuses_a_rectangle;
          case "reports a missing file" load_reports_a_missing_file;
        ] );
    ]
