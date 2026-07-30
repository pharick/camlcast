open Camlcast
open Support

(* Two throwaway patterns: one whose value can be read off by eye, one that
   exercises the masked generator. The named patterns of any particular game are
   its own content and are tested where they live. *)

let grey = Color.rgb 200 200 200

let checker =
  Texture.generate (fun ~u ~v ->
      Color.level grey (if ((u / 16) + (v / 16)) land 1 = 0 then 240 else 170))

let holes =
  Texture.generate_masked (fun ~u ~v ->
      if u mod 16 < 5 || v mod 16 < 5 then (grey, 255) else (Color.rgb 0 0 0, 0))

(* Every channel is uploaded straight into a byte of an ARGB pixel, so anything
   outside 0..255 would wrap round into a different colour. The generators clamp
   rather than trusting the caller, and they clamp each channel: a pattern is
   arithmetic about a base colour, and the three channels do not run out of
   range together. *)
let texels_are_bytes () =
  let wild =
    Texture.generate (fun ~u ~v ->
        Color.rgb ((u * 40) - 400) (v * 30) (600 - (u * 12)))
  in
  let worst = ref 128 in
  for v = 0 to Texture.default_size - 1 do
    for u = 0 to Texture.default_size - 1 do
      let c = Texture.sample wild ~u ~v in
      List.iter
        (fun k -> if k < 0 || k > 255 then worst := k)
        [ c.Color.r; c.Color.g; c.Color.b ]
    done
  done;
  Alcotest.(check bool)
    (Printf.sprintf "a generator that overshoots is clamped (saw %d)" !worst)
    true
    (!worst >= 0 && !worst <= 255);
  (* Opposite corners, where different channels are the ones out of range. *)
  Alcotest.check color "red and green clamp up from below" (Color.rgb 0 0 255)
    (Texture.sample wild ~u:0 ~v:0);
  Alcotest.check color "and blue clamps down from above" (Color.rgb 255 255 0)
    (Texture.sample wild ~u:63 ~v:63)

(* checker is the one pattern whose layout can be read off by eye, so it is
   also the one that pins down which way round sample's u and v go. *)
let sampling_is_row_major () =
  let light = Texture.sample checker ~u:0 ~v:0 in
  Alcotest.check color "one square across is the other shade"
    (Texture.sample checker ~u:0 ~v:16)
    (Texture.sample checker ~u:16 ~v:0);
  Alcotest.(check bool)
    "and differs from the first" true
    (Texture.sample checker ~u:16 ~v:0 <> light);
  Alcotest.check color "two squares diagonally is back to the first shade" light
    (Texture.sample checker ~u:16 ~v:16)

(* Ray.offset reaches 1.0 exactly when a ray strikes a corner; without the
   clamp that would index one past the last column. *)
let offsets_map_into_the_texture () =
  Alcotest.(check int) "the left edge" 0 (Texture.column_of_offset checker 0.);
  Alcotest.(check int)
    "just inside the right edge" (Texture.default_size - 1)
    (Texture.column_of_offset checker 0.999);
  Alcotest.(check int)
    "a corner hit clamps" (Texture.default_size - 1)
    (Texture.column_of_offset checker 1.0);
  Alcotest.(check int)
    "and so does anything below zero" 0
    (Texture.column_of_offset checker (-0.5));
  Alcotest.(check int)
    "the middle of the face is the middle of the texture"
    (Texture.default_size / 2)
    (Texture.column_of_offset checker 0.5);
  (* The offset is a fraction of one world cell and the texture is however many
     texels it says it is, so a denser pattern lands further along for the same
     hit. This is why column_of_offset takes the pattern rather than reading a
     module constant. *)
  let dense = Texture.generate ~size:256 (fun ~u:_ ~v:_ -> grey) in
  Alcotest.(check int)
    "a denser pattern spreads the same face over more columns" 128
    (Texture.column_of_offset dense 0.5)

let generate_masked_flags_transparency () =
  Alcotest.(check bool)
    "a solid pattern is opaque" true (Texture.opaque checker);
  Alcotest.(check bool)
    "one with holes in it is not" false (Texture.opaque holes);
  Alcotest.(check int)
    "a bar texel is solid" 255
    (Texture.alpha holes ~u:0 ~v:0);
  Alcotest.(check int) "a gap is clear" 0 (Texture.alpha holes ~u:10 ~v:10);
  (* Masked and solid have to agree on the flag, or the renderer would route a
     fully solid masked pattern into the translucent pass for nothing. *)
  let solid_but_masked =
    Texture.generate_masked (fun ~u:_ ~v:_ -> (grey, 255))
  in
  Alcotest.(check bool)
    "a masked pattern with no holes is still opaque" true
    (Texture.opaque solid_but_masked)

(* A pattern of no size is refused at the generator, because [size] is what
   every reader of one divides and clamps by: {!Texture.column_of_offset} would
   clamp to [-1] and {!Texture.sample} index an array that is not there, once
   per wall pixel, some frames later. *)
let a_pattern_of_no_size_is_refused () =
  List.iter
    (fun size ->
      Alcotest.check_raises (Printf.sprintf "generate ~size:%d" size)
        (Invalid_argument
           "Texture.generate: a pattern must have a positive size") (fun () ->
          ignore (Texture.generate ~size (fun ~u:_ ~v:_ -> grey)));
      Alcotest.check_raises (Printf.sprintf "generate_masked ~size:%d" size)
        (Invalid_argument
           "Texture.generate_masked: a pattern must have a positive size")
        (fun () ->
          ignore (Texture.generate_masked ~size (fun ~u:_ ~v:_ -> (grey, 255))));
      Alcotest.check_raises (Printf.sprintf "noise ~size:%d" size)
        (Invalid_argument "Texture.noise: a pattern must have a positive size")
        (fun () -> ignore (Texture.noise ~size ~seed:0 ~cell:4 ~u:0 ~v:0)))
    [ 0; -8 ]

let noise_stays_in_band () =
  let low = ref 255 and high = ref 0 in
  for v = 0 to Texture.default_size - 1 do
    for u = 0 to Texture.default_size - 1 do
      let n = Texture.noise ~size:Texture.default_size ~seed:3 ~cell:8 ~u ~v in
      if n < !low then low := n;
      if n > !high then high := n
    done
  done;
  Alcotest.(check bool)
    (Printf.sprintf "every sample is a byte (%d .. %d)" !low !high)
    true
    (!low >= 0 && !high <= 255);
  Alcotest.(check bool) "and the field actually varies" true (!high - !low > 32)

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
    true (!worst_u <= interior);
  Alcotest.(check bool)
    (Printf.sprintf "and so is the v seam (%d)" !worst_v)
    true (!worst_v <= interior)

let noise_seeds_are_independent () =
  let fingerprint seed =
    List.map
      (fun (u, v) ->
        Texture.noise ~size:Texture.default_size ~seed ~cell:8 ~u ~v)
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
      Alcotest.check_raises (Printf.sprintf "cell = %d" cell)
        (Invalid_argument "Texture.noise: cell must divide the pattern size")
        (fun () ->
          ignore
            (Texture.noise ~size:Texture.default_size ~seed:0 ~cell ~u:0 ~v:0)))
    [ 0; -4; 7; 48 ];
  (* And the divisor is the size actually being built, not the default one: 48
     does not divide 64 — it is in the list above — but it does divide 96. *)
  let accepted =
    try
      ignore (Texture.noise ~size:96 ~seed:0 ~cell:48 ~u:0 ~v:0);
      true
    with Invalid_argument _ -> false
  in
  Alcotest.(check bool) "48 is refused at 64 and accepted at 96" true accepted

(* A pattern is a texel density — so many texels per world cell — rather than a
   resolution, so it may be denser than the default without anything else in the
   engine changing. *)
let patterns_carry_their_own_size () =
  Alcotest.(check int) "the default" 64 (Texture.size checker);
  (* One texel marked, far enough into the pattern that reading it at the wrong
     stride lands somewhere else entirely: at a stride of 64 rather than 128,
     (5, 3) would be flat index 197, which is (69, 1) and not marked. *)
  let marked = Color.rgb 200 0 0 and ground = Color.rgb 10 10 10 in
  let dense =
    Texture.generate ~size:128 (fun ~u ~v ->
        if u = 5 && v = 3 then marked else ground)
  in
  Alcotest.(check int) "and one that says otherwise" 128 (Texture.size dense);
  (* Which is what makes the far corner readable at all: [sample] does not
     bounds-check, so a pattern that said 128 and held 64 x 64 texels would read
     off the end of its own array here rather than answer. *)
  Alcotest.check color "and has that many texels to sample" ground
    (Texture.sample dense ~u:127 ~v:127);
  Alcotest.check color "and is sampled at its own stride" marked
    (Texture.sample dense ~u:5 ~v:3)

(* What a file was drawn in is what the wall is made of: nothing is reduced or
   reinterpreted on the way in. Three saturated primaries pin that, and pin the
   channel order with it — a loader that reduced to luma, or read red where blue
   was, cannot agree with all three by accident. The grey ramp then pins the
   row-major order with nothing else in the way. Values are literals in
   tools/make_art.py and here, and the encoder there shares no code with the
   decoder. *)
let load_keeps_the_colour_of_the_file () =
  match Texture.load "fixtures/tile.png" with
  | Error (`Msg m) -> Alcotest.fail m
  | Ok t ->
      Alcotest.(check int) "the size comes from the file" 8 (Texture.size t);
      Alcotest.(check bool) "and it is solid throughout" true (Texture.opaque t);
      Alcotest.check color "green arrives green" (Color.rgb 0 255 0)
        (Texture.sample t ~u:0 ~v:0);
      Alcotest.check color "red arrives red" (Color.rgb 255 0 0)
        (Texture.sample t ~u:1 ~v:0);
      Alcotest.check color "blue arrives blue" (Color.rgb 0 0 255)
        (Texture.sample t ~u:2 ~v:0);
      Alcotest.check color "and white is white" (Color.rgb 255 255 255)
        (Texture.sample t ~u:3 ~v:0);
      Alcotest.check color "the ramp starts one row down" (Color.rgb 32 32 32)
        (Texture.sample t ~u:0 ~v:1);
      Alcotest.check color "and runs along it" (Color.rgb 60 60 60)
        (Texture.sample t ~u:7 ~v:1);
      Alcotest.(check int)
        "every texel is solid" 255
        (Texture.alpha t ~u:5 ~v:5)

(* Alpha survives loading, which is what makes a loaded pattern usable as a
   grille or a window: Material.opaque reads exactly this flag to decide whether
   a wall goes through the renderer's translucent pass. *)
let load_keeps_transparency () =
  match Texture.load "fixtures/holes.png" with
  | Error (`Msg m) -> Alcotest.fail m
  | Ok t ->
      Alcotest.(check bool)
        "a file with holes is not opaque" false (Texture.opaque t);
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
          case "a pattern of no size is refused" a_pattern_of_no_size_is_refused;
        ] );
      ( "noise",
        [
          case "stays in band" noise_stays_in_band;
          case "wraps without a seam"
            (noise_wraps_without_a_seam ~size:Texture.default_size);
          (* And at a size that is not the default, which is the case a wrapping
             lattice left behind at 64 would fail. *)
          case "wraps without a seam at 128"
            (noise_wraps_without_a_seam ~size:128);
          case "seeds are independent" noise_seeds_are_independent;
          case "refuses a lattice that cannot wrap"
            noise_refuses_a_lattice_that_cannot_wrap;
        ] );
      ( "loading",
        [
          case "keeps the colour of the file" load_keeps_the_colour_of_the_file;
          case "keeps transparency" load_keeps_transparency;
          case "refuses a rectangle" load_refuses_a_rectangle;
          case "reports a missing file" load_reports_a_missing_file;
        ] );
    ]
