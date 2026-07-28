open Camlcast
open Support

let make_builds_a_square () =
  let img = Image.make 4 (fun ~u ~v -> (Color.rgb u v 0, 255)) in
  Alcotest.(check int) "the width is kept" 4 img.Image.width;
  Alcotest.(check int) "and squared for the height" 4 img.Image.height;
  Alcotest.check color "and pixels are addressable" (Color.rgb 3 2 0)
    (fst (Image.sample img ~u:3 ~v:2))

(* A poster is wider than it is tall and a standing figure is taller than it is
   wide, so an image is a rectangle. The two extents have to stay apart all the
   way through: a picture that came back square would be a mirror of itself
   along the diagonal, which is a wrong picture and not an error. *)
let make_builds_a_rectangle () =
  let img = Image.make ~height:2 5 (fun ~u ~v -> (Color.rgb u v 0, 255)) in
  Alcotest.(check int) "the width" 5 img.Image.width;
  Alcotest.(check int) "the height" 2 img.Image.height;
  Alcotest.(check int) "and that many pixels" 10 (Array.length img.Image.pixels);
  Alcotest.check color "the far corner is where it should be" (Color.rgb 4 1 0)
    (fst (Image.sample img ~u:4 ~v:1));
  (* Row major: one step down is a whole width along. *)
  Alcotest.(check int)
    "one row down is one width along" 5
    (Image.index img ~u:0 ~v:1)

(* The hot drawing loop reads [pixels] and [alpha] directly rather than through
   [sample], to avoid allocating a tuple per pixel, so the two have to agree
   about where a pixel is. *)
let index_agrees_with_sample () =
  (* Non-square on purpose: a square image agrees with itself under a width and
     height swapped over, and that is the mistake this test is here to catch. *)
  let img =
    Image.make ~height:5 8 (fun ~u ~v -> (Color.rgb (u * 8) (v * 8) 0, u + v))
  in
  List.iter
    (fun (u, v) ->
      let i = Image.index img ~u ~v in
      let c, a = Image.sample img ~u ~v in
      Alcotest.check color
        (Printf.sprintf "(%d, %d) colour" u v)
        c img.Image.pixels.(i);
      Alcotest.(check int)
        (Printf.sprintf "(%d, %d) alpha" u v)
        a img.Image.alpha.(i))
    [ (0, 0); (7, 0); (0, 4); (3, 3); (7, 4) ]

let images_carry_their_own_size () =
  List.iter
    (fun (w, h) ->
      let img =
        Image.make ~height:h w (fun ~u:_ ~v:_ -> (Color.rgb 1 2 3, 255))
      in
      Alcotest.(check int)
        (Printf.sprintf "a %dx%d image's width" w h)
        w img.Image.width;
      Alcotest.(check int) "its height" h img.Image.height;
      Alcotest.(check int)
        "and has that many pixels" (w * h)
        (Array.length img.Image.pixels))
    [ (1, 1); (8, 8); (32, 4); (3, 64) ]

(* Read from a file this repository keeps and tools/make_art.py can rebuild.
   Its pixel values are literals in both places, and the encoder that wrote them
   shares no code with the decoder reading them here, so agreeing means the
   channel order and the row offsets are both right rather than both wrong. *)
let load_reads_a_file () =
  match Image.load "fixtures/swatch.png" with
  | Error (`Msg m) -> Alcotest.fail m
  | Ok img ->
      Alcotest.(check int) "the width" 4 img.Image.width;
      Alcotest.(check int) "the height" 3 img.Image.height;
      List.iter
        (fun (u, v, r, g, b, a) ->
          let c, alpha = Image.sample img ~u ~v in
          Alcotest.check color
            (Printf.sprintf "(%d, %d)" u v)
            (Color.rgb r g b) c;
          Alcotest.(check int) (Printf.sprintf "(%d, %d) alpha" u v) a alpha)
        [
          (0, 0, 10, 100, 200, 245);
          (3, 0, 70, 100, 170, 155);
          (0, 2, 10, 180, 190, 185);
          (3, 2, 70, 180, 160, 95);
          (1, 1, 30, 140, 185, 185);
        ]

(* A JPEG has no alpha channel, and arrives solid rather than clear — which is
   the only sane reading of a photograph, and the one that stops a loaded sprite
   from being invisible. Its colours are lossy, so only the shape and the alpha
   are worth asserting. *)
let a_file_without_alpha_is_solid () =
  match Image.load "fixtures/swatch.jpg" with
  | Error (`Msg m) -> Alcotest.fail m
  | Ok img ->
      Alcotest.(check int) "the width survives" 4 img.Image.width;
      Alcotest.(check int) "the height survives" 3 img.Image.height;
      Array.iteri
        (fun i a ->
          Alcotest.(check int) (Printf.sprintf "pixel %d is solid" i) 255 a)
        img.Image.alpha

let disc_is_a_circle () =
  Alcotest.(check bool)
    "the centre is inside" true
    (Image.disc ~cx:8. ~cy:8. ~r:4. 8 8);
  Alcotest.(check bool)
    "just inside the rim" true
    (Image.disc ~cx:8. ~cy:8. ~r:4. 11 8);
  Alcotest.(check bool)
    "just outside it" false
    (Image.disc ~cx:8. ~cy:8. ~r:4. 12 8);
  Alcotest.(check bool)
    "and the corner of its box is outside" false
    (Image.disc ~cx:8. ~cy:8. ~r:4. 11 11)

let clear_is_invisible () =
  Alcotest.(check int) "nothing shows through it" 0 (snd Image.clear)

let () =
  Alcotest.run "Image"
    [
      ( "images",
        [
          case "make builds a square" make_builds_a_square;
          case "make builds a rectangle" make_builds_a_rectangle;
          case "index agrees with sample" index_agrees_with_sample;
          case "images carry their own size" images_carry_their_own_size;
        ] );
      ( "loading",
        [
          case "load reads a file" load_reads_a_file;
          case "a file without alpha is solid" a_file_without_alpha_is_solid;
        ] );
      ( "helpers",
        [
          case "disc is a circle" disc_is_a_circle;
          case "clear is invisible" clear_is_invisible;
        ] );
    ]
