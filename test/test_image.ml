open Raycaster
open Support

let make_builds_a_square () =
  let img = Image.make 4 (fun ~u ~v -> (Color.rgb u v 0, 255)) in
  Alcotest.(check int) "the size is kept" 4 img.Image.size;
  Alcotest.check color "and pixels are addressable" (Color.rgb 3 2 0)
    (fst (Image.sample img ~u:3 ~v:2))

(* The hot drawing loop reads [pixels] and [alpha] directly rather than through
   [sample], to avoid allocating a tuple per pixel, so the two have to agree
   about where a pixel is. *)
let index_agrees_with_sample () =
  let img = Image.make 8 (fun ~u ~v -> (Color.rgb (u * 8) (v * 8) 0, u + v)) in
  List.iter
    (fun (u, v) ->
      let i = Image.index img ~u ~v in
      let c, a = Image.sample img ~u ~v in
      Alcotest.check color
        (Printf.sprintf "(%d, %d) colour" u v)
        c img.Image.pixels.(i);
      Alcotest.(check int) (Printf.sprintf "(%d, %d) alpha" u v) a
        img.Image.alpha.(i))
    [ (0, 0); (7, 0); (0, 7); (3, 5); (7, 7) ]

(* Unlike a Texture, whose size is a module constant, an image carries its own,
   so decals and sprites of different resolutions can sit in the same world. *)
let images_carry_their_own_size () =
  List.iter
    (fun n ->
      let img = Image.make n (fun ~u:_ ~v:_ -> (Color.rgb 1 2 3, 255)) in
      Alcotest.(check int)
        (Printf.sprintf "a %dx%d image" n n)
        n img.Image.size;
      Alcotest.(check int)
        "and has that many pixels" (n * n)
        (Array.length img.Image.pixels))
    [ 1; 8; 32; 64 ]

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
          case "index agrees with sample" index_agrees_with_sample;
          case "images carry their own size" images_carry_their_own_size;
        ] );
      ( "helpers",
        [
          case "disc is a circle" disc_is_a_circle;
          case "clear is invisible" clear_is_invisible;
        ] );
    ]
