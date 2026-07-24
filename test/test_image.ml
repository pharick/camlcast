open Raycaster
open Support

let make_builds_a_square () =
  let img = Image.make 4 (fun ~u ~v -> (Color.rgb u v 0, 255)) in
  Alcotest.(check int) "the size is kept" 4 img.Image.size;
  Alcotest.check color "and pixels are addressable" (Color.rgb 3 2 0)
    (fst (Image.sample img ~u:3 ~v:2))

(* A decal is a solid rectangle: every pixel opaque, so it covers the wall
   behind it. *)
let decals_are_opaque () =
  List.iter
    (fun (name, img) ->
      let solid = ref true in
      for v = 0 to img.Image.size - 1 do
        for u = 0 to img.Image.size - 1 do
          if snd (Image.sample img ~u ~v) <> 255 then solid := false
        done
      done;
      Alcotest.(check bool) (name ^ " is fully opaque") true !solid)
    [ ("painting", Image.painting); ("poster", Image.poster) ]

(* A sprite is cut out against a transparent background: its corners are clear,
   its middle is solid, so only the object itself is drawn. *)
let sprites_are_cut_out () =
  List.iter
    (fun (name, img) ->
      let mid = img.Image.size / 2 in
      Alcotest.(check int)
        (name ^ " has a clear corner")
        0
        (snd (Image.sample img ~u:0 ~v:0));
      Alcotest.(check bool)
        (name ^ " has a solid middle")
        true
        (snd (Image.sample img ~u:mid ~v:mid) > 0))
    [ ("barrel", Image.barrel); ("figure", Image.figure) ]

let () =
  Alcotest.run "Image"
    [
      ( "images",
        [
          case "make builds a square" make_builds_a_square;
          case "decals are opaque" decals_are_opaque;
          case "sprites are cut out" sprites_are_cut_out;
        ] );
    ]
