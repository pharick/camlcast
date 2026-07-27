open Raycaster
open Camlcast_demo
open Support

(* A decal is a solid rectangle: every pixel opaque, so it covers the wall
   behind it. *)
let decals_are_opaque () =
  List.iter
    (fun (name, img) ->
      let solid = ref true in
      for v = 0 to img.Image.height - 1 do
        for u = 0 to img.Image.width - 1 do
          if snd (Image.sample img ~u ~v) <> 255 then solid := false
        done
      done;
      Alcotest.(check bool) (name ^ " is fully opaque") true !solid)
    [ ("painting", Pictures.painting); ("poster", Pictures.poster) ]

(* A sprite is cut out against a transparent background: its corners are clear,
   its middle is solid, so only the object itself is drawn. *)
let sprites_are_cut_out () =
  List.iter
    (fun (name, img) ->
      let mu = img.Image.width / 2 and mv = img.Image.height / 2 in
      Alcotest.(check int)
        (name ^ " has a clear corner")
        0
        (snd (Image.sample img ~u:0 ~v:0));
      Alcotest.(check bool)
        (name ^ " has a solid middle")
        true
        (snd (Image.sample img ~u:mu ~v:mv) > 0))
    [ ("barrel", Pictures.barrel); ("figure", Pictures.figure) ]

let () =
  Alcotest.run "Pictures"
    [
      ( "pictures",
        [
          case "decals are opaque" decals_are_opaque;
          case "sprites are cut out" sprites_are_cut_out;
        ] );
    ]
