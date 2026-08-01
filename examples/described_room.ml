(* The same room as room.ml, described rather than built.

   Set the two side by side. The materials are the same, the geometry is the
   same, and the picture is the same — test_stage compares them pixel by pixel.
   What differs is everything around them: one library is opened rather than
   one library and a World, there is no Room.make, no window to open and close,
   and nothing to match on beyond saying what to do if SDL will not start.

   P is written round the description rather than opened over the file. Its
   names are short and ordinary — wall, room, text, path — so a local open puts
   them exactly where a world is being written and nowhere else, and marks where
   that starts and stops.

   The corners are given in whichever order reads best. outline measures the
   loop and winds it so the room is on the inside, so the classic mistake — a
   boundary wound the wrong way round, and a room black from within — cannot be
   written down. *)

open Camlcast

let checker ~color ~u ~v =
  Color.level color (if ((u / 16) + (v / 16)) land 1 = 0 then 240 else 170)

let stone =
  Material.make
    ~pattern:(Texture.generate (checker ~color:(Color.rgb 150 150 160)))

let ground =
  Material.make
    ~pattern:(Texture.generate (checker ~color:(Color.rgb 116 110 98)))

let height = 4.
let ground_plane = Plane.horizontal 0.

let level =
  P.(
    world ~atmosphere:Atmosphere.default
      ~spawn:("room", Vec.make (-4.5) 0.)
      [
        room ~name:"room"
          ~floor:(floor ~plane:ground_plane ~material:ground)
          ~ceiling:
            (roof ~plane:(Plane.above ground_plane height) ~material:stone)
          [
            outline ~height ~material:stone
              [
                Vec.make (-6.) (-6.);
                Vec.make 6. (-6.);
                Vec.make 6. 6.;
                Vec.make (-6.) 6.;
              ];
          ];
      ])

let () =
  match Run.play ~title:"A described room" level with
  | Ok _ending -> ()
  | Error (`Msg message) ->
      prerr_endline message;
      exit 1
