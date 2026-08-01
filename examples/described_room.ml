(* The same room as room.ml, described rather than built.

   Set the two side by side. The materials are the same, the geometry is the
   same, and the picture is the same — test_stage compares them pixel by pixel.
   What differs is everything around them: there is no World here, no Room.make,
   no window to open and close, and no result to match on. A description says
   what the world is, and the loop works out the rest.

   The corners are given in whichever order reads best. outline measures the
   loop and winds it so the room is on the inside, so the classic mistake — a
   boundary wound the wrong way round, and a room black from within — cannot be
   written down. *)

open Camlcast
open Camlcast_stage

let checker ~color ~u ~v =
  Color.level color (if ((u / 16) + (v / 16)) land 1 = 0 then 240 else 170)

let stone =
  Material.make
    ~pattern:(Texture.generate (checker ~color:(Color.rgb 150 150 160)))

let ground =
  Material.make
    ~pattern:(Texture.generate (checker ~color:(Color.rgb 116 110 98)))

let height = 4.
let floor = Plane.horizontal 0.

let world =
  Parts.world ~atmosphere:Atmosphere.default
    ~spawn:("room", Vec.make (-4.5) 0.)
    [
      Parts.room ~name:"room"
        ~floor:(Room.floor ~plane:floor ~material:ground)
        ~ceiling:(Room.roof ~plane:(Plane.above floor height) ~material:stone)
        [
          Parts.outline ~height ~material:stone
            [
              Vec.make (-6.) (-6.);
              Vec.make 6. (-6.);
              Vec.make 6. 6.;
              Vec.make (-6.) 6.;
            ];
        ];
    ]

let () =
  match Run.play ~title:"A described room" world with
  | Ok _ending -> ()
  | Error (`Msg message) ->
      prerr_endline message;
      exit 1
