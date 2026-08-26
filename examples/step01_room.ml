(* Step 1 of doc/making-a-game.mld — "One room". The guide quotes only what
   each step adds; this file is the whole game as of this step.

   One square vault you can walk around in. Everything the engine needs is
   here: a material to draw the walls with, a floor and a ceiling, a boundary
   through four corners, and a spawn -- and the three lines that turn the
   editor on, which every step after this one carries. README.md quotes this
   program. *)

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
let flat = Plane.horizontal 0.

(* The editor. It holds what outlives a frame -- which panel is up, what is
   picked -- so it is made once, here, and not inside anything. *)
let studio = Camlcast_edit.Session.create ()

let level =
  P.(
    world ~atmosphere:Atmosphere.default
      [
        room ~height ~material:stone ~floor:(floor ~plane:flat ground)
          ~ceiling:(roof stone)
          ~outline:
            (corners
               [
                 Vec.make (-6.) (-6.);
                 Vec.make 6. (-6.);
                 Vec.make 6. 6.;
                 Vec.make (-6.) 6.;
               ])
          [ spawn (Vec.make (-4.5) 0.) ];
        (* A child of the world, so the mouse is loose under it. *)
        Camlcast_edit.Session.pointer studio;
        (* And the panel in the layer drawn over the finished world. Step 12 is
           where this game puts something of its own in that layer. *)
        hud [ Camlcast_edit.Session.overlay studio () ];
      ])

let () =
  match Camlcast_edit.Session.play ~title:"The Undercroft" studio level with
  | Ok _ending -> ()
  | Error (`Msg message) ->
      prerr_endline message;
      exit 1
