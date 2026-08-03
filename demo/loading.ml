(** {b Art read from files.} The same three things twice over: on the left, a
    pattern, a picture and a figure drawn in a paint program and read off the
    disk; on the right, the generated ones every other demo uses.

    They are the same types and go down the same code paths — a
    {!Camlcast_core.Texture} loaded from a PNG is a [Texture.t] like any other,
    and the renderer has no way to ask where one came from. Generating art in
    code is still fully supported and is still what the rest of this library
    does. This is a second way in, not a replacement.

    Three things are worth walking up to.

    The loaded pattern is 128 texels across and the generated one 64, which a
    pattern may now say for itself. A pattern tiles once per world cell, so that
    is a density and not a resolution: at a cell's distance a 64-texel pattern
    gives each texel about nine screen pixels. Generated noise does not mind —
    it has no detail at that scale to lose — and a drawn surface very much does.

    The two screens standing in front of the walls are see-through because the
    patterns they wear carry an alpha, and the loaded one was not told that: the
    alpha came out of the file, and {!Camlcast_core.Material.opaque} read it and
    sent the wall down the translucent pass on its own.

    The loaded poster is 96 x 64 and hangs in a 2.4 x 1.6 space, undistorted,
    because a decal keeps its two extents apart all the way through —
    {!Camlcast_core.Room.decal_column} indexes the image by its width and
    {!Camlcast_core.Room.decal_row} by its height. The figure beside it keeps
    its shape for the same reason and a sprite's own one:
    {!Camlcast_core.Room.sprite_half_width} takes a billboard's width from its
    picture, so what a sprite is drawn in is the shape the file was authored in.
    {!Floating} is where that is worth looking at, with a cloud of dust three
    times as wide as it is tall.

    Where the files are found is {!Camlcast_core.Asset}'s answer, and it is
    relative to the executable rather than to this source tree. Set
    [CAMLCAST_ASSETS] to make it look somewhere else. *)

open Camlcast
open Camlcast_core.Result_ext

let height = 4.

(* {!Camlcast.Texture.of_asset} and {!Camlcast.Image.of_asset} find the file and
   read it in one step; the names here just say which kind this demo means. *)
let pattern = Texture.of_asset
let picture = Image.of_asset

(** Air clearer than the other demos', with the fog pushed back past the far
    wall. This is a room to look closely at things in, and the usual twelve-cell
    fade would grey out half of what there is to compare. *)
let air =
  Atmosphere.make ~haze:(Color.rgb 24 24 32) ~fog_distance:26.
    ~min_brightness:0.45 ~light:(Vec.make (-0.4) (-0.9)) ~ambient:0.7
    ~directional:0.3 ()

let flat = Plane.horizontal 0.
let sw = Vec.make (-6.) (-4.)
let se = Vec.make 6. (-4.)
let ne = Vec.make 6. 4.
let nw = Vec.make (-6.) 4.

(* The wall you face, in halves, split at the middle. You spawn looking east,
   where the engine's screen-right is +y — so the southern half is on your left
   and the northern one on your right. *)
let mid = Vec.make 6. 0.

let build () =
  let* tiles = pattern "assets/tiles.png" in
  let* grille = pattern "assets/grille.png" in
  let* poster = picture "assets/poster.png" in
  let* figure = picture "assets/figure.png" in
  (* A pattern carries its own colours, so the two halves differ in where their
     colour came from as much as in where their pattern did. The left half is
     terracotta because that is what was painted into the file; the right half
     is pale plaster because that is the argument written three lines below.
     Neither is a decision the engine took part in — a Material has no colour to
     overrule either of them with. *)
  let plaster = Color.rgb 198 190 176 and iron = Color.rgb 105 108 120 in
  let stone = Surfaces.solid (Patterns.stone ~color:(Color.rgb 150 146 140)) in
  let print image =
    P.[ decal ~along:2.5 ~z:1.7 ~half_width:1.2 ~half_height:0.8 image ]
  in
  Ok
    P.(
      world ~atmosphere:air
        ~spawn:("room", Vec.make (-4.) 0.)
        [
          room ~name:"room"
            ~floor:(floor ~plane:flat ~material:Surfaces.ground)
            ~ceiling:
              (roof ~plane:(Plane.above flat height) ~material:Surfaces.soffit)
            [
              wall ~height ~material:stone sw se;
              (* Left: everything on this side came out of a file, colour and
                 all. *)
              wall ~height
                ~material:(Material.make ~pattern:tiles)
                ~decals:(print poster) se mid;
              (* Right: everything on this side is a function of u and v, and its
                 colours are the arguments that function was given. *)
              wall ~height
                ~material:
                  (Surfaces.solid
                     (Patterns.brick ~color:plaster
                        ~mortar:(Color.rgb 176 170 160)))
                ~decals:(print Pictures.painting) mid ne;
              wall ~height ~material:stone ne nw;
              wall ~height ~material:stone nw sw;
              (* A pair of see-through screens, one from each source, each with a
                 wall behind it to reveal. *)
              wall ~height:2.6
                ~material:(Material.make ~pattern:grille)
                (Vec.make 4. (-3.4)) (Vec.make 4. (-0.6));
              wall ~height:2.6
                ~material:(Surfaces.seen_through (Patterns.bars ~color:iron))
                (Vec.make 4. 0.6) (Vec.make 4. 3.4);
              sprite ~key:"read" ~size:1.9 ~image:figure (Vec.make 0.5 (-2.));
              sprite ~key:"made" ~size:1.9 ~image:Pictures.figure
                (Vec.make 0.5 2.);
            ];
        ])

(** The description sits behind a [lazy] for the same reason the world used to:
    this runs the first time something asks for it and not when the program
    starts, which is what keeps a missing file from stopping
    [camlcast-demo --list] from listing the demos, and is why the failure below
    can afford to name the file. *)
let level =
  lazy (Reading.or_raise "the loading demo could not read its art" (build ()))

let world = lazy (Mount.build (Lazy.force level)).Scene.world
let run window = Run.on window (Lazy.force level)
