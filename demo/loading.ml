(** {b Art read from files.} The same three things twice over: on the left, a
    pattern, a picture and a figure drawn in a paint program and read off the
    disk; on the right, the generated ones every other demo uses.

    They are the same types and go down the same code paths — a
    {!Raycaster.Texture} loaded from a PNG is a [Texture.t] like any other, and
    the renderer has no way to ask where one came from. Generating art in code
    is still fully supported and is still what the rest of this library does.
    This is a second way in, not a replacement.

    Three things are worth walking up to.

    The loaded pattern is 128 texels across and the generated one 64, which a
    pattern may now say for itself. A pattern tiles once per world cell, so that
    is a density and not a resolution: at a cell's distance a 64-texel pattern
    gives each texel about nine screen pixels. Generated noise does not mind —
    it has no detail at that scale to lose — and a drawn surface very much does.

    The two screens standing in front of the walls are see-through because the
    patterns they wear carry an alpha, and the loaded one was not told that: the
    alpha came out of the file, and {!Raycaster.Material.opaque} read it and sent
    the wall down the translucent pass on its own.

    The loaded poster is 96 x 64 and hangs in a 2.4 x 1.6 space, undistorted,
    because a decal keeps its two extents apart all the way through —
    {!Raycaster.Room.decal_column} indexes the image by its width and
    {!Raycaster.Room.decal_row} by its height. The figure beside it keeps its
    shape for the same reason and a sprite's own one:
    {!Raycaster.Room.sprite_half_width} takes a billboard's width from its
    picture, so what a sprite is drawn in is the shape the file was authored in.
    {!Floating} is where that is worth looking at, with a cloud of dust three
    times as wide as it is tall.

    Where the files are found is {!Raycaster.Asset}'s answer, and it is relative
    to the executable rather than to this source tree. Set [CAMLCAST_ASSETS] to
    make it look somewhere else. *)

open Raycaster
open Result_ext

let height = 4.

(** Both loaders want a path first, and finding one can fail for its own reasons
    — the file is not there, or is not anywhere this program thought to look —
    so the two failures thread together and whichever happened is the one that
    gets reported. *)
let pattern name =
  let* path = Asset.path name in
  Texture.load path

let picture name =
  let* path = Asset.path name in
  Image.load path

(** Air clearer than the other demos', with the fog pushed back past the far
    wall. This is a room to look closely at things in, and the usual twelve-cell
    fade would grey out half of what there is to compare. *)
let air =
  Atmosphere.make ~haze:(Color.rgb 24 24 32) ~fog_distance:26.
    ~min_brightness:0.45
    ~light:(Vec.make (-0.4) (-0.9))
    ~ambient:0.7 ~directional:0.3

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
  let sw = Vec.make (-6.) (-4.)
  and se = Vec.make 6. (-4.)
  and ne = Vec.make 6. 4.
  and nw = Vec.make (-6.) 4. in
  (* The wall you face, in halves, split at the middle. You spawn looking east,
     where the engine's screen-right is +y — so the southern half is on your
     left and the northern one on your right. *)
  let mid = Vec.make 6. 0. in
  let print image =
    [
      Room.decal ~along:2.5 ~z:1.7 ~half_width:1.2 ~half_height:0.8 image;
    ]
  in
  let screen material a b = Room.wall ~height:2.6 ~material a b in
  let floor = Plane.horizontal 0. in
  let room =
    Room.make
      ~floor:{ Room.plane = floor; material = Surfaces.ground }
      ~ceiling:
        (Room.Roof
           { Room.plane = Plane.above floor height; material = Surfaces.soffit })
      ~sprites:
        [
          Room.sprite ~size:1.9 ~image:figure (Vec.make 0.5 (-2.));
          Room.sprite ~size:1.9 ~image:Pictures.figure (Vec.make 0.5 2.);
        ]
      [
        Room.wall ~height ~material:stone sw se;
        (* Left: everything on this side came out of a file, colour and all. *)
        Room.wall ~height ~material:(Material.make ~pattern:tiles) se mid
          ~decals:(print poster);
        (* Right: everything on this side is a function of u and v, and its
           colours are the arguments that function was given. *)
        Room.wall ~height
          ~material:
            (Surfaces.solid
               (Patterns.brick ~color:plaster ~mortar:(Color.rgb 176 170 160)))
          mid ne ~decals:(print Pictures.painting);
        Room.wall ~height ~material:stone ne nw;
        Room.wall ~height ~material:stone nw sw;
        (* A pair of see-through screens, one from each source, each with a wall
           behind it to reveal. *)
        screen
          (Material.make ~pattern:grille)
          (Vec.make 4. (-3.4))
          (Vec.make 4. (-0.6));
        screen
          (Surfaces.seen_through (Patterns.bars ~color:iron))
          (Vec.make 4. 0.6) (Vec.make 4. 3.4);
      ]
  in
  Ok
    (World.make ~rooms:[ ("room", room) ] ~links:[] ~atmosphere:air
       ~spawn:("room", Vec.make (-4.) 0.))

(** The world sits behind a [lazy] in the catalogue, so this runs the first time
    something asks for it and not when the program starts. That is what keeps a
    missing file from stopping [camlcast-demo --list] from listing the demos,
    and it is why the failure below can afford to name the file. *)
let world =
  lazy
    (match build () with
    | Ok world -> world
    | Error (`Msg m) ->
        failwith ("the loading demo could not read its art: " ^ m))

let run () = Engine.run (Lazy.force world)
