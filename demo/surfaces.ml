(** The showcase level's materials, sky and air.

    These were once a table indexed by an integer id, and reading them as names
    is the whole argument for {!Raycaster.Material}: [~material:Surfaces.brick]
    says what the wall is, where [~texture:1] said only where to look it up.
    Colour and pattern are still chosen independently — the colour says what a
    surface is made of, the pattern says how it was put together — so the same
    masonry can be red brick or grey stone. *)

open Raycaster

let brick = Material.make ~color:(Color.rgb 200 70 70) ~pattern:Patterns.brick
let panel = Material.make ~color:(Color.rgb 80 190 100) ~pattern:Patterns.panel
let stone = Material.make ~color:(Color.rgb 80 120 220) ~pattern:Patterns.stone

let tile =
  Material.make ~color:(Color.rgb 220 200 90) ~pattern:Patterns.checker

(** A steel grille and a leaded window: see-through, because the patterns they
    wear carry an alpha. *)
let grille = Material.make ~color:(Color.rgb 105 108 120) ~pattern:Patterns.bars

let window = Material.make ~color:(Color.rgb 150 205 230) ~pattern:Patterns.glass
let oak = Material.make ~color:(Color.rgb 130 82 45) ~pattern:Patterns.door

let ground =
  Material.make ~color:(Color.rgb 116 110 98) ~pattern:Patterns.checker

let soffit =
  Material.make ~color:(Color.rgb 92 92 112) ~pattern:Patterns.panel

(** A clear afternoon: a pale horizon deepening to a blue zenith, with the sun
    low in the west. *)
let day =
  {
    Sky.horizon = Color.rgb 176 196 222;
    zenith = Color.rgb 40 62 126;
    sun = Color.rgb 255 246 216;
    sun_azimuth = -0.9;
    sun_height = 0.5;
    sun_radius = 0.55;
    gradient = 2.2;
  }

(** Daylight air: a long, gentle fade to a blue-grey haze, and enough
    directional light that walls at different angles read as different
    surfaces. *)
let air =
  Atmosphere.make ~haze:(Color.rgb 24 24 32) ~fog_distance:12.
    ~min_brightness:0.25
    ~light:(Vec.make (-0.4) (-0.9))
    ~ambient:0.6 ~directional:0.4
