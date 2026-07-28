(** The showcase level's materials, sky and air.

    These were once a table indexed by an integer id, and reading them as names
    is the whole argument for {!Camlcast.Material}: [~material:Surfaces.brick]
    says what the wall is, where [~texture:1] said only where to look it up.

    A pattern carries its own colours, so a material here is a {!Patterns}
    function with its colours filled in and the rest generated. The same
    function serves several materials that way — {!Patterns.checker} is both the
    yellow {!tile} and the grey-brown {!ground}, and {!Patterns.panel} is both
    the green {!panel} and the slate {!soffit} — which is the reuse a
    brightness-only texture used to get for nothing, written down. What it buys
    in exchange is {!brick}, whose mortar is a colour of its own rather than a
    paler red. *)

open Camlcast

(** A material from a pattern function with its colours already given. The two
    differ only in which generator they reach for, and so in whether the result
    can be seen through. *)
let solid f = Material.make ~pattern:(Texture.generate f)

let seen_through f = Material.make ~pattern:(Texture.generate_masked f)

let brick =
  solid
    (Patterns.brick ~color:(Color.rgb 200 70 70) ~mortar:(Color.rgb 188 182 172))

let panel = solid (Patterns.panel ~color:(Color.rgb 80 190 100))
let stone = solid (Patterns.stone ~color:(Color.rgb 80 120 220))
let tile = solid (Patterns.checker ~color:(Color.rgb 220 200 90))

(** A steel grille and a leaded window: see-through, because the patterns they
    wear carry an alpha. *)
let grille = seen_through (Patterns.bars ~color:(Color.rgb 105 108 120))

let window =
  seen_through
    (Patterns.glass ~lead:(Color.rgb 78 82 92) ~pane:(Color.rgb 150 205 230))

let oak = solid (Patterns.door ~color:(Color.rgb 130 82 45))
let ground = solid (Patterns.checker ~color:(Color.rgb 116 110 98))
let soffit = solid (Patterns.panel ~color:(Color.rgb 92 92 112))

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
    directional light that walls at different angles read as different surfaces.
*)
let air =
  Atmosphere.make ~haze:(Color.rgb 24 24 32) ~fog_distance:12.
    ~min_brightness:0.25 ~light:(Vec.make (-0.4) (-0.9)) ~ambient:0.6
    ~directional:0.4
