(** The showcase level's materials, sky and air.

    These were once a table indexed by an integer id; named values are the
    argument for {!Camlcast_core.Material}: [~material:Surfaces.brick] says what
    the wall is, where [~texture:1] said only where to look it up.

    A pattern carries its own colours, so a material here is a {!Patterns}
    function with its colours filled in and the texture generated. The same
    function serves several materials — {!Patterns.checker} is both the yellow
    {!tile} and the grey-brown {!ground}, and {!Patterns.panel} is both the
    green {!panel} and the slate {!soffit} — the reuse a brightness-only texture
    got for free, written down. In exchange it gains {!brick}, whose mortar is a
    colour of its own rather than a paler red. *)

open Camlcast_core

(** A material from a pattern function with its colours already given. The two
    differ only in the generator used, and so in whether the result is
    see-through. *)
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

(** A clear afternoon: a pale horizon deepening to a blue zenith, the sun low in
    the west. This is {!Camlcast_core.Sky.default}, named for what it is here.
*)
let day = Sky.default

(** Dusk: the same afternoon later, the sun gone round to the east and almost
    down, so its disc sits near the horizon and the warmth is at the bottom of
    the sky.

    A second sky value is all that putting two rooms under different skies
    takes. Nothing else changes — {!air} is one per world and lights both. The
    sky is what a room is roofed with; the air is what the world is seen
    through. Walking from the plaza to the garden changes only the first.

    The gradient is {e shallower} than {!day}'s, the opposite of what the
    picture suggests: [gradient] is how fast the horizon colour gives way to the
    zenith one, so a large number keeps the warmth in a band at eye level. The
    garden is walled seven cells high and its sky is only met by looking up over
    the walls — a steep gradient would put every visible part of it at the
    zenith colour, and the dusk would only read as dusk where the walls hide it.
    Hence 1.1, and a zenith that is violet where {!day}'s is blue: the two skies
    must tell apart at the top of the frame, which is all a room this enclosed
    shows. *)
let dusk =
  Sky.make ~horizon:(Color.rgb 240 156 96) ~zenith:(Color.rgb 74 48 108)
    ~sun:(Color.rgb 255 226 170) ~sun_azimuth:2.1 ~sun_height:0.34
    ~sun_radius:0.5 ~gradient:1.1 ()

(** Daylight air: a long, gentle fade to a blue-grey haze, with enough
    directional light that walls at different angles read as different surfaces.
    {!Camlcast_core.Atmosphere.default} is exactly this air; the name states
    what the demos mean by it. *)
let air = Atmosphere.default
