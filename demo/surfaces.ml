(** The showcase level's materials, sky and air.

    These were once a table indexed by an integer id, and reading them as names
    is the whole argument for {!Camlcast_core.Material}:
    [~material:Surfaces.brick] says what the wall is, where [~texture:1] said
    only where to look it up.

    A pattern carries its own colours, so a material here is a {!Patterns}
    function with its colours filled in and the rest generated. The same
    function serves several materials that way — {!Patterns.checker} is both the
    yellow {!tile} and the grey-brown {!ground}, and {!Patterns.panel} is both
    the green {!panel} and the slate {!soffit} — which is the reuse a
    brightness-only texture used to get for nothing, written down. What it buys
    in exchange is {!brick}, whose mortar is a colour of its own rather than a
    paler red. *)

open Camlcast_core

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
    low in the west — which is {!Camlcast_core.Sky.default}, named for what it
    is here. *)
let day = Sky.default

(** The other end of the same afternoon: the sun gone round to the east and
    almost down, so its disc sits near the horizon and the warmth is all in the
    bottom of the sky.

    A second sky value is the whole of what "two rooms under different ones"
    takes. Nothing else changes — {!air} is one per world and lights both —
    which is the split worth seeing: the sky is what a room is roofed with, and
    the air is what the world is seen through. Walk from the plaza to the garden
    and only the first of those changes.

    The gradient is {e shallower} than {!day}'s, which is the opposite of what
    the picture suggests and the reason worth writing down: [gradient] is how
    fast the horizon colour gives way to the zenith one, so a large number keeps
    the warmth in a band at eye level. The garden is walled seven cells high and
    you meet its sky by looking up over them — a steep gradient would put every
    part of it you can actually see at the zenith colour, and a dusk that reads
    as dusk only where the walls hide it is no dusk at all. Hence 1.1, and a
    zenith that is violet where {!day}'s is blue: the two have to tell apart in
    a glance at the top of the frame, which is all a room this enclosed gives
    you. *)
let dusk =
  Sky.make ~horizon:(Color.rgb 240 156 96) ~zenith:(Color.rgb 74 48 108)
    ~sun:(Color.rgb 255 226 170) ~sun_azimuth:2.1 ~sun_height:0.34
    ~sun_radius:0.5 ~gradient:1.1 ()

(** Daylight air: a long, gentle fade to a blue-grey haze, and enough
    directional light that walls at different angles read as different surfaces.
    {!Camlcast_core.Atmosphere.default} is exactly that air, so the name here
    only says what the demos mean by it. *)
let air = Atmosphere.default
