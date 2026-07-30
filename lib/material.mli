(** What a surface is made of.

    Today that is one thing — the {!Texture} it wears, which carries its own
    colours — so this is a record of one field, and deliberately so. A material
    is the place a surface's properties go, and the pattern is only the first of
    them: how much light it throws back, what it sounds like underfoot, whether
    a mark will take on it. Each of those is a property of the {e surface} and
    not of the picture on it, and each would otherwise have to be threaded
    through {!Room.val-wall} one parameter at a time.

    A wall carries its material {e by value}, the way a {!Room.type-decal} has
    always carried its {!Image}. The alternative — an integer id looked up in a
    table somewhere — was what this replaced, and it had two faults. It put the
    whole look of the game in one module that every level had to share, so a
    second level could not have its own; and an id nobody had defined fell
    through to a default grey instead of failing, so a typo was a slightly wrong
    wall rather than an error.

    Sharing costs nothing: a {!Texture.t} is immutable, so every wall of a room
    made of the same material refers to the same arrays. Two surfaces that want
    the same pattern in {e different} colours no longer can share, though —
    since the colour is in the pattern, that is two patterns. {!Texture} is
    where that trade is written down. *)

type t = private { pattern : Texture.t  (** how the surface is put together *) }
(** Private rather than abstract. [pattern] is read on the drawing path — once
    per wall per column — and by anything asking whether two surfaces share a
    texture, so the read has to stay a field access and not a call. What closing
    the record buys is that a material is always made from a real pattern, which
    is what {!opaque} is derived from: the record has one field today and will
    have more, and every one of them added later would otherwise have to be
    written out by every caller that ever built one by hand. *)

val make : pattern:Texture.t -> t
(** A material wearing that pattern. Nothing is derived at the call — {!opaque}
    reads the pattern each time it is asked — so this is as cheap as the record
    it builds, and two materials made from the same {!Texture.t} share it. *)

val opaque : t -> bool
(** Whether the material hides what is behind it. It is the {e pattern} that
    decides — a grille and a window are see-through because their texels carry
    an alpha — so this is the one question the renderer asks to choose between
    painting a wall straight over the column and holding it back for the
    translucent pass. *)

val opaque_at : t -> along:float -> above:float -> bool
(** Whether the material hides what is behind it {e at that point} of a wall's
    surface: [along] it in cells, from the wall's [a] endpoint, and [above] the
    floor standing under it. The two coordinates a {!Room.type-decal} is placed
    in, and the two {!Renderer} turns into a texel.

    True exactly where that texel is fully solid, which is exactly where the
    renderer writes the pixel outright and records its distance. A texel it only
    {e blends} — a pane of glass, the gap in a grille — is one the eye goes
    through, and so one the crosshair goes through: what can be picked is what
    can be seen, and a surface drawn over what is behind it is the only thing
    that hides it.

    {!opaque} is this everywhere at once, and the two cannot disagree: a pattern
    is {!Texture.opaque} only if every texel of it is solid, so a material that
    says it is opaque answers true here at every point of it. Which is why the
    cheap whole-surface question is still the right one for the renderer to
    route a wall by, and this the right one to ask about a single ray. *)

val plane_texel : t -> x:float -> y:float -> Color.t
(** [plane_texel material ~x ~y] is the colour [material] shows at that world
    point, for a floor or ceiling {!Plane}. The two coordinates are the ground
    position, in cells; the plane's height does not come into it.

    The pattern tiles every world unit, so the fractional part of each
    coordinate indexes it — and that fraction is always between 0 and 1, even
    for negative coordinates.

    Tiling in world space rather than across the plane is what makes an incline
    visible: the features foreshorten and their rows tilt with the surface,
    which a flat colour could never show. *)
