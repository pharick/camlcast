(** What a surface is made of.

    Today that is one thing — the {!Texture} it wears, which carries its own
    colours — so this is a record of one field, deliberately. A material is
    where a surface's properties go, and the pattern is only the first: how much
    light it reflects, what it sounds like underfoot, whether a mark will take
    on it. Each of those is a property of the {e surface} and not of the picture
    on it, and each would otherwise have to be threaded through {!Room.val-wall}
    one parameter at a time.

    A wall carries its material {e by value}, as a {!Room.type-decal} carries
    its {!Image}. The alternative — an integer id looked up in a table — is what
    this replaced, and it had two faults. It put the whole look of the game in
    one module every level had to share, so a second level could not have its
    own; and an undefined id fell through to a default grey instead of failing,
    so a typo produced a slightly wrong wall rather than an error.

    Sharing costs nothing: a {!Texture.t} is immutable, so every wall of a room
    made of the same material refers to the same arrays. Two surfaces that want
    the same pattern in {e different} colours cannot share, though — the colour
    is in the pattern, so that is two patterns. {!Texture} documents that
    trade-off. *)

type t = private { pattern : Texture.t  (** how the surface is put together *) }
(** Private rather than abstract. [pattern] is read on the drawing path — once
    per wall per column — and by anything asking whether two surfaces share a
    texture, so the read must stay a field access, not a call. Closing the
    record guarantees a material is always made from a real pattern, which is
    what {!opaque} is derived from. The record has one field today and will have
    more; every later field would otherwise have to be written out by every
    caller that built one by hand. *)

val make : pattern:Texture.t -> t
(** A material wearing that pattern. Nothing is derived at construction —
    {!opaque} reads the pattern each time it is asked — so this is as cheap as
    the record it builds, and two materials made from the same {!Texture.t}
    share it. *)

val opaque : t -> bool
(** Whether the material hides what is behind it. The {e pattern} decides: a
    grille and a window are see-through because their texels carry an alpha.
    This is the one question the renderer asks to choose between painting a wall
    straight over the column and holding it back for the translucent pass. *)

val opaque_at : t -> along:float -> above:float -> bool
(** Whether the material hides what is behind it {e at that point} of a wall's
    surface: [along] the wall in cells, from the wall's [a] endpoint, and
    [above] the floor under it. These are the two coordinates a
    {!Room.type-decal} is placed in, and the two {!Renderer} turns into a texel.

    True exactly where that texel is fully solid, which is exactly where the
    renderer writes the pixel outright and records its distance. A texel it only
    {e blends} — a pane of glass, the gap in a grille — is one the eye sees
    through, and so one the crosshair goes through: what can be picked is what
    can be seen, and only a surface drawn over what is behind it hides it.

    {!opaque} is this question over the whole surface, and the two cannot
    disagree: a pattern is {!Texture.opaque} only if every texel is solid, so a
    material that reports opaque answers true here at every point. The cheap
    whole-surface question therefore remains correct for routing a wall in the
    renderer, and this one for a single ray. *)

val plane_texel : t -> x:float -> y:float -> Color.t
(** [plane_texel material ~x ~y] is the colour [material] shows at that world
    point, for a floor or ceiling {!Plane}. The two coordinates are the ground
    position, in cells; the plane's height is not involved.

    The pattern tiles every world unit, so the fractional part of each
    coordinate indexes it. That fraction is always between 0 and 1, even for
    negative coordinates.

    Tiling in world space rather than across the plane is what makes an incline
    visible: features foreshorten and their rows tilt with the surface, which a
    flat colour could not show. *)
