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

type t = { pattern : Texture.t  (** how the surface is put together *) }

let make ~pattern = { pattern }

(** Whether the material hides what is behind it. It is the {e pattern} that
    decides — a grille and a window are see-through because their texels carry
    an alpha — so this is the one question the renderer asks to choose between
    painting a wall straight over the column and holding it back for the
    translucent pass. *)
let opaque t = t.pattern.Texture.opaque

(** The colour this material shows at world point [(x, y)], for a floor or
    ceiling {!Plane}. The pattern tiles every world unit, so the fractional part
    of each coordinate indexes it — and that fraction is always between 0 and 1,
    even for negative coordinates.

    Tiling in world space rather than across the plane is what makes an incline
    visible: the features foreshorten and their rows tilt with the surface,
    which a flat colour could never show. *)
let plane_texel t ~x ~y =
  let frac v = v -. Float.floor v in
  Texture.sample t.pattern
    ~u:(Texture.column_of_offset t.pattern (frac x))
    ~v:(Texture.column_of_offset t.pattern (frac y))
