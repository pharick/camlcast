(** What a surface is made of: a colour, and the greyscale {!Texture} that
    modulates it.

    A wall carries its material {e by value}, the way a {!Room.decal} has always
    carried its {!Image}. The alternative — an integer id looked up in a table
    somewhere — was what this replaced, and it had two faults. It put the whole
    look of the game in one module that every level had to share, so a second
    level could not have its own; and an id nobody had defined fell through to a
    default grey instead of failing, so a typo was a slightly wrong wall rather
    than an error.

    Sharing costs nothing: a {!Texture.t} is immutable, so every wall of a room
    referring to the same material refers to the same two arrays. *)

type t = {
  color : Color.t;  (** what the surface is made of *)
  pattern : Texture.t;  (** how it was put together *)
}

let make ~color ~pattern = { color; pattern }

(** Whether the material hides what is behind it. It is the {e pattern} that
    decides — a grille and a window are see-through because their texels carry
    an alpha — so this is the one question the renderer asks to choose between
    painting a wall straight over the column and holding it back for the
    translucent pass. *)
let opaque t = t.pattern.Texture.opaque

(** The texel this material shows at world point [(x, y)], for a floor or
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
