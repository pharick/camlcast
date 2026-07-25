(** The House's materials and its air.

    Ash grey is not one colour. The walls are the lightest thing in the House
    and still nowhere near white; the floor is a shade darker and a shade
    cooler, because a floor that matched the walls would make a corridor read as
    a tube; and the ceiling is darker than either, since nothing lights it. The
    three are close enough to be the same substance and far enough apart that
    you can tell which way is up. Very slightly warm across all three — a
    perfectly neutral grey reads as digital, and this is meant to read as a
    material nobody can name. *)

open Raycaster

let wall = Material.make ~color:(Color.rgb 172 168 161) ~pattern:Ash.plaster

(** For the long walls of a corridor, where the faint striation in
    {!Ash.scored} is what keeps the wall receding once the fog has taken the
    rest of it. *)
let corridor = Material.make ~color:(Color.rgb 170 166 159) ~pattern:Ash.scored

let floor = Material.make ~color:(Color.rgb 132 129 125) ~pattern:Ash.ground
let ceiling = Material.make ~color:(Color.rgb 104 102 100) ~pattern:Ash.soffit

(** The one made thing. Barely darker than the wall it is set in — a door you
    can see from across a room would be a landmark, and the House does not offer
    landmarks. *)
let door = Material.make ~color:(Color.rgb 150 145 138) ~pattern:Ash.leaf

(** The air of the House, and most of what makes it the House.

    Three numbers do the work. The fog closes in at nine cells rather than
    twelve, so the far end of a corridor is gone before you have walked it. It
    fades to [0.06] rather than [0.25], so what it fades to is effectively
    black — the book's hallway does not recede into grey, it recedes into
    nothing. And the shading band is [0.85 .. 1.0] instead of [0.6 .. 1.0], so a
    wall is very nearly the same brightness whichever way it faces: there are no
    shadows and no discernible source, and distance is the only thing left that
    tells one surface from another. Which is to say the only thing you can
    navigate by is how far away something is, in a place built to make that
    useless.

    [haze] is nearly black rather than the blue-grey of a daylit world, because
    it fills a doorway the renderer could not see far enough through. In a
    daylit level that reads as distance. Here it reads as a door standing open
    onto nothing, and there is no need to correct the impression. *)
let air =
  Atmosphere.make ~haze:(Color.rgb 8 8 9) ~fog_distance:9. ~min_brightness:0.06
    ~light:(Vec.make (-0.4) (-0.9))
    ~ambient:0.85 ~directional:0.15
