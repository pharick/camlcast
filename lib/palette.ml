(** Where the colour choices live: the texture id on a {!Room.type-wall}
    indexes the wall tables here, and everything that dims a surface —
    orientation shading, distance fog — is here too. *)

let wall_color = function
  | 1 -> Color.rgb 200 70 70
  | 2 -> Color.rgb 80 190 100
  | 3 -> Color.rgb 80 120 220
  | 4 -> Color.rgb 220 200 90
  | 5 -> Color.rgb 105 108 120 (* steel grille *)
  | 6 -> Color.rgb 150 205 230 (* glass *)
  | 7 -> Color.rgb 130 82 45 (* wooden door *)
  | _ -> Color.rgb 160 160 160

(** What each kind of wall is built from. Colour and pattern are chosen
    independently: {!wall_color} says what the wall is made of, this says how it
    was put together. Ids 5 and 6 are see-through — a grille and a window — the
    rest are solid. *)
let pattern = function
  | 1 -> Texture.brick
  | 2 -> Texture.panel
  | 3 -> Texture.stone
  | 4 -> Texture.checker
  | 5 -> Texture.bars
  | 6 -> Texture.glass
  | 7 -> Texture.door
  | _ -> Texture.plain

(** A fixed direction the light comes from, in the flat world. *)
let light = Vec.normalize (Vec.make (-0.4) (-0.9))

(** How brightly a wall of a given orientation catches the light. With walls at
    every angle a fixed east/west versus north/south rule no longer works, so
    the brightness follows how squarely the wall's [normal] faces the light,
    kept within a band so no wall falls to black. *)
let face_shading (normal : Vec.t) =
  0.6 +. (0.4 *. Float.abs (Vec.dot normal light))

(** Linear distance fog: full colour up close, {!Config.min_brightness} at and
    beyond {!Config.fog_distance}. On the floor and ceiling it doubles as a
    horizon haze, fading the inclined planes out into the distance instead of
    letting them run to a hard edge. *)
let fog distance =
  Float.max Config.min_brightness (1. -. (distance /. Config.fog_distance))

(** The colour a wall is tinted before its greyscale {!pattern} modulates it:
    the wall colour, dimmed by orientation and distance. A texel at full
    brightness comes out this colour, darker texels proportionally darker. *)
let shaded_wall (wall : Room.wall) ~distance =
  Color.shade (wall_color wall.texture)
    (face_shading wall.normal *. fog distance)

(* Floor and ceiling are textured like the walls: a greyscale {!Texture}
   sampled in world space and tinted by a base colour. Because the pattern
   repeats every world unit, its features foreshorten and their rows tilt with
   the surface, which is what makes the incline of the planes plain to see —
   more so than the flat colour a plane would otherwise show. *)

let floor_color = Color.rgb 116 110 98
let ceiling_color = Color.rgb 92 92 112
let floor_pattern = Texture.checker
let ceiling_pattern = Texture.panel

(** The far haze the planes fade into where the eye looks past both of them. *)
let haze = Color.rgb 24 24 32

(** The texel a plane's pattern shows at world point [(x, y)]. The pattern tiles
    every world unit, so the fractional part of each coordinate indexes it — and
    that fraction is always between 0 and 1, even for negative coordinates. *)
let plane_texel pattern ~x ~y =
  let frac v = v -. Float.floor v in
  Texture.sample pattern
    ~u:(Texture.column_of_offset (frac x))
    ~v:(Texture.column_of_offset (frac y))
