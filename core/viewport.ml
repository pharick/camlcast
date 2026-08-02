(** How the window, whatever size it currently is, maps onto the camera.

    The window is resizable and can go fullscreen between any two frames, so the
    projection cannot be baked into constants: each frame asks SDL for the size
    of the drawing surface and builds one of these from it.

    Two rules decide what a resize does.

    {1 Pixels stay square}

    A wall one cell wide and one cell tall must cover the same number of pixels
    in both directions, or the world looks stretched.

    - Vertically, a one-cell length at perpendicular distance [d] covers
      [projection / d] pixels — that is what [projection] means.
    - Horizontally, the camera plane spans [2 * half_width] world units at
      distance 1 and is drawn across [width] pixels, so one world unit at
      distance [d] covers [width / (2 * half_width * d)] pixels.

    Setting the two equal gives [half_width = width / (2 * projection)], which
    is exactly how [half_width] is derived below. The [d] cancels, so the rule
    holds at every distance.

    {1 Where a pixel is}

    A pixel is its centre. Column [c] covers everything from [c] up to but not
    including [c + 1], and is sampled at [c + 0.5]; row [r] likewise.

    Three kinds of function here, and the rule is what joins them.
    {!ray_direction} and {!row_factor} take a pixel {e index} and answer for
    that pixel's centre. {!project_height}, {!project_point} and {!sprite_box}
    answer in the {e continuous} coordinates the same numbers live on, and are
    deliberately not shifted, so that the two are inverse — {!project_point} of
    {!ray_direction} at column [c] is [c + 0.5], the centre of that very column.
    {!first_pixel} and {!last_pixel} come back the other way, and are how
    {!Renderer} turns an extent into rows and columns.

    Those two are not the same rounding, and that is the part worth stating
    plainly, because getting it wrong costs a row rather than a rounding error.
    An extent is half-open: a wall runs from its top down to its foot, where the
    floor takes over. So the pixels it covers run from {!first_pixel} of where
    it starts to {!last_pixel} of where it stops, and those two are one apart.
    Round both ends alike and the extent claims a pixel whose centre falls past
    its far edge — a row of wall drawn over the floor and sampled below the
    wall's own foot, which is somewhere the wall never was.

    Say it the other way, that a pixel is its top-left corner, and half of this
    module means one thing and half the other: the rays would sample the left
    edges of their columns while the rasteriser covered their centres, and every
    sprite would stand half a pixel from the wall behind it, at every window
    size. The crosshair {!Paint} draws is [width / 2], the pixel containing the
    middle of the buffer, which is the nearest-centre pixel under this rule —
    exactly the straight-ahead ray at an odd width, and one of two equally near
    at an even one, where the middle falls on a boundary and no pixel is the
    centre.

    {!Sight} traces the middle itself rather than that pixel, on purpose: its
    answer is the same whatever size the window has been dragged to. At an even
    size the two are half a pixel apart, and what that half is worth has grown.
    Now that picking reads the {e texel} under the crosshair rather than judging
    a whole material, a ray landing half a pixel to one side can land on the
    next texel along, so the two can differ by one texel of the pattern at the
    edge of a grille's bar — where before it took a target narrower than a pixel
    to notice.

    Bounded on the screen, at half a pixel; not bounded in the world, because
    half a pixel subtends more of a surface the further off it is. At the
    512x384 an unresized window renders into, the two rays part by 0.0011 cells
    for every cell of distance — for a 64-texel pattern, one texel at about
    fourteen cells and two at twenty-eight. What caps it in practice is the haze
    rather than the arithmetic: an atmosphere with a [fog_distance] in the low
    tens has taken the pattern away before the disagreement reaches a second
    texel. Still only at even sizes, and still the price of a crosshair that
    means the same thing in every window.

    {1 Widening the window reveals more world}

    That leaves [projection] free. Anchoring it to the window {e height} fixes
    the vertical field of view, so dragging the window wider shows more to the
    left and right instead of magnifying what was already there — what a first
    person camera is expected to do. (Anchoring it to the width instead would do
    the opposite: a wider window would zoom in and crop the view vertically.)

    {!Config.fov} is therefore read as the horizontal field of view at
    {!Config.reference_aspect}; the vertical angle that implies is what is
    actually held fixed.

    {1 Height, eye and pitch}

    Walls no longer all stand on a flat floor at a fixed height, so the
    projection is written in terms of a world height [z]: {!project_height}
    turns any elevation, at any distance, into a screen row. It measures
    everything from the eye — [eye_z], the elevation the player's eye sits at
    this frame — and from the [horizon], the row the eye looks along. Pitch
    slides the horizon away from the middle of the window (a vertical shear,
    since a raycaster has no true vertical rotation), capped by
    {!Config.max_pitch}. *)

type t = {
  width : int;
  height : int;
  half_width : float;
      (** half the camera plane, in world units at distance 1 *)
  projection : float;  (** pixels covered by one world unit at distance 1 *)
  eye_z : float;  (** the elevation the eye sits at this frame *)
  horizon : float;  (** the screen row the eye looks straight along *)
}

(** Tangent of half the vertical field of view — the constant of the whole
    module, by the second rule above. *)
let vertical_half_extent =
  Float.tan (Config.fov /. 2.) /. Config.reference_aspect

let make ~pitch ~eye_z ~width ~height =
  (* A minimised window reports a zero size; clamp so the maths stays finite
     and the frame is merely pointless rather than full of NaNs. *)
  let width = Int.max 1 width and height = Int.max 1 height in
  let projection = float_of_int height /. 2. /. vertical_half_extent in
  {
    width;
    height;
    projection;
    eye_z;
    half_width = float_of_int width /. 2. /. projection;
    (* Level, the horizon is the middle row; [pitch] shears it away from there
       by that fraction of the window height. Looking up (positive pitch) slides
       it down and reveals more ceiling. *)
    horizon = (float_of_int height /. 2.) +. (pitch *. float_of_int height);
  }

(** Direction of the ray through screen column [column] — through its centre, by
    the rule above. [camera_x] picks the point of the camera plane that column
    looks through: -1 at the left edge of the window and +1 at the right, so the
    first column reads [-1 + 1/width] and the last [1 - 1/width], half a column
    in from each edge. One multiply and one add, no trigonometry per column. *)
let ray_direction t (player : Player.t) ~column =
  let camera_x =
    (2. *. (float_of_int column +. 0.5) /. float_of_int t.width) -. 1.
  in
  Vec.add player.dir (Vec.scale player.right (t.half_width *. camera_x))

(** The screen row a point of world height [z] projects to, at perpendicular
    distance [distance]. Similar triangles: its height above the eye,
    [z - eye_z], shrinks with distance and is measured down from the horizon. A
    wall's foot is [project_height] at the floor's height there, its top at the
    floor plus the wall's height, and the strip between them is the wall.

    A continuous row and not a pixel index: it is [Float.round] of this that
    names the pixel, by the rule above. *)
let project_height t ~z ~distance =
  t.horizon -. (t.projection *. (z -. t.eye_z) /. distance)

(** The first pixel an extent starting at the continuous coordinate [x] covers:
    the first whose centre lies at or past it. A centre sits half a pixel in, so
    that is [Float.round] — bar an [x] exactly on one, where it names the pixel
    after and the extent gives up a boundary it would only have won on an
    equality between two projected floats. *)
let first_pixel x = int_of_float (Float.round x)

(** The last pixel an extent stopping at the continuous coordinate [x] covers,
    which is {!first_pixel} of it less one and not that pixel itself. An extent
    is half-open — [x] is where the next thing begins — so the pixel whose
    centre [x] falls inside is the next thing's. *)
let last_pixel x = first_pixel x - 1

(** The dimensionless [(row + ½ - horizon) / projection] the centre of a screen
    row sits at, the quantity {!Plane.cast} needs to cast the floor and ceiling.
    The half is the same one {!ray_direction} adds, in the other direction. *)
let row_factor t ~row = (float_of_int row +. 0.5 -. t.horizon) /. t.projection

(** Where a point of the world lands on the screen: [(column, row)] in pixels,
    or [None] if it is level with the eye or behind it and has no place on the
    screen at all.

    [point] is where it stands on the floor plan and [z] how high it is, both in
    the frame [pose] is expressed in. The column is the inverse of
    {!ray_direction}: a point [lateral] to one side at perpendicular distance
    [d] sits that fraction across the camera plane.

    A vertical line in the world projects to a vertical line on the screen, and
    a straight line on a wall to a straight line on the screen, so four corners
    are enough to outline anything flat — which is what a game wanting to ring a
    decal needs, and why this is public. *)
let project_point t (pose : Player.t) ~point ~z =
  let rel = Vec.sub point pose.Player.pos in
  let distance = Vec.dot rel pose.Player.dir in
  if distance <= Ray.min_distance then None
  else
    let lateral = Vec.dot rel pose.Player.right in
    let camera_x = lateral /. (distance *. t.half_width) in
    Some
      ( (camera_x +. 1.) *. float_of_int t.width /. 2.,
        project_height t ~z ~distance )

(** Where a sprite lands on the screen: [(left, top, right, bottom)] in pixels.

    A sprite is a billboard facing the view, so [pose] places it — the player
    expressed in the room the sprite is in, since every room has its own
    coordinates. [floor_z] is the elevation of the floor under it and [distance]
    how far ahead it stands along the view.

    Where its foot and its head are is {!Room.sprite_foot} and
    {!Room.sprite_head}, so a sprite floating above the floor is projected by
    the same two calls as one resting on it. How wide it is is
    {!Room.sprite_half_width}, in cells; the screen width here is the screen
    {e height} scaled by the same ratio, which is the one line that keeps pixels
    square. [bottom - top] is what [size] cells cover at this distance, so
    scaling it by width-over-height is what [size * width / height] cells cover
    — measured vertically, applied horizontally, and correct because this module
    holds those two the same.

    {!Renderer} draws sprites with this. It is here, and public, because
    anything that wants to draw attention to one — an outline around what the
    player is looking at — has to land on the same rectangle, and there should
    be one answer to where that is. {!Sight.t} carries the [pose] and the
    [distance] it needs. *)
let sprite_box t (pose : Player.t) ~floor_z ~distance (s : Room.sprite) =
  let lateral =
    Vec.dot (Vec.sub s.Room.pos pose.Player.pos) pose.Player.right
  in
  let camera_x = lateral /. (distance *. t.half_width) in
  let centre = (camera_x +. 1.) *. float_of_int t.width /. 2. in
  let base = project_height t ~z:(Room.sprite_foot s ~floor_z) ~distance in
  let top = project_height t ~z:(Room.sprite_head s ~floor_z) ~distance in
  let half = (base -. top) *. Room.sprite_half_width s /. s.Room.size in
  (centre -. half, top, centre +. half, base)

(** How fast the middle of the screen rises with distance, at a given pitch: the
    vertical half of the ray the crosshair looks along, as world height gained
    per cell travelled. Level, it is zero and the crosshair looks along the
    horizon.

    It takes a pitch and not a viewport because it does not depend on one.
    Substituting [row = height / 2] into {!project_height} and solving for [z]
    leaves [2 * vertical_half_extent * pitch]: the [height] in the horizon's
    shear cancels the [height] in [projection], and what is left is the vertical
    field of view, which is the constant this module holds fixed. A window of
    any size or shape points its crosshair at the same place in the world.

    {!Sight} is what needs this — the renderer works in rows and never has to
    ask. *)
let centre_rise ~pitch = 2. *. vertical_half_extent *. pitch
