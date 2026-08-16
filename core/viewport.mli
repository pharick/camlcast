(** Maps the window, at its current size, onto the camera.

    The window is resizable and can go fullscreen between any two frames, so the
    projection cannot be baked into constants. Each frame asks SDL for the size
    of the drawing surface and builds one of these from it, with {!make}.
    Nothing outside a frame keeps one.

    Two rules decide what a resize does. {b Pixels stay square}: a wall one cell
    wide and one cell tall covers the same number of pixels in both directions,
    which is what derives [half_width] from [width] and [projection].
    {b Widening the window reveals more world}: the projection is anchored to
    the window height. {!Config.fov} is read as the horizontal field of view at
    {!Config.reference_aspect}, and the vertical angle it implies is what is
    actually held fixed — dragging the window wider shows more to the left and
    right instead of magnifying.

    A pixel is its centre. {!ray_direction} and {!row_factor} take a pixel
    {e index} and answer for that pixel's centre; {!project_height},
    {!project_point} and {!sprite_box} answer in continuous coordinates, and the
    two are inverse — {!project_point} of {!ray_direction} at column [c] is
    [c + 0.5], the centre of that column. {!first_pixel} and {!last_pixel}
    convert back, and an extent is half-open: the pixels it covers run from
    {!first_pixel} of where it starts to {!last_pixel} of where it stops, which
    is one short of rounding both ends alike. *)

type t = {
  width : int;  (** the drawing surface, in pixels *)
  height : int;
  half_width : float;
      (** half the camera plane, in world units at distance 1 — derived from
          [width] and [projection] by the pixels-stay-square rule *)
  projection : float;  (** pixels covered by one world unit at distance 1 *)
  eye_z : float;  (** the elevation the eye sits at this frame *)
  horizon : float;  (** the screen row the eye looks straight along *)
}
(** Concrete, though [half_width] and [projection] are derived: a viewport is
    built by {!make} at the top of a frame, read everywhere, and discarded with
    the frame, so it has no time to drift out of sync. *)

val make : pitch:float -> eye_z:float -> width:int -> height:int -> t
(** The viewport for a drawing surface of that size, seen by an eye at that
    elevation, tipped by [pitch] — the window-height fraction {!Player.t}
    carries. Level, the horizon is the middle row; pitch shears it away from
    there, so looking up (positive) slides it down and reveals more ceiling. A
    minimised window reports a zero size, which is clamped to one pixel so the
    maths stays finite and the frame is merely pointless. *)

val ray_direction : t -> Player.t -> column:int -> Vec.t
(** Direction of the ray through screen column [column] — through its centre.
    Deliberately {e not} unit length: it is [dir + right * k], which is what
    makes {!Ray}'s [t] a distance perpendicular to the camera plane, free of
    fish-eye. One multiply and one add, no trigonometry per column. *)

val project_height : t -> z:float -> distance:float -> float
(** The screen row a point of world height [z] projects to, at perpendicular
    distance [distance]. Similar triangles: the height above the eye shrinks
    with distance and is measured down from the horizon. A wall's foot is this
    at the floor's height there, its top at the floor plus the wall's height,
    and the strip between them is the wall. A continuous row, not a pixel index;
    {!first_pixel} and {!last_pixel} name the pixels. *)

val first_pixel : float -> int
(** The first pixel an extent starting at that continuous coordinate covers: the
    first whose centre lies at or past it, which is [Float.round] of it. *)

val last_pixel : float -> int
(** The last pixel an extent stopping at that continuous coordinate covers,
    which is {!first_pixel} of it {e less one}.

    An extent is half-open — the coordinate is where the next thing begins — so
    the pixel whose centre it falls inside belongs to the next extent, not this
    one. Rounding both ends alike instead hands over a pixel the extent only
    partly covers, sampled at a point outside it: for a wall, that is a row
    drawn over the floor at a height below the wall's own foot, and
    {!Texture.row_of_height} tiles a negative height into the top of the pattern
    rather than refusing it.

    These two are public for the same reason {!sprite_box} is. Anything drawing
    attention to what the renderer drew — an outline round the thing the player
    is looking at — must land on the same pixels, and the rounding is half of
    where those are. *)

val row_factor : t -> row:int -> float
(** The dimensionless [(row + ½ - horizon) / projection] at the centre of a
    screen row — the quantity {!Plane.cast} needs to cast the floor and ceiling.
*)

val project_point :
  t -> Player.t -> point:Vec.t -> z:float -> (float * float) option
(** Where a world point lands on the screen: [(column, row)] in pixels, or
    [None] if it is level with the eye or behind it and has no place on the
    screen. [point] is its position on the floor plan and [z] its height, both
    in the frame the pose is expressed in.

    A vertical line in the world projects to a vertical line on the screen, and
    a straight line on a wall to a straight line on the screen, so four corners
    suffice to outline anything flat. That is what a game ringing a decal needs,
    and why this is public. *)

type box = { left : float; top : float; right : float; bottom : float }
(** A rectangle on the screen, in pixels.

    A record because the four values share one type and prose was the only thing
    saying which was which. Its two callers took it as a bare tuple and unpacked
    it under different names — [left, y_top, rightx, y_base] in {!Renderer} and
    [left, top, right, bottom] in {!Camlcast.Aim} — so a transposition was a
    silently misplaced rectangle rather than an error. *)

val sprite_box :
  t -> Player.t -> floor_z:float -> distance:float -> Room.sprite -> box
(** Where a sprite lands on the screen, in pixels. The pose is the player
    expressed in the sprite's room, [floor_z] the elevation of the floor under
    it, and [distance] how far ahead it stands along the view.

    {!Renderer} draws sprites with this. It is public because anything drawing
    attention to a sprite — an outline around what the player is looking at —
    must land on the same rectangle, and there should be one answer to where
    that is. {!Sight.t} carries the pose and distance it needs. *)

val centre_rise : pitch:float -> float
(** How fast the middle of the screen rises with distance, at a given pitch: the
    vertical component of the ray the crosshair looks along, as world height
    gained per cell travelled. Level, it is zero and the crosshair looks along
    the horizon. It takes a pitch, not a viewport, because it does not depend on
    one: a window of any size or shape points its crosshair at the same place in
    the world. {!Sight} needs this. *)
