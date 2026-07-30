(** How the window, whatever size it currently is, maps onto the camera.

    The window is resizable and can go fullscreen between any two frames, so
    the projection cannot be baked into constants: each frame asks SDL for the
    size of the drawing surface and builds one of these from it, with
    {!make}. Nothing outside a frame keeps one.

    Two rules decide what a resize does. {b Pixels stay square}: a wall one
    cell wide and one cell tall covers the same number of pixels in both
    directions, which is what derives [half_width] from [width] and
    [projection]. {b Widening the window reveals more world}: the projection
    is anchored to the window height, so {!Config.fov} is read as the
    horizontal field of view at {!Config.reference_aspect} and the vertical
    angle it implies is what is actually held fixed — dragging the window
    wider shows more to the left and right instead of magnifying what was
    already there.

    A pixel is its centre. {!ray_direction} and {!row_factor} take a pixel
    {e index} and answer for that pixel's centre; {!project_height},
    {!project_point} and {!sprite_box} answer in continuous coordinates, so
    [Float.round] of one of their answers is exactly "the pixel whose centre
    that covers". The two are inverse — {!project_point} of {!ray_direction}
    at column [c] is [c + 0.5], the centre of that very column. *)

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
    built by {!make} at the top of a frame, read everywhere, and thrown away
    with the frame, so there is no time for one to drift out of true. *)

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
    distance [distance]. Similar triangles: its height above the eye shrinks
    with distance and is measured down from the horizon. A wall's foot is this
    at the floor's height there, its top at the floor plus the wall's height,
    and the strip between them is the wall. A continuous row and not a pixel
    index — it is [Float.round] of this that names the pixel. *)

val row_factor : t -> row:int -> float
(** The dimensionless [(row + ½ - horizon) / projection] the centre of a
    screen row sits at, the quantity {!Plane.view_distance} needs to cast the
    floor and ceiling. *)

val project_point : t -> Player.t -> point:Vec.t -> z:float -> (float * float) option
(** Where a point of the world lands on the screen: [(column, row)] in pixels,
    or [None] if it is level with the eye or behind it and has no place on the
    screen at all. [point] is where it stands on the floor plan and [z] how
    high it is, both in the frame the pose is expressed in.

    A vertical line in the world projects to a vertical line on the screen,
    and a straight line on a wall to a straight line on the screen, so four
    corners are enough to outline anything flat — which is what a game wanting
    to ring a decal needs, and why this is public. *)

val sprite_box :
  t -> Player.t -> floor_z:float -> distance:float -> Room.sprite ->
  float * float * float * float
(** Where a sprite lands on the screen: [(left, top, right, bottom)] in
    pixels. The pose is the player expressed in the room the sprite is in,
    [floor_z] the elevation of the floor under it, and [distance] how far
    ahead it stands along the view.

    {!Renderer} draws sprites with this. It is here, and public, because
    anything that wants to draw attention to one — an outline around what the
    player is looking at — has to land on the same rectangle, and there should
    be one answer to where that is. {!Sight.t} carries the pose and the
    distance it needs. *)

val centre_rise : pitch:float -> float
(** How fast the middle of the screen rises with distance, at a given pitch:
    the vertical half of the ray the crosshair looks along, as world height
    gained per cell travelled. Level, it is zero and the crosshair looks along
    the horizon. It takes a pitch and not a viewport because it does not
    depend on one — a window of any size or shape points its crosshair at the
    same place in the world. {!Sight} is what needs this. *)
