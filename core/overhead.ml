(* Implementation of {!Camlcast_core.Overhead}; the interface carries the
   prose. *)

type t = {
  origin_x : float;
  origin_y : float;
  cx : float;
  cy : float;
  scale : float;
}

let bounds room =
  let corners = ref [] in
  for index = 0 to Room.wall_count room - 1 do
    let wall = Room.wall_at room index in
    corners := wall.Room.a :: wall.Room.b :: !corners
  done;
  for index = 0 to Room.threshold_count room - 1 do
    let threshold = Room.threshold_at room index in
    corners := threshold.Room.a :: threshold.Room.b :: !corners
  done;
  match !corners with
  | [] -> None
  | (first : Vec.t) :: rest ->
      Some
        (List.fold_left
           (fun (x0, y0, x1, y1) (v : Vec.t) ->
             ( Float.min x0 v.x,
               Float.min y0 v.y,
               Float.max x1 v.x,
               Float.max y1 v.y ))
           (first.x, first.y, first.x, first.y)
           rest)

let fit ~bounds:(x0, y0, x1, y1) ~x ~y ~width ~height ~inset =
  (* Guarded rather than divided: a room whose boundary is one point, which a
     description can be halfway through describing, would otherwise give a
     scale of infinity and put every wall at the same pixel. *)
  let span = Float.max 1e-6 (Float.max (x1 -. x0) (y1 -. y0)) in
  {
    origin_x = float_of_int (x + (width / 2));
    origin_y = float_of_int (y + (height / 2));
    cx = (x0 +. x1) /. 2.;
    cy = (y0 +. y1) /. 2.;
    scale = float_of_int (width - (2 * inset)) /. span;
  }

let scale t = t.scale

let to_panel t (v : Vec.t) =
  ( int_of_float (t.origin_x +. ((v.x -. t.cx) *. t.scale)),
    int_of_float (t.origin_y +. ((v.y -. t.cy) *. t.scale)) )

let to_room t (px, py) =
  Vec.make
    (t.cx +. ((float_of_int px -. t.origin_x) /. t.scale))
    (t.cy +. ((float_of_int py -. t.origin_y) /. t.scale))
