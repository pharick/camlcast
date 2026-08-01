(* Implementation of {!Camlcast.Debug_map}; the interface carries the
   prose. *)

open Camlcast_core

let backdrop = Color.rgb 10 12 18
let border = Color.rgb 70 80 100
let wall_ink = Color.rgb 200 205 215
let normal_ink = Color.rgb 90 170 255
let linked = Color.rgb 90 210 120
let unlinked = Color.rgb 240 90 80
let eye = Color.rgb 255 220 120
let mark_error = Color.rgb 255 60 60
let mark_warning = Color.rgb 255 190 60

(* A tick this long in world units, so it stays the same length on the map
   whatever the room's size — which is what makes a row of them readable as a
   direction rather than as a fringe. *)
let normal_tick = 0.45
let margin = 8

let panel buffer =
  let side =
    Int.max 60
      (Int.min buffer.Framebuffer.width buffer.Framebuffer.height * 45 / 100)
  in
  (margin, margin, side, side)

(* Every point the map has to fit: the boundary and the openings in it. Sprites
   are deliberately left out of the measurement — a mote drifting far from the
   room would shrink everything else to fit it in. *)
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

let draw buffer world player diagnostics =
  let room_index = player.Player.room in
  let room = World.room world room_index in
  let x, y, w, h = panel buffer in
  Paint.rect buffer ~x ~y ~w ~h ~color:backdrop ~alpha:210;
  Paint.ring buffer
    [ (x, y); (x + w - 1, y); (x + w - 1, y + h - 1); (x, y + h - 1) ]
    ~color:border;
  match bounds room with
  | None -> ()
  | Some (x0, y0, x1, y1) ->
      let inner = 10 in
      let span = Float.max 1e-6 (Float.max (x1 -. x0) (y1 -. y0)) in
      let scale = float_of_int (w - (2 * inner)) /. span in
      (* Centred on the room's own middle, so a long thin room sits in the
         panel rather than in one corner of it. *)
      let cx = (x0 +. x1) /. 2. and cy = (y0 +. y1) /. 2. in
      let ox = float_of_int (x + (w / 2)) and oy = float_of_int (y + (h / 2)) in
      (* World y grows downward and so does the screen's, so this is a scale and
         an offset and never a flip. *)
      let at (v : Vec.t) =
        ( int_of_float (ox +. ((v.x -. cx) *. scale)),
          int_of_float (oy +. ((v.y -. cy) *. scale)) )
      in
      let segment a b ~color =
        let x0, y0 = at a and x1, y1 = at b in
        Paint.line buffer ~x0 ~y0 ~x1 ~y1 ~color
      in
      let disc centre radius ~color =
        let steps = 16 in
        Paint.ring buffer
          (List.init steps (fun step ->
               let angle =
                 2. *. Float.pi *. float_of_int step /. float_of_int steps
               in
               at (Vec.add centre (Vec.scale (Vec.of_angle angle) radius))))
          ~color
      in
      let cross centre ~color =
        let px, py = at centre in
        Paint.line buffer ~x0:(px - 3) ~y0:(py - 3) ~x1:(px + 3) ~y1:(py + 3)
          ~color;
        Paint.line buffer ~x0:(px - 3) ~y0:(py + 3) ~x1:(px + 3) ~y1:(py - 3)
          ~color
      in
      for index = 0 to Room.wall_count room - 1 do
        let wall = Room.wall_at room index in
        segment wall.Room.a wall.Room.b ~color:wall_ink;
        (* The tick is the whole reason to draw this from above: it points the
           way the wall faces, and a boundary wound the wrong way round is a
           row of them pointing out of the room instead of into it. *)
        let middle = Vec.scale (Vec.add wall.Room.a wall.Room.b) 0.5 in
        segment middle
          (Vec.add middle (Vec.scale wall.Room.normal normal_tick))
          ~color:normal_ink
      done;
      for index = 0 to Room.threshold_count room - 1 do
        let threshold = Room.threshold_at room index in
        let color =
          match World.portal world ~room:room_index ~threshold:index with
          | Some _ -> linked
          | None -> unlinked
        in
        segment threshold.Room.a threshold.Room.b ~color
      done;
      disc player.Player.pos Config.collision_padding ~color:eye;
      segment player.Player.pos
        (Vec.add player.Player.pos (Vec.scale player.Player.dir 0.9))
        ~color:eye;
      List.iter
        (fun (diagnostic : Check.t) ->
          match diagnostic.Check.spot with
          | Some (room, spot) when room = room_index ->
              cross spot
                ~color:
                  (match diagnostic.Check.severity with
                  | Check.Error -> mark_error
                  | Check.Warning -> mark_warning)
          | _ -> ())
        diagnostics
