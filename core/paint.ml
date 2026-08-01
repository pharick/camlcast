let clipped (fb : Framebuffer.t) ~x ~y ~w ~h =
  ( Int.max 0 x,
    Int.max 0 y,
    Int.min fb.Framebuffer.width (x + w),
    Int.min fb.Framebuffer.height (y + h) )

let rect fb ~x ~y ~w ~h ~color:(c : Color.t) ~alpha =
  let r = c.Color.r and g = c.Color.g and b = c.Color.b in
  let x0, y0, x1, y1 = clipped fb ~x ~y ~w ~h in
  (* Taken at the nearer end when it falls outside 0 .. 255, on both sides and
     for the same reason {!sub} reads a picture's own alpha that way: nothing at
     or below nothing, an outright write at or above solid. Framebuffer.blend
     weighs the destination with 255 - alpha and stores the result in a byte, so
     an alpha out of range there does not fade — it wraps, and the panel comes
     out a colour nobody asked for. This alpha arrives from P.rect, which is to
     say from a game, and a game is entitled to arrive at it by arithmetic. *)
  if alpha > 0 then
    for py = y0 to y1 - 1 do
      for px = x0 to x1 - 1 do
        if alpha >= 255 then Framebuffer.set fb ~x:px ~y:py ~r ~g ~b
        else Framebuffer.blend fb ~x:px ~y:py ~r ~g ~b ~alpha
      done
    done

let sub ?tint fb (img : Image.t) ~x ~y ~sx ~sy ~sw ~sh =
  let x0, y0, x1, y1 = clipped fb ~x ~y ~w:sw ~h:sh in
  (* And no further into the picture than the picture goes, at either edge. The
     near one matters as much as the far: [u] is [sx + px - x], so a negative
     [sx] would read before the row and land in the one above it rather than
     stopping. *)
  let x0 = Int.max x0 (x - sx)
  and y0 = Int.max y0 (y - sy)
  and x1 = Int.min x1 (x + img.Image.width - sx)
  and y1 = Int.min y1 (y + img.Image.height - sy) in
  for py = y0 to y1 - 1 do
    let v = sy + py - y in
    for px = x0 to x1 - 1 do
      let u = sx + px - x in
      let i = Image.index img ~u ~v in
      let a = img.Image.alpha.(i) in
      if a > 0 then begin
        let c = img.Image.pixels.(i) in
        let r, g, b =
          match tint with
          | None -> (c.Color.r, c.Color.g, c.Color.b)
          | Some (t : Color.t) ->
              ( c.Color.r * t.Color.r / 255,
                c.Color.g * t.Color.g / 255,
                c.Color.b * t.Color.b / 255 )
        in
        if a >= 255 then Framebuffer.set fb ~x:px ~y:py ~r ~g ~b
        else Framebuffer.blend fb ~x:px ~y:py ~r ~g ~b ~alpha:a
      end
    done
  done

let image ?tint fb (img : Image.t) ~x ~y =
  sub ?tint fb img ~x ~y ~sx:0 ~sy:0 ~sw:img.Image.width ~sh:img.Image.height

let bar fb ~x ~y ~w ~h ~fraction ~color =
  rect fb ~x:(x - 1) ~y:(y - 1) ~w:(w + 2) ~h:(h + 2) ~color:(Color.rgb 0 0 0)
    ~alpha:140;
  let fraction = Float.max 0. (Float.min 1. fraction) in
  rect fb ~x ~y
    ~w:(int_of_float (fraction *. float_of_int w))
    ~h ~color ~alpha:255

let dot fb ~x ~y ~color = rect fb ~x ~y ~w:1 ~h:1 ~color ~alpha:255

let line fb ~x0 ~y0 ~x1 ~y1 ~color =
  let steps = Int.max (abs (x1 - x0)) (abs (y1 - y0)) in
  if steps = 0 then dot fb ~x:x0 ~y:y0 ~color
  else
    let step from to_ i =
      from
      + int_of_float
          (Float.round
             (float_of_int i /. float_of_int steps *. float_of_int (to_ - from)))
    in
    (* Which of the [steps + 1] positions could land on the buffer at all.
       [steps] and [step] are untouched, so every pixel that was drawn is still
       drawn at the same place; what goes is the ones [dot] would have clipped
       away one at a time. The difference is not a nicety: these coordinates
       come from projecting the world, which divides by distance, so a ring
       round something close to the eye is a few pixels of line on a segment
       millions of pixels long.

       An axis constrains [i] to where [from + i * d / steps] stays on the
       buffer. Solved a whole pixel wide of each edge, which is more than the
       half-pixel [step]'s rounding can move a point, so no position that would
       have drawn is cut. An axis that does not move constrains nothing, or
       rules out the line entirely. *)
    let range from to_ limit =
      let d = to_ - from in
      if d = 0 then if from < 0 || from >= limit then None else Some (0, steps)
      else
        let at bound =
          float_of_int steps *. float_of_int (bound - from) /. float_of_int d
        in
        let lo, hi =
          if d > 0 then (at (-1), at limit) else (at limit, at (-1))
        in
        Some
          ( Int.max 0 (int_of_float (Float.floor lo)),
            Int.min steps (int_of_float (Float.ceil hi)) )
    in
    match
      (range x0 x1 fb.Framebuffer.width, range y0 y1 fb.Framebuffer.height)
    with
    | Some (lo, hi), Some (lo', hi') ->
        for i = Int.max lo lo' to Int.min hi hi' do
          dot fb ~x:(step x0 x1 i) ~y:(step y0 y1 i) ~color
        done
    | _ -> ()

let ring fb corners ~color =
  let corners = Array.of_list corners in
  let n = Array.length corners in
  for i = 0 to n - 1 do
    let x0, y0 = corners.(i) and x1, y1 = corners.((i + 1) mod n) in
    line fb ~x0 ~y0 ~x1 ~y1 ~color
  done

let crosshair fb ~color =
  let cx = fb.Framebuffer.width / 2 and cy = fb.Framebuffer.height / 2 in
  rect fb ~x:(cx - 5) ~y:cy ~w:11 ~h:1 ~color ~alpha:255;
  rect fb ~x:cx ~y:(cy - 5) ~w:1 ~h:11 ~color ~alpha:255
