let clipped (fb : Framebuffer.t) ~x ~y ~w ~h =
  ( Int.max 0 x,
    Int.max 0 y,
    Int.min fb.Framebuffer.width (x + w),
    Int.min fb.Framebuffer.height (y + h) )

let rect fb ~x ~y ~w ~h ~r ~g ~b ~alpha =
  let x0, y0, x1, y1 = clipped fb ~x ~y ~w ~h in
  for py = y0 to y1 - 1 do
    for px = x0 to x1 - 1 do
      if alpha >= 255 then Framebuffer.set fb ~x:px ~y:py ~r ~g ~b
      else Framebuffer.blend fb ~x:px ~y:py ~r ~g ~b ~a:alpha
    done
  done

let sub ?tint fb (img : Image.t) ~x ~y ~sx ~sy ~sw ~sh =
  let x0, y0, x1, y1 = clipped fb ~x ~y ~w:sw ~h:sh in
  (* And no further into the picture than the picture goes. *)
  let x1 = Int.min x1 (x + img.Image.width - sx)
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
        else Framebuffer.blend fb ~x:px ~y:py ~r ~g ~b ~a
      end
    done
  done

let image ?tint fb (img : Image.t) ~x ~y =
  sub ?tint fb img ~x ~y ~sx:0 ~sy:0 ~sw:img.Image.width ~sh:img.Image.height

let bar fb ~x ~y ~w ~h ~fraction ~r ~g ~b =
  rect fb ~x:(x - 1) ~y:(y - 1) ~w:(w + 2) ~h:(h + 2) ~r:0 ~g:0 ~b:0 ~alpha:140;
  let fraction = Float.max 0. (Float.min 1. fraction) in
  rect fb ~x ~y
    ~w:(int_of_float (fraction *. float_of_int w))
    ~h ~r ~g ~b ~alpha:255

let dot fb ~x ~y ~r ~g ~b = rect fb ~x ~y ~w:1 ~h:1 ~r ~g ~b ~alpha:255

let line fb ~x0 ~y0 ~x1 ~y1 ~r ~g ~b =
  let steps = Int.max (abs (x1 - x0)) (abs (y1 - y0)) in
  if steps = 0 then dot fb ~x:x0 ~y:y0 ~r ~g ~b
  else
    let step from to_ i =
      from
      + int_of_float
          (Float.round
             (float_of_int i /. float_of_int steps *. float_of_int (to_ - from)))
    in
    for i = 0 to steps do
      dot fb ~x:(step x0 x1 i) ~y:(step y0 y1 i) ~r ~g ~b
    done

let ring fb corners ~r ~g ~b =
  let corners = Array.of_list corners in
  let n = Array.length corners in
  for i = 0 to n - 1 do
    let x0, y0 = corners.(i) and x1, y1 = corners.((i + 1) mod n) in
    line fb ~x0 ~y0 ~x1 ~y1 ~r ~g ~b
  done

let crosshair fb ~r ~g ~b =
  let cx = fb.Framebuffer.width / 2 and cy = fb.Framebuffer.height / 2 in
  rect fb ~x:(cx - 5) ~y:cy ~w:11 ~h:1 ~r ~g ~b ~alpha:255;
  rect fb ~x:cx ~y:(cy - 5) ~w:1 ~h:11 ~r ~g ~b ~alpha:255
