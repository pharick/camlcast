(* Implementation of {!Camlcast.Image}; the interface carries the prose. *)

open Result_ext

type t = {
  width : int;
  height : int;
  pixels : Color.t array;
  alpha : int array;
}

let clamp v = Int.min 255 (Int.max 0 v)

(* Whether [width * height] is a length an array can have, the same division as
   {!Texture.fits} and for the same reason: multiplied out, the check would
   overflow exactly where the thing it is checking for does. Past the bound the
   product wraps rather than growing, and the record would keep the size it was
   asked for while its two arrays came out shorter — which [index] does not
   check and [sample] reads on its word. *)
let fits ~width ~height = height <= Sys.max_array_length / width

let make ?height ~width f =
  let height = Option.value height ~default:width in
  if width <= 0 || height <= 0 then
    invalid_arg "Image.make: an image must have a positive size";
  if not (fits ~width ~height) then
    invalid_arg "Image.make: an image that size does not fit in an array";
  let n = width * height in
  let pixels = Array.make n (Color.rgb 0 0 0) and alpha = Array.make n 0 in
  for v = 0 to height - 1 do
    for u = 0 to width - 1 do
      let color, a = f ~u ~v in
      let i = (v * width) + u in
      pixels.(i) <- Color.clamp color;
      alpha.(i) <- clamp a
    done
  done;
  { width; height; pixels; alpha }

let index t ~u ~v = (v * t.width) + u

let sample t ~u ~v =
  let i = index t ~u ~v in
  (t.pixels.(i), t.alpha.(i))

let load path =
  let* s = Bitmap.load path in
  let w = s.Bitmap.width and h = s.Bitmap.height in
  (* A file is a run-time failure and not an authoring mistake, so an empty one
     comes back as an [Error] rather than as [make]'s [Invalid_argument]. *)
  if w <= 0 || h <= 0 then
    Error
      (`Msg
         (Printf.sprintf
            "%s: a picture must have a positive size, and this one is %dx%d"
            path w h))
  else Ok (make ~height:h ~width:w (fun ~u ~v -> Bitmap.sample s ~u ~v))

let of_asset name =
  let* path = Asset.path name in
  load path

let disc ~cx ~cy ~r ~u ~v =
  let du = float_of_int u -. cx and dv = float_of_int v -. cy in
  (du *. du) +. (dv *. dv) < r *. r

let clear = (Color.rgb 0 0 0, 0)
