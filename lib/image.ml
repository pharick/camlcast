(* Implementation of {!Camlcast.Image}; the interface carries the prose. *)

open Result_ext

type t = {
  width : int;
  height : int;
  pixels : Color.t array;
  alpha : int array;
}

let clamp v = Int.min 255 (Int.max 0 v)

let make ?height width f =
  let height = Option.value height ~default:width in
  if width <= 0 || height <= 0 then
    invalid_arg "Image.make: an image must have a positive size";
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
  let* s = Surface.read path in
  let w = s.Surface.width and h = s.Surface.height in
  (* A file is a run-time failure and not an authoring mistake, so an empty one
     comes back as an [Error] rather than as [make]'s [Invalid_argument]. *)
  if w <= 0 || h <= 0 then
    Error
      (`Msg
         (Printf.sprintf
            "%s: a picture must have a positive size, and this one is %dx%d"
            path w h))
  else Ok (make ~height:h w (fun ~u ~v -> Surface.sample s ~x:u ~y:v))

let of_asset name =
  let* path = Asset.path name in
  load path

let disc ~cx ~cy ~r u v =
  let du = float_of_int u -. cx and dv = float_of_int v -. cy in
  (du *. du) +. (dv *. dv) < r *. r

let clear = (Color.rgb 0 0 0, 0)
