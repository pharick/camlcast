(* Implementation of {!Camlcast.Image}; the interface carries the prose. *)

open Result_ext

type t = {
  width : int;
  height : int;
  pixels : Color.t array;
  alpha : int array;
}

(* The ordinary array bound: [pixels] and [alpha] both hold words. What [index]
   does not check and [sample] reads on its word is the product below. *)
let fits = Extent.fits ~limit:Sys.max_array_length

let make ~width ?height f =
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
      alpha.(i) <- Color.clamp_channel a
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
  (* A file is a run-time failure and not an authoring mistake, so both of the
     sizes [make] refuses are asked about here and come back as an [Error]
     rather than as its [Invalid_argument]. Both, and not only the empty one:
     [load]'s type says a file is a condition, and a [make] reached with a size
     it will not take is that promise broken from inside — an exception out of a
     function whose whole signature says there is not going to be one. *)
  if w <= 0 || h <= 0 then
    Error
      (`Msg
         (Printf.sprintf
            "%s: a picture must have a positive size, and this one is %dx%d"
            path w h))
  else if not (fits ~width:w ~height:h) then
    Error
      (`Msg
         (Printf.sprintf "%s: a picture of %dx%d does not fit in an array" path
            w h))
  else Ok (make ~height:h ~width:w (fun ~u ~v -> Bitmap.sample s ~u ~v))

let of_asset = Asset.read load

let disc ~cx ~cy ~r ~u ~v =
  let du = float_of_int u -. cx and dv = float_of_int v -. cy in
  (du *. du) +. (dv *. dv) < r *. r

let clear = (Color.rgb 0 0 0, 0)
