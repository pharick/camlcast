(* Implementation of {!Camlcast.Texture}; the interface carries the prose. *)

open Result_ext

let default_size = 64

type t = {
  size : int;
  texels : Color.t array;
  alpha : int array;
  opaque : bool;
}

let size t = t.size
let opaque t = t.opaque
let sample t ~u ~v = t.texels.((v * t.size) + u)
let alpha t ~u ~v = t.alpha.((v * t.size) + u)

(* [offset] reaches 1.0 exactly when a ray strikes a corner, which would index
   one past the end, so the result is clamped. *)
let column_of_offset t offset =
  Int.min (t.size - 1)
    (Int.max 0 (int_of_float (offset *. float_of_int t.size)))

let clamp v = Int.min 255 (Int.max 0 v)

let generate ?(size = default_size) f =
  if size <= 0 then
    invalid_arg "Texture.generate: a pattern must have a positive size";
  {
    size;
    texels =
      Array.init (size * size) (fun i ->
          Color.clamp (f ~u:(i mod size) ~v:(i / size)));
    alpha = Array.make (size * size) 255;
    opaque = true;
  }

let generate_masked ?(size = default_size) f =
  if size <= 0 then
    invalid_arg "Texture.generate_masked: a pattern must have a positive size";
  let n = size * size in
  let texels = Array.make n (Color.rgb 0 0 0) and alpha = Array.make n 0 in
  let opaque = ref true in
  for i = 0 to n - 1 do
    let color, a = f ~u:(i mod size) ~v:(i / size) in
    texels.(i) <- Color.clamp color;
    alpha.(i) <- clamp a;
    if a < 255 then opaque := false
  done;
  { size; texels; alpha; opaque = !opaque }

(* A file is a run-time failure and not an authoring mistake, so a picture of
   the wrong shape or no size comes back as an [Error] rather than as
   [generate]'s [Invalid_argument]. *)
let load path =
  let* s = Bitmap.load path in
  let w = s.Bitmap.width and h = s.Bitmap.height in
  if w <> h then
    Error
      (`Msg
         (Printf.sprintf "%s: a pattern must be square, and this one is %dx%d"
            path w h))
  else if w <= 0 then
    Error (`Msg (Printf.sprintf "%s: a pattern must have a positive size" path))
  else begin
    let n = w * w in
    let texels = Array.make n (Color.rgb 0 0 0) and alpha = Array.make n 0 in
    let opaque = ref true in
    for v = 0 to w - 1 do
      for u = 0 to w - 1 do
        let i = (v * w) + u in
        let color, a = Bitmap.sample s ~u ~v in
        texels.(i) <- color;
        alpha.(i) <- a;
        if a < 255 then opaque := false
      done
    done;
    Ok { size = w; texels; alpha; opaque = !opaque }
  end

let of_asset name =
  let* path = Asset.path name in
  load path

let hash a b =
  let h = a * 73856093 lxor (b * 19349663) in
  h lxor (h lsr 13) land max_int

let noise ~size ~seed ~cell ~u ~v =
  if size <= 0 then
    invalid_arg "Texture.noise: a pattern must have a positive size";
  if cell <= 0 || size mod cell <> 0 then
    invalid_arg "Texture.noise: cell must divide the pattern size";
  let cells = size / cell in
  let corner x y =
    float_of_int
      (hash ((x mod cells) + (seed * 7919)) ((y mod cells) + seed) land 255)
  in
  (* Smoothstep, so the interpolation arrives at each corner with zero slope and
     the eye cannot pick out the lattice the values hang on. *)
  let smooth t = t *. t *. (3. -. (2. *. t)) in
  let mix a b t = a +. ((b -. a) *. t) in
  let x0 = u / cell and y0 = v / cell in
  let fx = smooth (float_of_int (u mod cell) /. float_of_int cell)
  and fy = smooth (float_of_int (v mod cell) /. float_of_int cell) in
  let top = mix (corner x0 y0) (corner (x0 + 1) y0) fx
  and bottom = mix (corner x0 (y0 + 1)) (corner (x0 + 1) (y0 + 1)) fx in
  int_of_float (mix top bottom fy)
