(** A small deterministic random number generator, seeded once per run.

    The house is generated as it is explored, which makes it the one part of the
    game with no fixed value to assert against. Seeding it is what makes it
    testable at all: the same seed builds the same house, so a test can expand a
    hundred rooms and check every invariant of the result, and a bug found by
    walking into it can be walked into again.

    This is splitmix64 — a counter and one round of mixing, no state beyond the
    counter. Not cryptographic and not trying to be; it only has to be
    reproducible and not visibly patterned, and it is small enough to read in
    one sitting, which the stdlib's [Random] is not. *)

type t = { mutable state : int64 }

let make seed = { state = Int64.of_int seed }

(* The golden-ratio increment and the two shift/multiply rounds are the
   published constants. They are not adjustable knobs: the sequence's quality
   comes from exactly these numbers. *)
let gamma = 0x9E3779B97F4A7C15L

let next t =
  t.state <- Int64.add t.state gamma;
  let z = t.state in
  let z = Int64.mul (Int64.logxor z (Int64.shift_right_logical z 30)) 0xBF58476D1CE4E5B9L in
  let z = Int64.mul (Int64.logxor z (Int64.shift_right_logical z 27)) 0x94D049BB133111EBL in
  Int64.logxor z (Int64.shift_right_logical z 31)

(** A non-negative int below [bound]. Taking the remainder biases the low values
    very slightly — by about [bound] divided by two to the sixty-second — which
    at the sizes anything here asks for is far below what a player or a test
    could notice. *)
let below t bound =
  if bound <= 0 then invalid_arg "Rng.below: bound must be positive"
  else Int64.to_int (Int64.rem (Int64.shift_right_logical (next t) 1) (Int64.of_int bound))

(** A float from zero up to but not including one. *)
let unit t = float_of_int (below t 1_000_000) /. 1_000_000.

(** [chance t p] is true with probability [p]. *)
let chance t p = unit t < p

(** Pick from a list of [(weight, value)] pairs, in proportion to the weights.
    Weights are ints because they are written by hand in the catalogue and read
    better as "three of these to one of those" than as decimals. *)
let weighted t choices =
  let total = List.fold_left (fun n (w, _) -> n + w) 0 choices in
  if total <= 0 then invalid_arg "Rng.weighted: no choices with any weight";
  let roll = below t total in
  let rec pick n = function
    | [] -> assert false
    | (w, value) :: rest -> if n < w then value else pick (n - w) rest
  in
  pick roll choices

(** Pick one element of a non-empty array. *)
let pick t items = items.(below t (Array.length items))
