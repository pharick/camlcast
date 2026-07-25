open House
open Support

(* Everything the generator can be asserted about rests on this: the same seed
   is the same house, so a run is a value rather than an event. *)
let the_same_seed_gives_the_same_sequence () =
  let draw seed = List.init 40 (fun _ -> Rng.below (Rng.make seed) 1000) in
  ignore (draw 0);
  let take seed =
    let t = Rng.make seed in
    List.init 40 (fun _ -> Rng.below t 1000)
  in
  Alcotest.(check (list int)) "twice from one seed" (take 99) (take 99);
  Alcotest.(check bool)
    "and a different seed differs" true
    (take 99 <> take 100)

let below_stays_in_range () =
  let t = Rng.make 7 in
  List.iter
    (fun bound ->
      for _ = 1 to 400 do
        let n = Rng.below t bound in
        Alcotest.(check bool)
          (Printf.sprintf "%d is in 0..%d" n (bound - 1))
          true
          (n >= 0 && n < bound)
      done)
    [ 1; 2; 6; 97; 1000 ];
  Alcotest.check_raises "a bound of zero"
    (Invalid_argument "Rng.below: bound must be positive") (fun () ->
      ignore (Rng.below t 0))

(* Not a randomness test — splitmix64's quality is not this project's problem —
   only that nothing has been wired up backwards and left it constant or
   trapped in a short cycle. *)
let the_sequence_is_not_degenerate () =
  let t = Rng.make 3 in
  let draws = List.init 600 (fun _ -> Rng.below t 6) in
  Alcotest.(check int)
    "every value of a six-sided die comes up" 6
    (List.length (List.sort_uniq compare draws));
  let counts =
    List.map
      (fun face -> List.length (List.filter (( = ) face) draws))
      [ 0; 1; 2; 3; 4; 5 ]
  in
  List.iter
    (fun n ->
      Alcotest.(check bool)
        (Printf.sprintf "%d of 600 is not wildly off a hundred" n)
        true
        (n > 50 && n < 160))
    counts

let unit_stays_in_the_unit_interval () =
  let t = Rng.make 11 in
  for _ = 1 to 500 do
    let x = Rng.unit t in
    Alcotest.(check bool)
      (Printf.sprintf "%f is in [0, 1)" x)
      true
      (x >= 0. && x < 1.)
  done

(* The catalogue is written as "three of these to one of those", so the weights
   have to actually come out in that proportion. *)
let weighted_follows_the_weights () =
  let t = Rng.make 23 in
  let draws =
    List.init 2000 (fun _ -> Rng.weighted t [ (1, `Rare); (9, `Common) ])
  in
  let rare = List.length (List.filter (( = ) `Rare) draws) in
  Alcotest.(check bool)
    (Printf.sprintf "about a tenth were rare (%d of 2000)" rare)
    true
    (rare > 120 && rare < 280);
  Alcotest.(check bool)
    "a weight of zero never comes up" true
    (List.for_all
       (( = ) `Yes)
       (List.init 200 (fun _ -> Rng.weighted t [ (0, `No); (5, `Yes) ])));
  Alcotest.check_raises "nothing to choose from"
    (Invalid_argument "Rng.weighted: no choices with any weight") (fun () ->
      ignore (Rng.weighted t [ (0, `No) ]))

let chance_is_a_probability () =
  let t = Rng.make 5 in
  Alcotest.(check bool)
    "never at zero" true
    (List.for_all not (List.init 200 (fun _ -> Rng.chance t 0.)));
  Alcotest.(check bool)
    "always at one" true
    (List.for_all Fun.id (List.init 200 (fun _ -> Rng.chance t 1.)));
  let hits =
    List.length (List.filter Fun.id (List.init 2000 (fun _ -> Rng.chance t 0.25)))
  in
  Alcotest.(check bool)
    (Printf.sprintf "about a quarter (%d of 2000)" hits)
    true
    (hits > 400 && hits < 600)

let () =
  Alcotest.run "Rng"
    [
      ( "sequence",
        [
          case "the same seed gives the same sequence"
            the_same_seed_gives_the_same_sequence;
          case "below stays in range" below_stays_in_range;
          case "the sequence is not degenerate" the_sequence_is_not_degenerate;
          case "unit stays in the unit interval" unit_stays_in_the_unit_interval;
        ] );
      ( "choosing",
        [
          case "weighted follows the weights" weighted_follows_the_weights;
          case "chance is a probability" chance_is_a_probability;
        ] );
    ]
