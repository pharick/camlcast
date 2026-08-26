(** What a row of slots reads as.

    The line between the two halves is what these assert: a slot with no printer
    still says which hook made it and whether it moved, and says nothing about
    its value rather than guessing at one. *)

open Camlcast_loom

let case name body = Alcotest.test_case name `Quick body
let slot kind changed shown = { Hook.kind; changed; shown }

let a_slot_reads_as_its_hook_and_its_value () =
  Alcotest.(check string)
    "a state that moved, and said what to" "state   * 3"
    (Camlcast_edit.Slots.line (slot Hook.State true (Some "3")));
  Alcotest.(check string)
    "a state that did not move" "state     3"
    (Camlcast_edit.Slots.line (slot Hook.State false (Some "3")))

let a_slot_that_said_nothing_says_nothing () =
  Alcotest.(check string)
    "no printer, so no value -- but still a kind and still a mark" "ref     * -"
    (Camlcast_edit.Slots.line (slot Hook.Ref true None));
  Alcotest.(check string)
    "a memo that has not computed is the same case" "memo      -"
    (Camlcast_edit.Slots.line (slot Hook.Memo false None))

(* A component holding nothing and a panel with nothing to show are different
   facts, and a blank in both places would say neither. *)
let a_component_holding_nothing_says_so () =
  Alcotest.(check (list string))
    "not an empty list" [ "(no hooks)" ]
    (Camlcast_edit.Slots.lines [||]);
  Alcotest.(check (list string))
    "and a row reads in hook order"
    [ "state     0"; "effect    -" ]
    (Camlcast_edit.Slots.lines
       [| slot Hook.State false (Some "0"); slot Hook.Effect false None |])

let () =
  Alcotest.run "Slots"
    [
      ( "a line",
        [
          case "reads as its hook and its value"
            a_slot_reads_as_its_hook_and_its_value;
          case "that said nothing says nothing"
            a_slot_that_said_nothing_says_nothing;
          case "a component holding nothing says so"
            a_component_holding_nothing_says_so;
        ] );
    ]
