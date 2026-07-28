(** [Key] is a table of names, and a table is only worth testing where it could
    be wrong: that two names are not secretly the same place, that the way out
    to a scancode and back is the identity, and that every key it names fits in
    the block {!Camlcast.Input} sizes for the keyboard. Get that last one wrong
    and a control quietly indexes into the mouse's part of the arrays. *)

open Camlcast
open Support

(* Every key the module names. Kept by hand, which is the point: a key added to
   [Key] without a line here is one nothing has checked. *)
let named =
  Key.
    [
      ("a", a);
      ("b", b);
      ("c", c);
      ("d", d);
      ("e", e);
      ("f", f);
      ("g", g);
      ("h", h);
      ("i", i);
      ("j", j);
      ("k", k);
      ("l", l);
      ("m", m);
      ("n", n);
      ("o", o);
      ("p", p);
      ("q", q);
      ("r", r);
      ("s", s);
      ("t", t);
      ("u", u);
      ("v", v);
      ("w", w);
      ("x", x);
      ("y", y);
      ("z", z);
      ("k1", k1);
      ("k2", k2);
      ("k3", k3);
      ("k4", k4);
      ("k5", k5);
      ("k6", k6);
      ("k7", k7);
      ("k8", k8);
      ("k9", k9);
      ("k0", k0);
      ("f1", f1);
      ("f2", f2);
      ("f3", f3);
      ("f4", f4);
      ("f5", f5);
      ("f6", f6);
      ("f7", f7);
      ("f8", f8);
      ("f9", f9);
      ("f10", f10);
      ("f11", f11);
      ("f12", f12);
      ("return", return);
      ("escape", escape);
      ("backspace", backspace);
      ("tab", tab);
      ("space", space);
      ("minus", minus);
      ("equals", equals);
      ("leftbracket", leftbracket);
      ("rightbracket", rightbracket);
      ("backslash", backslash);
      ("semicolon", semicolon);
      ("apostrophe", apostrophe);
      ("grave", grave);
      ("comma", comma);
      ("period", period);
      ("slash", slash);
      ("up", up);
      ("down", down);
      ("left", left);
      ("right", right);
      ("insert", insert);
      ("home", home);
      ("pageup", pageup);
      ("delete", delete);
      ("kend", kend);
      ("pagedown", pagedown);
      ("lshift", lshift);
      ("rshift", rshift);
      ("lctrl", lctrl);
      ("rctrl", rctrl);
      ("lalt", lalt);
      ("ralt", ralt);
      ("lgui", lgui);
      ("rgui", rgui);
      ("numlockclear", numlockclear);
      ("kp_divide", kp_divide);
      ("kp_multiply", kp_multiply);
      ("kp_minus", kp_minus);
      ("kp_plus", kp_plus);
      ("kp_enter", kp_enter);
      ("kp_period", kp_period);
      ("kp_1", kp_1);
      ("kp_2", kp_2);
      ("kp_3", kp_3);
      ("kp_4", kp_4);
      ("kp_5", kp_5);
      ("kp_6", kp_6);
      ("kp_7", kp_7);
      ("kp_8", kp_8);
      ("kp_9", kp_9);
      ("kp_0", kp_0);
      ("capslock", capslock);
      ("printscreen", printscreen);
      ("scrolllock", scrolllock);
      ("pause", pause);
      ("application", application);
      ("menu", menu);
    ]

(* Two names for one place would make a binding table quietly ambiguous: bind
   one and the other fires with it. *)
let no_two_names_are_the_same_place () =
  List.iter
    (fun (name, key) ->
      List.iter
        (fun (other, elsewhere) ->
          if name <> other && Key.to_scancode key = Key.to_scancode elsewhere
          then
            Alcotest.failf "%s and %s are both scancode %d" name other
              (Key.to_scancode key))
        named)
    named

(* Every named key has to land inside the block [Input] sizes for the keyboard,
   or its control would index into the mouse's part of the arrays. *)
let every_key_fits_the_keyboard_block () =
  List.iter
    (fun (name, key) ->
      let scancode = Key.to_scancode key in
      Alcotest.(check bool)
        (Printf.sprintf "%s is in range" name)
        true
        (scancode >= 0 && scancode < Key.count))
    named

(* The escape hatch is the way to a key the table does not name, and it has to
   reach the named ones by the same road or the two would disagree. *)
let the_way_out_and_back_is_the_identity () =
  List.iter
    (fun (name, key) ->
      Alcotest.(check bool)
        (name ^ " survives the round trip")
        true
        (Key.of_scancode (Key.to_scancode key) = key))
    named

(* [name] feeds help text, so the one thing it must never do is hand back the
   empty string: a gap in the middle of a line reads as a bug rather than as a
   key nobody could name. *)
let every_key_can_be_printed () =
  List.iter
    (fun (name, key) ->
      Alcotest.(check bool)
        (name ^ " prints as something")
        true
        (String.length (Key.name key) > 0))
    named;
  Alcotest.(check bool)
    "and so does a place nobody named" true
    (String.length (Key.name (Key.of_scancode 0)) > 0)

let () =
  Alcotest.run "Key"
    [
      ( "the table",
        [
          case "no two names are the same place" no_two_names_are_the_same_place;
          case "every key fits the keyboard block"
            every_key_fits_the_keyboard_block;
          case "the way out and back is the identity"
            the_way_out_and_back_is_the_identity;
          case "every key can be printed" every_key_can_be_printed;
        ] );
    ]
