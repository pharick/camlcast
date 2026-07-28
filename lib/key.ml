(** The keyboard, named by the engine rather than by SDL. See {!Key} for what a
    key is; this is the table from those names to the scancodes underneath, and
    it is the only place in the engine that has one. *)

open Tsdl

type t = int

let count = Sdl.Scancode.num_scancodes
let of_scancode scancode = scancode
let to_scancode key = key

(** The layout-dependent name first, because it is the one printed on the key in
    front of the player. A key the layout does not reach — the function row on
    some boards — has no keycode and falls back to the name of the place. *)
let name key =
  match Sdl.get_key_name (Sdl.get_key_from_scancode key) with
  | "" -> ( match Sdl.get_scancode_name key with "" -> "?" | place -> place)
  | printed -> printed

let a = Sdl.Scancode.a
let b = Sdl.Scancode.b
let c = Sdl.Scancode.c
let d = Sdl.Scancode.d
let e = Sdl.Scancode.e
let f = Sdl.Scancode.f
let g = Sdl.Scancode.g
let h = Sdl.Scancode.h
let i = Sdl.Scancode.i
let j = Sdl.Scancode.j
let k = Sdl.Scancode.k
let l = Sdl.Scancode.l
let m = Sdl.Scancode.m
let n = Sdl.Scancode.n
let o = Sdl.Scancode.o
let p = Sdl.Scancode.p
let q = Sdl.Scancode.q
let r = Sdl.Scancode.r
let s = Sdl.Scancode.s
let t = Sdl.Scancode.t
let u = Sdl.Scancode.u
let v = Sdl.Scancode.v
let w = Sdl.Scancode.w
let x = Sdl.Scancode.x
let y = Sdl.Scancode.y
let z = Sdl.Scancode.z
let k1 = Sdl.Scancode.k1
let k2 = Sdl.Scancode.k2
let k3 = Sdl.Scancode.k3
let k4 = Sdl.Scancode.k4
let k5 = Sdl.Scancode.k5
let k6 = Sdl.Scancode.k6
let k7 = Sdl.Scancode.k7
let k8 = Sdl.Scancode.k8
let k9 = Sdl.Scancode.k9
let k0 = Sdl.Scancode.k0
let f1 = Sdl.Scancode.f1
let f2 = Sdl.Scancode.f2
let f3 = Sdl.Scancode.f3
let f4 = Sdl.Scancode.f4
let f5 = Sdl.Scancode.f5
let f6 = Sdl.Scancode.f6
let f7 = Sdl.Scancode.f7
let f8 = Sdl.Scancode.f8
let f9 = Sdl.Scancode.f9
let f10 = Sdl.Scancode.f10
let f11 = Sdl.Scancode.f11
let f12 = Sdl.Scancode.f12
let return = Sdl.Scancode.return
let escape = Sdl.Scancode.escape
let backspace = Sdl.Scancode.backspace
let tab = Sdl.Scancode.tab
let space = Sdl.Scancode.space
let minus = Sdl.Scancode.minus
let equals = Sdl.Scancode.equals
let leftbracket = Sdl.Scancode.leftbracket
let rightbracket = Sdl.Scancode.rightbracket
let backslash = Sdl.Scancode.backslash
let semicolon = Sdl.Scancode.semicolon
let apostrophe = Sdl.Scancode.apostrophe
let grave = Sdl.Scancode.grave
let comma = Sdl.Scancode.comma
let period = Sdl.Scancode.period
let slash = Sdl.Scancode.slash
let up = Sdl.Scancode.up
let down = Sdl.Scancode.down
let left = Sdl.Scancode.left
let right = Sdl.Scancode.right
let insert = Sdl.Scancode.insert
let home = Sdl.Scancode.home
let pageup = Sdl.Scancode.pageup
let delete = Sdl.Scancode.delete
let kend = Sdl.Scancode.kend
let pagedown = Sdl.Scancode.pagedown
let lshift = Sdl.Scancode.lshift
let rshift = Sdl.Scancode.rshift
let lctrl = Sdl.Scancode.lctrl
let rctrl = Sdl.Scancode.rctrl
let lalt = Sdl.Scancode.lalt
let ralt = Sdl.Scancode.ralt
let lgui = Sdl.Scancode.lgui
let rgui = Sdl.Scancode.rgui
let numlockclear = Sdl.Scancode.numlockclear
let kp_divide = Sdl.Scancode.kp_divide
let kp_multiply = Sdl.Scancode.kp_multiply
let kp_minus = Sdl.Scancode.kp_minus
let kp_plus = Sdl.Scancode.kp_plus
let kp_enter = Sdl.Scancode.kp_enter
let kp_period = Sdl.Scancode.kp_period
let kp_1 = Sdl.Scancode.kp_1
let kp_2 = Sdl.Scancode.kp_2
let kp_3 = Sdl.Scancode.kp_3
let kp_4 = Sdl.Scancode.kp_4
let kp_5 = Sdl.Scancode.kp_5
let kp_6 = Sdl.Scancode.kp_6
let kp_7 = Sdl.Scancode.kp_7
let kp_8 = Sdl.Scancode.kp_8
let kp_9 = Sdl.Scancode.kp_9
let kp_0 = Sdl.Scancode.kp_0
let capslock = Sdl.Scancode.capslock
let printscreen = Sdl.Scancode.printscreen
let scrolllock = Sdl.Scancode.scrolllock
let pause = Sdl.Scancode.pause
let application = Sdl.Scancode.application
let menu = Sdl.Scancode.menu
