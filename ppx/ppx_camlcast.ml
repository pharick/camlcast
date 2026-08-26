(* Wraps each of {!Camlcast.P}'s element constructors in
   {!Camlcast_loom.Element.at}, so that a description carries the place in a
   source file it was written at and a tool outside can find the expression
   again.

   Off unless [CAMLCAST_POSITIONS] says otherwise. Dune sets it per profile,
   which is what keeps source paths out of a shipped binary: a release build
   preprocesses with this rewriter installed and has it do nothing. *)

open Ppxlib

let enabled =
  match Sys.getenv_opt "CAMLCAST_POSITIONS" with
  | Some ("1" | "true") -> true
  | _ -> false

(* A path is P's if its last step is: [P], [Camlcast.P], or whatever else a
   game aliased it to on the way in. Nothing here resolves modules, so this is
   the most it can honestly claim. *)
let functions = Camlcast_vocabulary.functions
let values = Camlcast_vocabulary.values
let named_p = function Lident "P" | Ldot (_, "P") -> true | _ -> false

(* Bare [wall] means P's wall inside [P.( ... )] and means whatever is in scope
   anywhere else -- a game's own local named [text] is not this rewriter's
   business. Hence [inside]: qualified names are taken anywhere, bare ones only
   under the open that gives them their meaning. *)
let targets ~inside ~names = function
  | Ldot (prefix, name) when named_p prefix -> List.mem name names
  | Lident name -> inside && List.mem name names
  | _ -> false

let opens_p = function
  | { pmod_desc = Pmod_ident { txt; _ }; _ } -> named_p txt
  | _ -> false

(* The tuple written out rather than [__POS__]. The rewriter is already holding
   the location, and an emitted [__POS__] would be expanded against whatever
   location it ended up carrying rather than the one meant. The four numbers
   are what {!Camlcast_loom.Element.pos} documents: the columns are columns,
   and an expression spanning several lines has no span here -- what is wanted
   from it is an anchor. *)
let position (loc : Location.t) =
  let start = loc.loc_start and stop = loc.loc_end in
  let open Ast_builder.Default in
  pexp_tuple ~loc
    [
      estring ~loc start.pos_fname;
      eint ~loc start.pos_lnum;
      eint ~loc (start.pos_cnum - start.pos_bol);
      eint ~loc (stop.pos_cnum - stop.pos_bol);
    ]

(* Camlcast.Element rather than Camlcast_loom.Element: (implicit_transitive_deps
   false) means a game naming only camlcast cannot name camlcast.loom, and a
   game preprocessed by this is by definition naming camlcast. *)
let wrap expression =
  let loc = expression.pexp_loc in
  let at =
    Ast_builder.Default.pexp_ident ~loc
      { txt = Ldot (Ldot (Lident "Camlcast", "Element"), "at"); loc }
  in
  Ast_builder.Default.eapply ~loc at [ position loc; expression ]

class positions =
  object
    inherit Ast_traverse.map as super
    val inside = false

    method! expression expression =
      match expression.pexp_desc with
      | Pexp_open (declaration, body) when opens_p declaration.popen_expr ->
          let body = {<inside = true>}#expression body in
          { expression with pexp_desc = Pexp_open (declaration, body) }
      | Pexp_apply ({ pexp_desc = Pexp_ident { txt; _ }; _ }, _)
        when targets ~inside ~names:functions txt ->
          (* Descended into first, so that the arguments -- a room's children,
             a wall's decals -- are wrapped as themselves, and then wrapped
             once. Wrapping before descending would put this rewriter's own
             output back through it. *)
          wrap (super#expression expression)
      | Pexp_ident { txt; _ } when targets ~inside ~names:values txt ->
          wrap expression
      | _ -> super#expression expression
  end

let () =
  Driver.register_transformation "camlcast_positions"
    ?impl:(if enabled then Some (new positions)#structure else None)
