type 'a expression

(*
 * Types
 *)
(* Analogous of the "type_value" variant. *)
(* string: use OCaml's *)
type integer
(* boolean: use OCaml's *)
(* record_type: not supported yet *)
type ipv4_address = integer * integer * integer * integer

(*
 * Channels
 *)
(* Channel receives values of type 'o and sends values of type 'o *)
type ('i, 'o) channel
type channel_inverted = bool

(*
 * Function definitions
 *)
(* Since currying is not available (functions are not first-class), we will
 * encode multiple arguments as tuple types. *)
type ('dom, 'ret) fn
val decl_fn : ('dom -> 'ret expression) -> ('dom, 'ret) fn

(*
 * Expressions
 *)
val true_ : bool expression
val false_ : bool expression
val and_ : bool expression -> bool expression -> bool expression
val or_ : bool expression -> bool expression -> bool expression
val not_ : bool expression -> bool expression
val equals : 'a expression -> 'a expression -> bool expression
val greater_than : 'a expression -> 'a expression -> bool expression
val less_than : 'a expression -> 'a expression -> bool expression
val int : integer -> integer expression
val plus : integer expression -> integer expression -> integer expression
val minus : integer expression -> integer expression -> integer expression
val times : integer expression -> integer expression -> integer expression
val mod_ : integer expression -> integer expression -> integer expression
val quotient : integer expression -> integer expression -> integer expression
val abs : integer expression -> integer expression
val ipv4_address : (integer * integer * integer * integer) expression
  -> ipv4_address expression
val int_to_address : integer expression -> ipv4_address expression
val address_to_int : ipv4_address expression -> integer expression
val empty_list : 'a list expression
val cons_list : 'a expression -> 'a list expression -> 'a list expression
val append_list : 'a list expression -> 'a list expression
  -> 'a list expression
(* TupleValue will have to be simulated using nested pairs. *)
val pair : 'a expression -> 'b expression -> ('a * 'b) expression
(* Seq and if-then-else: cannot be implemented as a function because of strict
 * evaluation. *)
(*val seq : 'a expression -> 'b expression -> 'b expression*)
(*val ite : bool expression -> 'a expression -> 'a expression option*)
(*val par : ?*)
(* if-then-else *)
val update : 'a ref expression -> 'a expression -> unit expression
(*val update_indexable : ?*)
(* RecordProjection: records not supported yet, using this instead: *)
val first : ('a * _) expression -> 'a expression
val second : (_ * 'b) expression -> 'b expression
(* Don't know why in motto code the constructor is "Functor_App"; here I will
 * simply name it "fn_apply". *)
val fn_apply : ('dom, 'ret) fn -> 'dom expression -> 'ret expression
val integer_range : integer expression -> integer expression
  -> integer list expression
(*val map : ?*)
(*val iterate : ?*)
val send : channel_inverted -> (_, 'a) channel -> 'a expression -> unit expression
val receive : channel_inverted -> ('a, _) channel -> 'a expression
val peek : channel_inverted -> _ channel -> unit expression
val str : string -> string expression
(*val meta_quoted : ?*)
(*val hole : ?*)
(*val literal_expr : ?*)
