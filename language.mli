type 'a expression

(*
 * Underlying OCaml types
 *)
type ipv4_address

(*
 * Flick types
 *)
(* Analogous of the "type_value" variant. *)
type _ typ
val ( ** ) : 'a typ -> 'b typ -> ('a * 'b) typ
val string : string typ
val int : int typ
val bool : bool typ
(* record_type: not supported yet *)
val ipv4_address : ipv4_address typ

(*
 * Channels
 *)
(* Channel receives values of type 'o and sends values of type 'i *)
type ('i, 'o) channel

type channel_inverted = bool

type void (* Empty type *)

type (_, _) chan_type =
  | Io : 'i * 'o -> ('i, 'o) chan_type

(*
 * Function definitions
 *)
(* Note: since currying is not available (functions are not first-class),
 * multiple arguments should be encoded as tuples. *)
type ('dom, 'ret) fn
val fn : 'dom typ -> 'ret typ ->
  ('dom expression -> 'ret expression) -> ('dom, 'ret) fn

(*
 * Processes
 *)
(*
type _ chan_types =
  | Nil : unit chan_types
  | Cons : 'a * 'b chan_types -> ('a * 'b) chan_types

type 'chans process
*)

(*
 * Expressions
 *)
val true_ : bool expression
val false_ : bool expression
val (&&) : bool expression -> bool expression -> bool expression
val (||) : bool expression -> bool expression -> bool expression
val not : bool expression -> bool expression
val (=) : 'a expression -> 'a expression -> bool expression
val (>=) : 'a expression -> 'a expression -> bool expression
val (<=) : 'a expression -> 'a expression -> bool expression
val mkint : int -> int expression
val (+) : int expression -> int expression -> int expression
val (-) : int expression -> int expression -> int expression
val ( * ) : int expression -> int expression -> int expression
val mod_ : int expression -> int expression -> int expression
val (/) : int expression -> int expression -> int expression
val abs : int expression -> int expression
val mk_ipv4_address : (int * int * int * int) expression
  -> ipv4_address expression
val int_to_address : int expression -> ipv4_address expression
val address_to_int : ipv4_address expression -> int expression
val empty : 'a list expression
val (^::) : 'a expression -> 'a list expression -> 'a list expression
val (@) : 'a list expression -> 'a list expression
  -> 'a list expression
(* TupleValue will have to be simulated using nested pairs. *)
val pair : 'a expression -> 'b expression -> ('a * 'b) expression
(* Seq and if-then-else: cannot be implemented as a function because of strict
 * evaluation. *)
(*val seq : 'a expression -> 'b expression -> 'b expression*)
(*val ite : bool expression -> 'a expression -> 'a expression option*)
(*val par : ?*)
(* if-then-else *)
val (:=) : 'a ref expression -> 'a expression -> unit expression
(*val update_indexable : ?*)
(* RecordProjection: records not supported yet, using this instead: *)
val fst : ('a * _) expression -> 'a expression
val snd : (_ * 'b) expression -> 'b expression
(* Don't know why in motto code the constructor is "Functor_App"; here I will
 * simply name it "fn_apply". *)
val apply : ('dom, 'ret) fn -> 'dom expression -> 'ret expression
val integer_range : int expression -> int expression
  -> int list expression
(*val map : ?*)
val iterate :
  'a list expression (* List *)
  -> 'b (* Initial value *)
  -> ('b expression -> 'a expression -> 'b expression) (* Loop body *)
  -> 'b expression
val (!) : channel_inverted -> (_, 'a) channel -> 'a expression -> unit expression
val (?.) : channel_inverted -> ('a, _) channel -> 'a expression
val peek : channel_inverted -> _ channel -> unit expression
val mkstr : string -> string expression
(*val meta_quoted : ?*)
(*val hole : ?*)
(*val literal_expr : ?*)
