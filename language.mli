module type Semantics = sig
  
  open ~Pervasives

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
  macro ( ** ) : 'a typ -> 'b typ -> ('a * 'b) typ
  macro string : string typ
  macro int : int typ
  macro bool : bool typ
  macro void : unit typ
  (* record_type: not supported yet *)
  macro ipv4_address : ipv4_address typ

  (*
   * Channels
   *)
  (* Channel receives values of type 'o and sends values of type 'i *)
  type ('i, 'o) channel

  type channel_inverted = bool

  type void (* Empty type *)

  type (_, _) chan_type =
    | Io : 'i typ * 'o typ -> ('i, 'o) chan_type

  macro channel : ('i, 'o) chan_type -> ('i, 'o) channel
  (* Can read from channel? *)
  macro can : (_, _) channel -> bool expression
  macro put_incoming : ('a, _) channel -> 'a expression -> unit
  macro put_outgoing : (_, 'a) channel -> 'a expression -> unit

  (*
   * Function definitions
   *)
  (* Note: since currying is not available (functions are not first-class),
   * multiple arguments should be encoded as tuples. *)
  type ('dom, 'ret) fn
  macro fn : 'dom typ -> 'ret typ ->
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
  macro mkbool : bool -> bool expression
  macro (&&) : bool expression -> bool expression -> bool expression
  macro (||) : bool expression -> bool expression -> bool expression
  macro not : bool expression -> bool expression
  macro (=) : 'a expression -> 'a expression -> bool expression
  macro (>=) : 'a expression -> 'a expression -> bool expression
  macro (<=) : 'a expression -> 'a expression -> bool expression
  macro mkint : int -> int expression
  macro (+) : int expression -> int expression -> int expression
  macro (-) : int expression -> int expression -> int expression
  macro ( * ) : int expression -> int expression -> int expression
  macro mod_ : int expression -> int expression -> int expression
  macro (/) : int expression -> int expression -> int expression
  macro abs : int expression -> int expression
  macro mk_ipv4_address : (int * int * int * int) expression
    -> ipv4_address expression
  macro int_to_address : int expression -> ipv4_address expression
  macro address_to_int : ipv4_address expression -> int expression
  macro empty : unit -> 'a list expression
  macro (^::) : 'a expression -> 'a list expression -> 'a list expression
  macro (@) : 'a list expression -> 'a list expression
    -> 'a list expression
  (* TupleValue will have to be simulated using nested pairs. *)
  macro pair : 'a expression -> 'b expression -> ('a * 'b) expression
  (* Seq and if-then-else: cannot be implemented as a function because of strict
   * evaluation. *)
  macro seq : unit expression -> 'b expression -> 'b expression
  macro if_ : bool expression -> 'a expression -> 'a expression -> 'a expression
  (*val par : ?*)
  (* if-then-else *)
  macro (:=) : 'a ref expression -> 'a expression -> unit expression
  (*val update_indexable : ?*)
  (* RecordProjection: records not supported yet, using this instead: *)
  macro fst : ('a * _) expression -> 'a expression
  macro snd : (_ * 'b) expression -> 'b expression
  (* Don't know why in motto code the constructor is "Functor_App"; here I will
   * simply name it "fn_apply". *)
  macro apply : ('dom, 'ret) fn -> 'dom expression -> 'ret expression
  macro integer_range : int expression -> int expression
    -> int list expression
  (*val map : ?*)
  macro iterate :
    'a list expression (* List *)
    -> 'b expression (* Initial value *)
    -> ('b expression -> 'a expression -> 'b expression) (* Loop body *)
    -> 'b expression
  macro (<~) : (_, 'a) channel -> 'a expression -> unit expression
  macro (?.) : ('a, _) channel -> 'a expression
  macro (??) : ('a, _) channel -> 'a expression (* Peeking *)
  macro mkstr : string -> string expression
  (*val meta_quoted : ?*)
  (*val hole : ?*)
  (*val literal_expr : ?*)

  macro eval : 'a expression -> 'a
end
