type 'a expression = 'a expr

macro map : ('a -> 'b) expr -> 'a expr -> 'b expr =
  fun f x -> << $f $x >>
macro lift2 : ('a -> 'b -> 'c) expr -> 'a expr -> 'b expr -> 'c expr =
  fun f x y ->
    << $f $x $y >>

(*
 * Underlying OCaml types
 *)
type ipv4_address = int * int * int * int

(*
 * Types.
 *)
type _ typ =
  | Void : unit typ
  | Product : 'a typ * 'b typ -> ('a * 'b) typ
  | IPv4Address : (int * int * int * int) typ
  | Int : int typ
  | String : string typ
  | Bool : bool typ
macro ( ** ) x y = Product (x, y)
macro void = Void
macro int = Int
macro string = String
macro bool = Bool
macro ipv4_address = IPv4Address

(*
 * Channels.
 *)
type ('i, 'o) channel = ('i list ref * 'o list ref) expr
  (* FIXME implement better channels. *)

type channel_inverted = bool

type void (* Empty type *)

type (_, _) chan_type =
  | Io : 'i typ * 'o typ -> ('i, 'o) chan_type

macro channel _ = << (ref [], ref []) >>

type ('dom, 'ret) fn = 'dom expression -> 'ret expression

macro fn (_ : 'dom typ) (_ : 'ret typ) (f : ('dom, 'ret) fn) = f

macro mkbool b = Expr.of_bool b

macro (&&) x y =
  lift2 << (&&) >> x y

macro (||) x y =
  lift2 << (||) >> x y

macro (=) x y =
  lift2 << (=) >> x y

macro (<>) x y =
  lift2 << (<>) >> x y

macro (>=) x y =
  lift2 << (>=) >> x y

macro (<=) x y =
  lift2 << (<=) >> x y

macro mkint i = Expr.of_int i

macro (+) x y =
  lift2 << (+) >> x y

macro (-) x y =
  lift2 << (-) >> x y

macro ( * ) x y =
  lift2 << ( * ) >> x y

macro (mod) x y =
  lift2 << (mod) >> x y

macro (/) x y =
  lift2 << (/) >> x y

macro mk_ipv4_address (a,b,c,d) =
  << (mkint a, mkint b, mkint c, mkint d) >>

macro int_to_address i =
  map << fun i ->
    (i land 0xff000000, i land 0xff0000, i land 0xff00, i land 0xff) >>
    i

macro empty () =
  << [] >>

macro (^::) (x : 'a expression) (xs : 'a list expression) : 'a list expression =
  lift2 << fun x xs -> x :: xs >> x xs
