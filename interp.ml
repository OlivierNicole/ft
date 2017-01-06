type 'a expression = 'a

(*
 * Underlying OCaml types
 *)
type ipv4_address = int * int * int * int

(*
 * Types.
 *)
type _ typ =
  | Product : 'a typ * 'b typ -> ('a * 'b) typ
  | IPv4Address : (int * int * int * int) typ
  | Int : int typ
  | String : string typ
  | Bool : bool typ
let ( ** ) x y = Product (x, y)
let int = Int
let string = String
let bool = Bool
let ipv4_address = IPv4Address

(*
 * Channels.
 *)
type ('i, 'o) channel = 'i (* FIXME implement real channels. *)
type channel_inverted = bool

(*
 * Function definitions.
 *)
type ('dom, 'ret) fn = 'dom expression -> 'ret expression

let fn _ _ f = f

(*
 * Expressions.
 *)
let true_ = true
let false_ = false
let (&&) = (&&)
let (||) = (||)
let not b = not b
let (=) = (=)
let (>=) = (>=)
let (<=) = (<=)
let mkint i = i
let (+) = (+)
let (-) = (-)
let ( * ) = ( * )
let mod_ a b = a mod b
let (/) = (/)
let abs = Pervasives.abs
let mk_ipv4_address addr = addr
let int_to_address i =
  (i land 0xff000000, i land 0xff0000, i land 0xff00, i land 0xff)
let address_to_int (a, b, c, d) =
  (a lsl 24) lor (b lsl 16) lor (c lsl 8) lor d
let empty = []
let (^::) x xs = x :: xs
let (@) = (@)
let pair x y = x, y
let (:=) r x = r := x
let fst x = fst x
let snd x = snd x
let apply f x = f x
let rec integer_range lb ub =
  if ub <= lb then []
  else
    let new_ub = pred ub in
    new_ub :: integer_range lb new_ub
let (!) _inverted _chan _x = ()
let (?.) _inverted chan = chan
let peek _inverted _chan = ()
let mkstr s = s
