type 'a expression = 'a

(*
 * Types.
 *)
type integer = int
type ipv4_address = integer * integer * integer * integer

(*
 * Channels.
 *)
type ('i, 'o) channel = 'i (* FIXME implement real channels. *)
type channel_inverted = bool

(*
 * Function definitions.
 *)
type ('dom, 'ret) fn = 'dom -> 'ret expression

let decl_fn f = f

(*
 * Expressions.
 *)
let true_ = true
let false_ = false
let and_ = (&&)
let or_ = (||)
let not_ b = not b
let equals = (=)
let greater_than = (>=)
let less_than = (<=)
let int i = i
let plus = (+)
let minus = (-)
let times = ( * )
let mod_ a b = a mod b
let quotient = (/)
let abs = Pervasives.abs
let ipv4_address addr = addr
let int_to_address i =
  (i land 0xff000000, i land 0xff0000, i land 0xff00, i land 0xff)
let address_to_int (a, b, c, d) =
  (a lsl 24) lor (b lsl 16) lor (c lsl 8) lor d
let empty_list = []
let cons_list x xs = x :: xs
let append_list = (@)
let pair x y = x, y
let update r x = r := x
let first x = fst x
let second x = snd x
let fn_apply f x = f x
let rec integer_range lb ub =
  if ub <= lb then []
  else
    let new_ub = pred ub in
    new_ub :: integer_range lb new_ub
let send _inverted _chan _x = ()
let receive _inverted chan = chan
let peek _inverted _chan = ()
let str s = s
