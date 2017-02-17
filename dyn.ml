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

macro not b = map <<not>> b

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

macro abs x =
  map <<abs>> x

macro mk_ipv4_address quadruple =
  <<
    let (a,b,c,d) = $quadruple in
    (a,b,c,d)
  >>

macro int_to_address i =
  map << fun i ->
    (i land 0xff000000, i land 0xff0000, i land 0xff00, i land 0xff) >>
    i

macro address_to_int a =
  map
    << fun (a, b, c, d) ->
      (a lsl 24) lor (b lsl 16) lor (c lsl 8) lor d >>
    a

macro empty () =
  << [] >>

macro (^::) (x : 'a expression) (xs : 'a list expression) : 'a list expression =
  lift2 << fun x xs -> x :: xs >> x xs

macro (@) x y =
  lift2 << (@) >> x y

macro pair x y =
  lift2 << fun x y -> x, y >> x y

macro ref (x : 'a expression) : 'a ref expression =
  << ref $x >>

macro (:=) r y =
  lift2 << (:=) >> r y

macro (!) r =
  map << (!) >> r

macro fst x =
  map <<fst>> x

macro snd x =
  map <<snd>> x

macro apply (f : ('dom, 'ret) fn) (x : 'dom expression) : 'ret expression =
  f x

macro seq (x : unit expression) (y : 'a expression) =
  << $x ; $y >>

macro if_ (cond : bool expression) (ift : 'a expression) (iff : 'a expression)
    : 'a expression =
  << if $cond then $ift else $iff >>

let rec integer_range' (lb : int) (ub : int) : int list =
  let open Pervasives in
  if ub <= lb then []
  else pred ub :: integer_range' lb (pred ub)

macro rec integer_range (lb : int expression) (ub : int expression)
    : int list expression
 =
   << integer_range' $lb $ub >>

let rec iterate' l x f =
  match l with
  | [] -> x
  | y :: ys ->
      let x' = iterate' ys x f in
      f x' y

macro rec iterate (l : 'a list expression) (x : 'b expression) f =
  <<
    iterate' $l $x (fun z -> $(f << z >>))
  >>

macro (??) (chan : ('i, 'o) channel) : 'i expression =
  let open Pervasives in
  <<
    match !(fst $chan) with
    | [] -> failwith "No data in channel."
    | x :: _ -> x
  >>

macro (?.) (chan : ('i, 'o) channel) : 'i expression =
  let open Pervasives in
  <<
    let chan = $chan in
    match !(fst chan) with
    | [] -> failwith "No data in channel."
    | x :: xs -> fst chan := xs; x
  >>

macro mkstr s = Expr.of_string s

macro can (chan : ('i, 'o) channel) : bool expression =
  let open Pervasives in
  <<
    match !(fst $chan) with
    | [] -> false
    | _ -> true
  >>

macro put_incoming chan x =
  let open Pervasives in
  << let chan = $chan in fst chan := !(fst chan) @ [$x] >>

macro put_outgoing chan x =
  let open Pervasives in
  << let chan = $chan in snd chan := !(snd chan) @ [$x] >>

macro (<~) chan x =
  put_outgoing chan x
