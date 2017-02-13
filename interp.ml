type 'a expression = 'a Lazy.t

let pure = Lazy.from_val
let force = Lazy.force
let map : ('a -> 'b) -> 'a Lazy.t -> 'b Lazy.t =
  fun f x ->
    lazy (f (force x))
let lift2 : ('a -> 'b -> 'c) -> 'a Lazy.t -> 'b Lazy.t -> 'c Lazy.t =
  fun f x y ->
    lazy (f (force x) (force y))

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
let ( ** ) x y = Product (x, y)
let void = Void
let int = Int
let string = String
let bool = Bool
let ipv4_address = IPv4Address

(*
 * Channels.
 *)
type ('i, 'o) channel = 'i list ref * 'o list ref
  (* FIXME implement better channels. *)

type channel_inverted = bool

type void (* Empty type *)

type (_, _) chan_type =
  | Io : 'i typ * 'o typ -> ('i, 'o) chan_type

let channel _ = (ref [], ref [])

(*
 * Function definitions.
 *)
type ('dom, 'ret) fn = 'dom expression -> 'ret expression

let fn (_ : 'dom typ) (_ : 'ret typ)
    (f : 'dom expression -> 'ret expression) =
  f

(*
 * Expressions.
 *)
let true_ = pure true

let false_ = pure false

let (&&) x y =
  lazy
    (if force x then force y else false)

let (||) x y =
  lazy
    (if force x then true else force y)

let not = map not

let (=) a b =
  lift2 (=) a b

let (<>) a b =
  lift2 (<>) a b

let (>=) a b =
  lift2 (>=) a b

let (<=) a b =
  lift2 (<=) a b

let mkint i =
  Lazy.from_val i

let (+) x y =
  lift2 (+) x y

let (-) x y =
  lift2 (-) x y

let ( * ) x y =
  lift2 ( * ) x y

let mod_ a b =
  lift2 (mod) a b

let (/) x y =
  lift2 (/) x y

let abs x = map Pervasives.abs x

let mk_ipv4_address addr = addr

let int_to_address i =
  map (fun i ->
    (i land 0xff000000, i land 0xff0000, i land 0xff00, i land 0xff))
    i

let address_to_int a =
  map (fun (a, b, c, d) ->
    (a lsl 24) lor (b lsl 16) lor (c lsl 8) lor d)
    a

let empty =
  Lazy.from_val []

let (^::) (x : 'a expression) (xs : 'a list expression) : 'a list expression =
  lift2 (fun x xs -> x :: xs) x xs

let (@) l l' = lift2 (@) l l'

let pair x y = lift2 (fun x y -> x, y)

let ref (x : 'a expression) : 'a ref expression =
  lazy (Pervasives.ref (force x))

let (:=) r x = lift2 (:=) r x

let (!) r = map (!) r

let fst x = map fst x

let snd x = map snd x

let apply (f : ('dom, 'ret) fn) (x : 'dom expression) =
  f x

let seq (x : unit expression) (y : 'a expression) =
  lazy (
    force x;
    force y)

let if_ (cond : bool expression) (ift : 'a expression) (iff : 'a expression)
    : 'a expression =
  lazy (
    if force cond then force ift else force iff)

let rec integer_range (lb : int expression) (ub : int expression)
    : int list expression =
  if_ (ub <= lb)
    empty
    (let new_ub = map pred ub in
      new_ub ^:: integer_range lb new_ub)

let rec iterate l x f =
  match (force l) with
  | [] -> x
  | y :: ys ->
      let x' = iterate (pure ys) x f in
      f x' y

let (??) chan =
  match (force !(fst chan)) with
  | [] -> failwith "No data in channel."
  | x :: _ -> x

let (?.) chan =
  match force !(fst chan) with
  | [] -> failwith "No data in channel."
  | x :: xs -> seq (fst chan := pure xs) (pure x)

let mkstr (s : string) = Lazy.from_val s

let eval x = force x

(* Can read on channel? *)
let can (chan : ('i, 'o) channel) : bool expression =
  !(fst chan) <> []

let put_incoming chan x =
  fst chan := !(fst chan) @ [x]

let put_outgoing chan x =
  snd chan := !(snd chan) @ [x]

let (<~) chan x =
  put_outgoing chan x
