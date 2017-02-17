open ~Pervasives

type 'a expression = 'a ~Lazy.t

macro pure = ~Lazy.from_val
macro force = ~Lazy.force
macro map : ('a -> 'b) -> 'a ~Lazy.t -> 'b ~Lazy.t =
  fun f x ->
    lazy (f (force x))
macro lift2 : ('a -> 'b -> 'c) -> 'a ~Lazy.t -> 'b ~Lazy.t -> 'c ~Lazy.t =
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
macro ( ** ) x y = Product (x, y)
macro void = Void
macro int = Int
macro string = String
macro bool = Bool
macro ipv4_address = IPv4Address

(*
 * Channels.
 *)
type ('i, 'o) channel = 'i list ref * 'o list ref
  (* FIXME implement better channels. *)

type channel_inverted = bool

type void (* Empty type *)

type (_, _) chan_type =
  | Io : 'i typ * 'o typ -> ('i, 'o) chan_type

macro channel _ = (ref [], ref [])

(*
 * Function definitions.
 *)
type ('dom, 'ret) fn = 'dom expression -> 'ret expression

macro fn (_ : 'dom typ) (_ : 'ret typ)
    (f : 'dom expression -> 'ret expression) =
  f

(*
 * Expressions.
 *)
macro mkbool b = pure b

macro (&&) x y =
  lazy
    (if force x then force y else false)

macro (||) x y =
  lazy
    (if force x then true else force y)

macro not b = map not b

macro (=) a b =
  lift2 (=) a b

macro (<>) a b =
  lift2 (<>) a b

macro (>=) a b =
  lift2 (>=) a b

macro (<=) a b =
  lift2 (<=) a b

macro mkint i =
  ~Lazy.from_val i

macro (+) x y =
  lift2 (+) x y

macro (-) x y =
  lift2 (-) x y

macro ( * ) x y =
  lift2 ( * ) x y

macro (mod) a b =
  lift2 (mod) a b

macro (/) x y =
  lift2 (/) x y

macro abs x = map ~Pervasives.abs x

macro mk_ipv4_address addr = addr

macro int_to_address i =
  map (fun i ->
    (i land 0xff000000, i land 0xff0000, i land 0xff00, i land 0xff))
    i

macro address_to_int a =
  map (fun (a, b, c, d) ->
    (a lsl 24) lor (b lsl 16) lor (c lsl 8) lor d)
    a

macro empty () =
  ~Lazy.from_val []

macro (^::) (x : 'a expression) (xs : 'a list expression) : 'a list expression =
  lift2 (fun x xs -> x :: xs) x xs

macro (@) l l' = lift2 (@) l l'

macro pair x y = lift2 (fun x y -> x, y) x y

macro ref (x : 'a expression) : 'a ref expression =
  lazy (~Pervasives.ref (force x))

macro (:=) r x = lift2 (:=) r x

macro (!) r = map (!) r

macro fst x = map fst x

macro snd x = map snd x

macro apply (f : ('dom, 'ret) fn) (x : 'dom expression) =
  f x

macro seq (x : unit expression) (y : 'a expression) =
  lazy (
    force x;
    force y)

macro if_ (cond : bool expression) (ift : 'a expression) (iff : 'a expression)
    : 'a expression =
  lazy (
    if force cond then force ift else force iff)

static rec integer_range' (lb : int) (ub : int) : int list =
  let open ~Pervasives in
  if ub <= lb then []
  else pred ub :: integer_range' lb (pred ub)

macro rec integer_range (lb : int expression) (ub : int expression)
    : int list expression =
  lazy (
    integer_range' (force lb) (force ub)
  )

macro rec iterate (l : 'a list expression) (x : 'b expression) f =
  match (force l) with
  | [] -> x
  | y :: ys ->
      let x' = iterate (pure ys) x f in
      f x' (pure y)

macro (??) (chan : ('i, 'o) channel) : 'i expression =
  match (~Pervasives.fst chan).contents with
  | [] -> failwith "No data in channel."
  | x :: _ -> pure x

macro (?.) (chan : ('i, 'o) channel) : 'i expression =
  match (~Pervasives.fst chan).contents with
  | [] -> failwith "No data in channel."
  | x :: xs -> seq (pure (~Pervasives.(:=) (~Pervasives.fst chan) xs)) (pure x)

macro mkstr (s : string) = ~Lazy.from_val s

macro eval x = force x

(* Can read on channel? *)
macro can (chan : ('i, 'o) channel) : bool expression =
  match ~Pervasives.(!) (~Pervasives.fst chan) with
  | [] -> pure false
  | _ -> pure true

macro put_incoming chan x =
  let open ~Pervasives in
  fst chan := !(fst chan) @ [force x]

macro put_outgoing chan x =
  let open ~Pervasives in
  snd chan := !(snd chan) @ [force x]

macro (<~) chan x =
  lazy (put_outgoing chan x)
