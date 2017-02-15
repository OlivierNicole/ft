### Overall plan

Build an implementation of the Flick[3] language, which seems to be
called "Crisp", as a DSL using macros.  (There are lots of names here:
it seems that Flick is a system built using an interpreter "Motto",
for a language Crisp, based on a previous language "Matron")

The current implementation[0] is a standalone compiler.  We should be
able to do better with macros for at least the following reasons:

* Instead of writing a lexer, parser, type checker, etc., we can just
  use OCaml's.

* Compilers (including OCaml and Motto) usually manipulate untyped
  representations of code, where the representation of '3' and the
  representation of '"four"' have the same type.

  With macros we can use a typed representation instead.

* Since our implementation is just a library, it'll integrate with
  existing OCaml code and tools.  For example, we can combine it with
  other DSLs in a single application, use Merlin to give information
  about Flick programs, etc.

### Approach

We'll use the "finally tagless" approach [1] which has been used to
successfully embed a wide range of DSLs.  For example, [2] is an
embedding of a query language into OCaml using the finally tagless
approach.

### First steps

The first step is to build a signature for the language, with one
abstract type for each kind of language construct ("expression",
"type", etc.), and one value member for each syntax constructor
("function call", "conditional", etc.)

The syntax for Crisp is defined here:

   https://github.com/NaaS/motto/blob/c5337d4f/syntax/crisp_syntax.ml

and the ASTs for types and expressions are defined here:
   https://github.com/NaaS/motto/blob/c5337d4f/syntax/crisp_syntax.ml#L36-L61
   https://github.com/NaaS/motto/blob/c5337d4f/syntax/crisp_syntax.ml#L259-L380

The translation from the AST type definitions into a module signature
is a fairly mechanical process.  Each data constructor declaration,
such as `LessThan`

```
LessThan of expression * expression
```

needs to be turned into a type signature, rather like a GADT declaration:

```
LessThan : expression -> expression -> expression
```

and each AST type needs to be given a parameter that corresponds to
the type of the code it represents (so that "expression" becomes "'a
expression", for example).

Then the types of the syntax constructors need to be instantiated to
reflect the types the corresponding expressions should have in the
language, which can be determined from the type checking algorithm:

   https://github.com/NaaS/motto/blob/c5337d4f/front-end/type_infer.ml#L86-L1059

For example, the type checking code for LessThan

   https://github.com/NaaS/motto/blob/c5337d4f/front-end/type_infer.ml#L135-L142

says that the types of its two aruments should be the same and the
type of the result should be Bool, so the final signature for
LessThan should look like this:

```
val lessThan : 'a expression -> 'a expression -> bool expression
```

This signature corresponds to the lexing, parsing, AST and type
checking code for LessThan in the Motto implementation.  Overall, I'd
expect our implementation of the "front end" to be at least an order
of magnitude smaller.

Not everything will be this easy: records, variants and functions are
likely to bring some challenges.  But we have techniques available
that should help -- e.g. the approach used in ctypes for structs may
well work here for records.

### Subsequent steps

Once we have a signature we can give it various implementations:

* An interpreter.  Typically this involves interpreting flick
  expressions as OCaml values, by defining a module that satisfies the
  signature with the following definition for expression:

    type 'a expression = 'a

* A compiler, using typed expressions to represent the output

    type 'a expression = 'a expr

* An optimizing compiler, using some more interesting definition for
  'expression'.

And we'll need to figure out how to get the whole thing running in the
Flick environment.

[0] https://github.com/NaaS/motto
[1] http://okmij.org/ftp/tagless-final/
[2] http://dl.acm.org/citation.cfm?doid=2847538.2847542
[3] https://www.usenix.org/node/196179
