We are porting Jane Street's Bonsai library to F#. The source repository is in /home/tobys/bonsai/[src|test|extra], and we are porting it into /home/tobys/WoofWare.Zoomies/Port. Do not attempt to migrate other subdirectories of Bonsai other than src, test or extra.

A similar port has already been done, it converted /home/tobys/repos/incremental to /home/tobys/repos/WoofWare.Incremental. Eagerly look to this for inspiration in how to structure the port we're doing.

In general we try to follow the structure of Bonsai closely, e.g. in having the same files present.

# Ordering
Start by implementing src/. You should figure out a topological sort for the modules, and start by implementing the module with zero dependencies. There's a dot file which represents module dependencies in bonsai/src/deps.dot, although it has a lot of excess information and will need decoding.
Once you have a topological ordering, ask me for confirmation before continuing with the first module.

If there are functions that come from outside Bonsai, they may live in the source cache, e.g. Fn.const lives in /home/tobys/.opam/default/lib/base/fn.ml
These should be implemented in a file that lives at the top of our project: External.fs.

Once we reach any (local_ graph) syntax, stop and ask for assistance.
Once this is done, we'll continue with implementing extra/, and then finally test/. 

# Porting idioms
Note the following idioms:

Existential types in F# are implemented with the visitor pattern (skolemization) as follows:

type FooEval<'ret> = abstract Eval<'a> : Foo<'a> -> 'ret
type FooCrate = abstract Apply<'ret> : FooEval<'ret> -> 'ret

You can use this encoding to implement GADTs where necessary. We use the terminology "crate" to mean what Jane Street uses the term "Packed" for.

===

We define a type and then a module with the same name, if necessary using CompilationRepresentationFlags.ModuleSuffix on the module. Jane Street usually instead defines the module and then a type within it called `t`; we define `type Foo = ...` and then `module Foo = ...` for the methods.

===

Jane Street uses `Uniform_array`; we use `voption array`, because .NET already has value types that can be stored in an array. Similarly, they use `Uopt`; we already have `ValueOption` (or `voption`) built-in.

===

I have already ported TimingWheel, one of the key dependencies. It's available as `WoofWare.TimingWheel`. Its naming conventions are the same as those in WoofWare.Incremental.

===

When porting names, Ocaml uses `snake_case`; F# uses `PascalCase` for the same terms.

===

OCaml has named arguments like `~f:Blah_type`. F# does not. These must be translated without the tilde.

===

OCaml makes heavy use of optional arguments like `?f`. F# is much more restricted about where it can do this. Translate them as mandatory arguments which have an option type.

===

We simply ignore the `sexp` functions in the OCaml.

===

The type system requires a genuine type, not System.Void, to be the return type of our visitor-pattern encoding of existential types.
The type `FakeUnit` is simply a one-element data type which serves this purpose.

===

We use `Object.ReferenceEquals` where OCaml would use `phys_same`.
It is important to note that `Object.ReferenceEquals` does not have OCaml's semantics if one or more of its inputs is a value type.
When checking the correctness of `Object.ReferenceEquals`, you must note whether its inputs are both class types.

===

The `invariant` pattern for expressing invariants in Jane Street's OCaml frequently calls `ignore` on fields when there are no invariants to express about those fields.
We simply don't mention such fields, rather than explicitly calling `ignore`.

===

Ocaml code sometimes has inline tests (tests written in the same file as the source code). Move these out to the test project.