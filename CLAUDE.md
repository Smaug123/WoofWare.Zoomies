We are porting Jane Street's `incremental` to F#.

Here are some common patterns:

* GADTs don't exist in F#. We use the "crate" pattern (skolemization) to represent the existential type instead: `type FooCrate = abstract Apply<'ret> : FooEval<'ret> -> 'ret` and `type FooEval<'ret> = abstract Eval<'a> : Foo<'a> -> 'ret`.
* Named arguments (`~f` or `~f:...`) don't exist in F#; we simply don't use names.
* Optional arguments (`?f`) can be made in F#, but we just won't. Any optional args in the OCaml should be made non-optional in F#.
* We use `thisCaseConvention` instead of OCaml's `this_case_convention`, and we use `fooThrowing` instead of `foo_exn` indicating that a function can throw.
* Jane Street's `Uopt` type is F#'s built-in `ValueOption`; `Uopt.value_exn u` is `u.Value` in F#.
* Universally quantified types look like `let foo : type a. a t -> bool` in OCaml; in F# they are e.g. `let foo<'a> (x : 'a t) -> bool = ...`.
* OCaml refers to types as `Module_name.t`; we define the type to have name `ModuleName` and then we also define `module ModuleName = ...` (possibly using `[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]` if the defining file is different for the type vs the module).
* The OCaml heavily uses type inference; we annotate types of top-level function definitions.
* The OCaml defines many types centrally in what is `Types.fs` for us, and then it has to repeat some of those type definitions when it comes to the file that defines the module. F# doesn't need to repeat the type.
* OCaml explicitly uses `ref` to create a mutable refcell; F# usually just uses `let mutable`. OCaml uses `incr` to increment mutable ints; idiomatic F# uses `foo.Value <- foo.Value + 1`.
* `null` is a reserved keyword in F#. When porting OCaml code that uses `null` as an identifier, use `null'` instead.

# Building the project

To build the project, run:
```
dotnet build
```
