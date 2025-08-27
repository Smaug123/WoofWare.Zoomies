Claude code's software engineering approach is mismatching what I would expect. Please help me write a CLAUDE.md that corrects these:
* It frequently takes shortcuts (e.g. using unsafe casts, objs, Teq.believeMe) rather than persist in getting the more type-safe approach.
* It ignores the existing CLAUDE.md file for how to migrate OCaml to F#.
* It goes beyond what I've asked for, which is unnecessary.


We are porting Jane Street's Bonsai library to F#. The source repository is in bonsai/[src|test|extra], and we are porting it into WoofWare.Zoomies/Port. Do not attempt to migrate other subdirectories of Bonsai other than src, test or extra.

Ignore existing files in WoofWare.Zoomies, focus only on WoofWare.Zoomies/Port. Create F# projects called WoofWare.Zoomies.Port[.Test]

A similar port has already been done, it converted incremental to WoofWare.Incremental. Eagerly look to this for inspiration in how to structure the port we're doing.

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

===

Import modules are not necessary in F#. Ignore them.

===

Interface files (ending with `_intf.ml` or similar) should be SKIPPED entirely. Do not create F# equivalents of these files. They define module signatures which are not needed in F#.

===

Functors are inexpressible in F#, they need to be inlined.

===

Line numbers can be obtained in F# via class members, but not module methods.
A sample in C# looks like:

public void TraceMessage(string message,
        [System.Runtime.CompilerServices.CallerMemberName] string memberName = "",
        [System.Runtime.CompilerServices.CallerFilePath] string sourceFilePath = "",
        [System.Runtime.CompilerServices.CallerLineNumber] int sourceLineNumber = 0)
{
    System.Diagnostics.Trace.WriteLine("message: " + message);
    System.Diagnostics.Trace.WriteLine("member name: " + memberName);
    System.Diagnostics.Trace.WriteLine("source file path: " + sourceFilePath);
    System.Diagnostics.Trace.WriteLine("source line number: " + sourceLineNumber);
}

===

Use `dotnet build` to see if the code written compiles. If it is unclear how best to resolve a compile error, ask for assistance.

===

# MANDATORY F# GADT Conversion Guide

## BINDING CONSTRAINTS

⚠️ **CRITICAL**: This is NOT a suggestion guide. These are MANDATORY requirements.

### RULE ZERO: NO SIMPLIFICATION ALLOWED
- The Crate Pattern is REQUIRED, not optional
- Type errors are EXPECTED - work through them systematically
- NEVER suggest "simpler approaches" or "alternatives"
- Compilation difficulty is NOT permission to abandon the pattern

---

## ENFORCEMENT PROTOCOL

### When You Encounter Type Errors:
1. **REQUIRED FIRST RESPONSE**: Show the exact error message
2. **REQUIRED SECOND STEP**: Analyze which Crate/Eval interface needs adjustment
3. **REQUIRED THIRD STEP**: Fix within the pattern constraints
4. **FORBIDDEN**: Any suggestion to simplify or abandon the pattern

### Automatic Red Flags:
If you write ANY of these phrases, you've violated the requirements:
- "Let me simplify this"
- "More straightforward approach"
- "Too many type errors"
- "Use regular discriminated unions instead"

### Required Response to Errors:
"I see type error [X]. This indicates [specific technical issue]. I'll adjust [specific component] while maintaining the Crate Pattern..."

---

## THE CRATE PATTERN (MANDATORY IMPLEMENTATION)

### Core Structure (NON-NEGOTIABLE):

```fsharp
// 1. ALWAYS create the Eval interface first
type SomeTypeEval<'ret> =
    abstract Eval<'a> : SomeType<'a> -> 'ret

// 2. ALWAYS create the Crate type
type SomeTypeCrate =
    abstract Apply<'ret> : SomeTypeEval<'ret> -> 'ret

// 3. ALWAYS provide the make function
module SomeTypeCrate =
    let make (value: SomeType<'a>) : SomeTypeCrate =
        { new SomeTypeCrate with
            member _.Apply e = e.Eval value }
```

### MANDATORY Conversion Rules:

#### Rule 1: Existential Types Detection
```ocaml
type packed_foo = T : _ foo -> packed_foo [@@unboxed]
```
**MUST** become:
```fsharp
type FooEval<'ret> = abstract Eval<'a> : Foo<'a> -> 'ret
type FooCrate = abstract Apply<'ret> : FooEval<'ret> -> 'ret
```

#### Rule 2: GADT Constructor Conversion
```ocaml
type 'a t =
  | Map : ('a1 -> 'a) * 'a1 Node.t -> 'a t
```
**MUST** become:
```fsharp
type Kind<'a> =
    | Map of MapCrate<'a>  // NEVER Map of ('a1 -> 'a) * 'a1 Node
```

#### Rule 3: Multi-Parameter GADTs
```ocaml
| Bind_main : (_, 'a) Bind.t -> 'a t
```
**MUST** become specialized crates:
```fsharp
type BindMainEval<'a, 'ret> = abstract Eval<'b> : Bind<'b, 'a> -> 'ret
type BindMainCrate<'a> = abstract Apply<'ret> : BindMainEval<'a, 'ret> -> 'ret
```

---

## TYPE ERROR DEBUGGING (MANDATORY PROCESS)

### Step 1: Error Analysis (REQUIRED)
- Identify the exact type mismatch
- Trace which Eval interface or Crate is involved
- Never skip this analysis

### Step 2: Pattern-Preserving Fix (REQUIRED)
Common solutions (try these IN ORDER):
1. Add missing `Crate.make` wrappers
2. Create specialized Eval interfaces
3. Use `Teq<'a, 'b>` for type equality proofs
4. Adjust type parameter order in Eval interfaces
5. Add helper modules with construction functions

### Step 3: Verification (REQUIRED)
- Ensure the fix maintains the Crate Pattern
- Verify type safety is preserved
- Confirm no existential types are exposed

---

## PATTERN RECOGNITION CHECKLIST (MANDATORY)

When converting OCaml to F#, you MUST identify:

- [ ] `Type : _ foo -> t` patterns → FooCrate required
- [ ] `'a t` with hidden type constructors → Crates in DU required
- [ ] Functions with existential types → Crate wrapping required
- [ ] Module.Packed types → Always becomes crates
- [ ] `(_, 'a)` patterns → Specialized crates required

---

## ENFORCEMENT EXAMPLES

### ✅ CORRECT Response to Type Error:
```
"I'm getting error: 'Expected FooCrate but got Foo<string>'. This means I need to wrap the value using FooCrate.make. The type parameter 'string' is being exposed when it should be hidden. Here's the fix while maintaining the Crate Pattern..."
```

### ❌ FORBIDDEN Response:
```
"I'm getting several type errors with the Crate Pattern. This is getting complex - let me use a simpler discriminated union approach instead..."
```

---

## COMPLEXITY COMMITMENT LEVEL

**ABSOLUTE COMMITMENT REQUIRED**
- Simplification: FORBIDDEN
- Pattern deviation: FORBIDDEN  
- On compilation errors: DEBUG AND FIX, never abandon
- Difficulty level: IRRELEVANT to pattern requirements

---

## FINAL ENFORCEMENT

This guide represents BINDING CONSTRAINTS, not suggestions. 

**If you find yourself wanting to simplify**: STOP. Re-read this guide. Debug the specific type error. Maintain the pattern.

**If you encounter "too many" type errors**: This is EXPECTED. Each error is a clue about which Crate or Eval interface needs adjustment.

**If the pattern seems "too complex"**: Complexity is INTENTIONAL for type safety. The user chose this pattern for a reason.

Your job is to implement the Crate Pattern correctly, not to judge its complexity or suggest alternatives.