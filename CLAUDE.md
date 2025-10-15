# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

# Project

This is WoofWare.Zoomies, a reactive immediate-mode TUI framework which will eventually follow the model of Jane Street's Bonsai. It is written in F#, but is currently extremely incomplete.

The intended philosophy of the project is that from a small set of powerful and coherent primitives, it should be possible to build a number of higher-level ergonomic libraries which provide an easy-to-use interface.
When using the higher-level libraries, the coherence of the underlying primitives should permit the user to drop down seamlessly, as low-level as necessary to achieve any particular customisation.
Ideally, the foundation of the framework is very small, with perfect compositional properties, such that an ergonomic system is merely a natural corollary of the primitives.

The framework should avoid doing work unless that work is necessary.
Ultimately, we will use something like Bonsai to deduplicate work.

The framework renders the world afresh each cycle.
It doesn't inherently consider the virtual DOM on one render loop to be related to the DOM of the previous loop, for example.
It stores state internally for efficiency, but as far as the end-user programmer is concerned, they simply provide a virtual DOM on request every tick, and we render it.

# Commands

## Build
- `dotnet build` - Standard .NET build (`dotnet` is on the path thanks to `direnv`)
- `nix build` - Build using Nix flake

## Test
- `dotnet test` - Run all tests
- `dotnet test --filter "TestMethod=SpecificTest"` - Run specific test
- `dotnet test WoofWare.Zoomies.Test` - Run only the test project

## Code Quality
- `dotnet fantomas .` - Format F# code using Fantomas
- `./analyzers/run.sh` - Run F# analyzers. These generally don't fail with an exit code; they'll print their output, though. Treat warnings as errors (this is not actually easy to do with the current analyzer tool, otherwise I'd have made the analyzer script fail on warnings).

Always format with Fantomas before committing, and run the analyzers with `./analyzers/run.sh`.

# Architecture

## Project Structure
- **WoofWare.Zoomies/** - Core TUI framework library
- **WoofWare.Zoomies.App/** - Demonstration/example application
- **WoofWare.Zoomies.Test/** - Unit tests and test infrastructure

## Documentation

Architecture documentation lives in `docs/architecture/`.

## Core Concepts

### Virtual DOM (Vdom)
The framework uses a virtual DOM approach with these key types:
- `Vdom<'bounds>` - Virtual DOM nodes supporting text content, checkboxes, bordered panels, and split layouts
- Direction-based panel splitting (Vertical/Horizontal) with proportional or absolute sizing
- Focus management system for interactive elements

### Nursery Pattern
Task management using a nursery pattern (`Nursery.fs`) for structured concurrency with proper cancellation and cleanup.

### World State Management
- `WorldFreezer<'appEvent>` - Manages application state changes and event processing
- `WorldProcessor<'appEvent, 'userState>` - Processes world state changes and updates VDOM
- Event-driven architecture with keystroke handling and focus cycling. The render loop summons a readout of all the external changes at the start of each render; the framework consumes input only on demand

### Rendering System
- `RenderState` - Tracks rendering state including previous VDOM for diffing
- Console abstraction layer for terminal operations
- ANSI control sequence and mouse mode support

## Writing tests

* When writing tests that exercise the UI, use the system as a user would. For example, don't use spooky external mutable state to control vdom creation; just let the WoofWare.Zoomies framework give you an appropriate user state, and send keystrokes to manipulate the state.

## File Compilation Order
F# files must be compiled in dependency order. Core files follow a sequence something like this:
1. `ConsoleModifiers.fs` - Input modifiers
1. `Nursery.fs` - Task nursery pattern
1. `Vdom.fs` - Virtual DOM definitions
1. `CtrlCHandler.fs` - Signal handling
1. `ConsoleColor.fs`, `Terminal.fs`, `Console.fs` - Terminal abstraction
1. `IStopwatch.fs` - Timing interface (with Myriad-generated mocks)
1. `WorldFreezer.fs` - State management
1. `Render.fs` - Rendering engine
1. `App.fs` - Application framework

## Dependency libraries

### WoofWare.Expect snapshot testing

The usual workflow for updating snapshots using the WoofWare.Expect snapshot testing library is:

* Enter bulk-update mode by setting a `[<OneTimeSetUp>]` function (from NUnit.Framework) to `GlobalBuilderConfig.enterBulkUpdateMode ()`
* Run the tests. They will fail in the process of updating snapshots (this is so that you can't accidentally commit a test in update mode).
* Undo bulk-update mode by commenting out the `enterBulkUpdateMode ()`.
* Rerun the tests, if you like, to observe that the snapshots are now working.

# F# Language Gotchas and Hints

## Recursive functions in modules require `let rec ... and` syntax

When defining mutually recursive functions or a function that calls itself within an F# module, you must use:
- `let rec` for the first function
- `and` (not `let`) for subsequent functions in the mutual recursion group

**Incorrect:**
```fsharp
module Foo =
    let private resolveConstraint x = ... resolveConstraint ...  // Error: resolveConstraint not defined
    let private otherFunction = ...
```

**Correct:**
```fsharp
module Foo =
    let rec private resolveConstraint x = ... resolveConstraint ...
    and private otherFunction = ...
    and private anotherFunction = ...
```

## Type ordering in F# projects matters for cross-file references

Unlike C# with its two-pass compiler, F# is a single-pass compiler where types must be defined before they're used. This affects file ordering in `.fsproj` files.

## Mutable lists

It's almost always correct to use `ResizeArray` rather than a mutable F# `list`; it's simply almost always much more efficient.

## IAsyncDisposable

You can `use foo = someIAsyncDisposable` from within the `task { ... }` computation expression.
NUnit is happy to have tests be `Task`s, and FsCheck is happy to have properties be functions producing `Task`s; don't block inside tests, but use the appropriate `task` computation expression.

## Object.ReferenceEquals

There's an analyzer banning `Object.ReferenceEquals` because it's type-unsafe and does the wrong thing silently on structs.
We've defined `Object.referenceEquals`, a type-safe version that will fail to compile if used with structs; use this function instead.

## FsCheck properties

Use unit-returning `actual |> shouldEqual expected` (with an `FsUnitTyped` assertion) rather than `actual = expected` as the conclusion of an FsCheck property.
When that property fails, the version which throws gives you much better output than the one with an equality check.

## Constructing FsCheck properties

Generally try and avoid filtering unless it's really hard to avoid.
You can often construct what you need instead of filtering.
For example, instead of filtering to a nonempty list, you can pass an `'a` and an `'a list`, and as the first line of the function, prepend the element to the list. Now it's nonempty!

# NUnit bugs

NUnit's filtering is pretty borked.
You can't apply filters that contain special characters in the test name (like a space character).
You have to do e.g. `FullyQualifiedName~singleword` rather than `FullyQualifiedName~single word test`, but this only works on tests whose names are single words to begin with.

Instead of running `dotnet test`, you can perform a build (`dotnet build`) and then run `dotnet woofware.nunittestrunner WoofWare.Zoomies.Test/bin/Debug/net9.0/WoofWare.Zoomies.Test.dll`.
This is an NUnit test runner which accepts a `--filter` arg that takes the same filter syntax as `dotnet test`, but actually parses it correctly: test names can contain spaces.
(The most foolproof way to provide test names to WoofWare.NUnitTestRunner is by XML-encoding: e.g. `FullyQualifiedName="MyNamespace.MyTestsClass&lt;ParameterType1%2CParameterType2&gt;.MyTestMethod"`. The `~` query operator is also supported.)
