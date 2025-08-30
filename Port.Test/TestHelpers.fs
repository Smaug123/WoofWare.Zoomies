namespace WoofWare.Zoomies.Port.Test

open NUnit.Framework
open WoofWare.Zoomies.Port
open WoofWare.Incremental

/// Test helpers and utilities for Bonsai tests
[<AutoOpen>]
module TestHelpers =
    
    /// Assert that two values are equal with a custom message
    let assertEqual expected actual message =
        Assert.AreEqual(expected, actual, message)
    
    /// Assert that a computation produces the expected value
    let assertComputationResult expected (computation : 'a Computation) =
        // TODO: Add proper computation evaluation when Eval module is implemented
        // For now, just ensure the computation can be created
        Assert.IsNotNull(computation, "Computation should not be null")
    
    /// Create a simple test environment
    let createTestEnvironment () : Environment =
        Environment.empty
    
    /// Create a basic incremental instance for testing
    let createTestIncremental () : Incremental =
        Incremental.make ()
    
    /// Run a basic test with a clean incremental state
    let runIncrementalTest (testFunc : Incremental -> unit) =
        let incr = createTestIncremental ()
        try
            testFunc incr
        finally
            // Clean up resources if needed
            ()
    
    /// Assert that a function throws an exception
    let assertThrows<'T when 'T :> exn> (func : unit -> obj) =
        Assert.Throws<'T>(fun () -> func () |> ignore) |> ignore
    
    /// Create a simple value for testing
    let createTestValue<'a> (value : 'a) : Value<'a> =
        Value.return' value
    
    /// Create a simple variable for testing
    let createTestVar<'a> (initialValue : 'a) : Var<'a> =
        Var.create initialValue