namespace WoofWare.Zoomies.Port.Test

open NUnit.Framework
open WoofWare.Zoomies.Port

[<TestFixture>]
type SimplifyTests() =
    
    [<Test>]
    member _.``FreeVariables empty should work``() =
        let empty = Simplify.FreeVariables.empty
        let isEmpty = Simplify.FreeVariables.isEmpty empty
        Assert.IsTrue(isEmpty, "Empty FreeVariables should be empty")
    
    [<Test>]
    member _.``FreeVariables add should work``() =
        let empty = Simplify.FreeVariables.empty
        let withVar = Simplify.FreeVariables.add empty 42
        let isEmpty = Simplify.FreeVariables.isEmpty withVar
        Assert.IsFalse(isEmpty, "FreeVariables with added variable should not be empty")
    
    [<Test>]
    member _.``FreeVariables merge should work``() =
        let fv1 = Simplify.FreeVariables.add Simplify.FreeVariables.empty 1
        let fv2 = Simplify.FreeVariables.add Simplify.FreeVariables.empty 2
        let merged = Simplify.FreeVariables.merge fv1 fv2
        
        Assert.IsFalse(Simplify.FreeVariables.isEmpty merged, "Merged FreeVariables should not be empty")
    
    [<Test>]
    member _.``Env empty should work``() =
        let env = Simplify.Env.empty
        let result = Simplify.Env.tryFind env 123
        Assert.IsTrue(result.IsNone, "Empty environment should not contain any values")
    
    [<Test>]
    member _.``Env add and find should work``() =
        let env = Simplify.Env.empty
        let envWithValue = Simplify.Env.add env 42 (box "test")
        let result = Simplify.Env.tryFind envWithValue 42
        
        Assert.IsTrue(result.IsSome, "Environment should contain added value")
    
    [<Test>]
    member _.``OptionOrMiss None should work``() =
        let none : Simplify.OptionOrMiss<int> = Simplify.OptionOrMiss.None
        let mapped = Simplify.OptionOrMiss.map (fun x -> x + 1) none
        
        match mapped with
        | Simplify.OptionOrMiss.None -> Assert.Pass("Mapping None should stay None")
        | _ -> Assert.Fail("Mapping None should stay None")
    
    [<Test>]
    member _.``OptionOrMiss Some should work``() =
        let some : Simplify.OptionOrMiss<int> = Simplify.OptionOrMiss.Some (42, false)
        let mapped = Simplify.OptionOrMiss.map (fun x -> x + 1) some
        
        match mapped with
        | Simplify.OptionOrMiss.Some (43, false) -> Assert.Pass("Mapping Some should transform value")
        | _ -> Assert.Fail("Mapping Some should transform value correctly")
    
    [<Test>]
    member _.``SimplifyValue constant should work``() =
        let value = Value.return' 42
        let result = Simplify.simplifyValue value
        
        match result with
        | Simplify.OptionOrMiss.Some (42, false) -> Assert.Pass("Constant value should simplify to Some")
        | _ -> Assert.Fail("Constant value should simplify correctly")
    
    [<Test>]
    member _.``Optimize should not crash``() =
        let computation = Computation.Return (Value.return' "test")
        let optimized = Simplify.optimize computation
        
        Assert.IsNotNull(optimized, "Optimize should return non-null computation")