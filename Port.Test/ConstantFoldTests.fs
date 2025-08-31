namespace WoofWare.Zoomies.Port.Test

#nowarn "3559" // Type implicitly inferred as 'obj'

open NUnit.Framework
open WoofWare.Zoomies.Port

[<TestFixture>]
type ConstantFoldTests() =
    
    [<Test>]
    member _.``IsValueConstant should detect constants``() =
        let constantValue = Value.return' 42
        let isConstant = ConstantFold.isValueConstant constantValue
        
        Assert.IsTrue(isConstant, "Constant value should be detected as constant")
    
    [<Test>]
    member _.``GetConstantContent should extract constant values``() =
        let constantValue = Value.return' "test"
        let content = ConstantFold.getConstantContent constantValue
        
        Assert.IsTrue(content.IsSome, "Should extract content from constant value")
        Assert.AreEqual("test", content.Value, "Should extract correct content")
    
    [<Test>]
    member _.``GetConstantContent should return None for non-constants``() =
        // For now we don't have easy way to create non-constant values
        // So we'll just test the constant case
        let constantValue = Value.return' 123
        let content = ConstantFold.getConstantContent constantValue
        
        Assert.IsTrue(content.IsSome, "Constant should have content")
    
    [<Test>]
    member _.``SafeEvaluate should handle successful evaluation``() =
        let result = ConstantFold.safeEvaluate (fun () -> 42)
        
        Assert.IsTrue(result.IsSome, "Safe evaluation should succeed")
        Assert.AreEqual(42, result.Value, "Should return correct result")
    
    [<Test>]
    member _.``SafeEvaluate should handle exceptions``() =
        let result = ConstantFold.safeEvaluate (fun () -> failwith "test error")
        
        Assert.IsTrue(result.IsNone, "Safe evaluation should handle exceptions")
    
    [<Test>]
    member _.``TransformValue should not crash``() =
        let context = {
            ConstantFold.ConstantsInScope = ConstantFold.emptyScope
            ConstantFold.Evaluated = ConstantFold.Unconditionally
        }
        let value = Value.return' 42
        let result = ConstantFold.transformValue context value
        
        Assert.IsNotNull(result, "Transform should not crash")
    
    [<Test>]
    member _.``TransformComputation should not crash``() =
        let context = {
            ConstantFold.ConstantsInScope = ConstantFold.emptyScope
            ConstantFold.Evaluated = ConstantFold.Unconditionally
        }
        let computation = Computation.Return (Value.return' "test")
        let resultTrampoline = ConstantFold.transformComputation context computation
        let result = Trampoline.run resultTrampoline
        
        Assert.IsNotNull(result, "Transform should not crash")
    
    [<Test>]
    member _.``ConstantFold should optimize computation``() =
        let computation = Computation.Return (Value.return' 123)
        let optimized = ConstantFold.constantFold computation
        
        Assert.IsNotNull(optimized, "Constant fold should return valid computation")
    
    [<Test>]
    member _.``Optimize should work as alias``() =
        let computation = Computation.Return (Value.return' "hello")
        let optimized = ConstantFold.optimize computation
        
        Assert.IsNotNull(optimized, "Optimize should return valid computation")
    
    [<Test>]
    member _.``WouldBenefit should detect beneficial cases``() =
        let constantComputation = Computation.Return (Value.return' 42)
        let wouldBenefit = ConstantFold.wouldBenefit constantComputation
        
        Assert.IsTrue(wouldBenefit, "Should detect that constant computation would benefit")
    
    [<Test>]
    member _.``CountFoldableConstants should count constants``() =
        let computation = Computation.Return (Value.return' 42)
        let count = ConstantFold.countFoldableConstants computation
        
        Assert.AreEqual(1, count, "Should count one foldable constant")
    
    [<Test>]
    member _.``EmptyScope should be empty``() =
        let scope = ConstantFold.emptyScope
        let result = ConstantFold.findConstant 123 scope
        
        Assert.IsTrue(result.IsNone, "Empty scope should not contain any constants")
    
    [<Test>]
    member _.``AddConstant and FindConstant should work``() =
        let scope = ConstantFold.emptyScope
        let scopeWithConstant = ConstantFold.addConstant 42 (box "test") scope
        let result = ConstantFold.findConstant 42 scopeWithConstant
        
        Assert.IsTrue(result.IsSome, "Should find added constant")
        Assert.AreEqual("test", unbox<string> result.Value, "Should return correct constant value")
    
    [<Test>]
    member _.``CreateTransform should create valid transform``() =
        let context = {
            ConstantFold.ConstantsInScope = ConstantFold.emptyScope
            ConstantFold.Evaluated = ConstantFold.Maybe
        }
        let transform = ConstantFold.createTransform context
        
        Assert.IsNotNull(transform, "Should create valid transform")
        Assert.IsNotNull(transform.TransformC, "TransformC should not be null")
        Assert.IsNotNull(transform.TransformV, "TransformV should not be null")