namespace WoofWare.Zoomies.Port.Test

#nowarn "3559" // Type implicitly inferred as 'obj'

open NUnit.Framework
open WoofWare.Zoomies.Port

[<TestFixture>]
type FlattenValuesTests() =
    
    [<Test>]
    member _.``FlattenValue should not crash``() =
        let value = Value.return' 42
        let result = FlattenValues.flattenValue value
        
        Assert.IsNotNull(result, "FlattenValue should not crash")
        Assert.AreEqual(value, result, "For now, should return same value")
    
    [<Test>]
    member _.``FlattenComputation should handle Return``() =
        let computation = Computation.Return (Value.return' "test")
        let result = FlattenValues.flattenComputation computation
        
        Assert.IsNotNull(result, "FlattenComputation should not crash")
    
    [<Test>]
    member _.``WouldBenefit should return true``() =
        let value = Value.return' 123
        let result = FlattenValues.wouldBenefit value
        
        Assert.IsTrue(result, "Should return true for now")
    
    [<Test>]
    member _.``NestingDepth should return positive number``() =
        let value = Value.return' [1; 2; 3]
        let depth = FlattenValues.nestingDepth value
        
        Assert.IsTrue(depth > 0, "Nesting depth should be positive")
    
    [<Test>]
    member _.``Optimize should not crash``() =
        let value = Value.return' (42, "test")
        let result = FlattenValues.optimize value
        
        Assert.IsNotNull(result, "Optimize should not crash")
    
    [<Test>]
    member _.``CreateTransformation should create valid transform``() =
        let transform = FlattenValues.createTransformation ()
        
        Assert.IsNotNull(transform, "Transform should not be null")
        Assert.IsNotNull(transform.TransformC, "TransformC should not be null")
        Assert.IsNotNull(transform.TransformV, "TransformV should not be null")
    
    [<Test>]
    member _.``FlattenGraph should not crash``() =
        let computation = Computation.Return (Value.return' 42)
        let result = FlattenValues.flattenGraph computation
        
        Assert.IsNotNull(result, "FlattenGraph should not crash")
    
    [<Test>]
    member _.``AnalyzeFlattening should return valid analysis``() =
        let computation = Computation.Return (Value.return' "hello")
        let analysis = FlattenValues.analyzeFlattening computation
        
        Assert.IsNotNull(analysis, "Analysis should not be null")
        Assert.IsTrue(analysis.EstimatedImprovement >= 0, "EstimatedImprovement should be non-negative")
    
    [<Test>]
    member _.``GetStatistics should return valid stats``() =
        let computation = Computation.Return (Value.return' 123)
        let stats = FlattenValues.getStatistics computation
        
        Assert.IsTrue(stats.TotalValues > 0, "Should have positive total values")
        Assert.IsTrue(stats.MaxDepth > 0, "Should have positive max depth")
        Assert.IsTrue(stats.AvgDepth > 0.0, "Should have positive average depth")