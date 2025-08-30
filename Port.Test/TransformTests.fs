namespace WoofWare.Zoomies.Port.Test

open NUnit.Framework
open WoofWare.Zoomies.Port

[<TestFixture>]
type TransformTests() =
    
    [<Test>]
    member _.``VarFromParent types should work``() =
        let none = Transform.VarFromParent.None
        let one = Transform.VarFromParent.One 42
        let two = Transform.VarFromParent.Two (1, 2)
        
        Assert.IsNotNull(none, "None should be valid")
        Assert.IsNotNull(one, "One should be valid")
        Assert.IsNotNull(two, "Two should be valid")
    
    [<Test>]
    member _.``ForValue transform should not crash``() =
        let mapper : Transform.ForValue.UserMapper<unit, int> = {
            F = fun context parent value -> value
        }
        let value = Value.return' 42
        let result = Transform.ForValue.transform mapper () value
        
        Assert.IsNotNull(result, "Transform should return non-null value")
    
    [<Test>]
    member _.``ForComputation transform should not crash``() =
        let mapper : Transform.ForComputation.UserMapper<unit, string> = {
            F = fun context parent computation -> computation
        }
        let computation = Computation.Return (Value.return' "test")
        let result = Transform.ForComputation.transform mapper () computation
        
        Assert.IsNotNull(result, "Transform should return non-null computation")
    
    [<Test>]
    member _.``MapValues should not crash``() =
        let f (value : Value<int>) : Value<int> = value
        let computation = Computation.Return (Value.return' 42)
        let result = Transform.mapValues f computation
        
        Assert.IsNotNull(result, "MapValues should return non-null computation")
    
    [<Test>]
    member _.``MapComputations should not crash``() =
        let f (comp : int Computation) : int Computation = comp
        let computation = Computation.Return (Value.return' 42)
        let result = Transform.mapComputations f computation
        
        Assert.IsNotNull(result, "MapComputations should return non-null computation")
    
    [<Test>]
    member _.``CountNodes should work``() =
        let computation = Computation.Return (Value.return' "test")
        let count = Transform.countNodes computation
        
        Assert.IsTrue(count > 0, "Node count should be positive")
    
    [<Test>]
    member _.``FindValues should work``() =
        let predicate (value : Value<int>) : bool = true
        let computation = Computation.Return (Value.return' 123)
        let values = Transform.findValues predicate computation
        
        Assert.IsNotNull(values, "FindValues should return non-null list")