namespace WoofWare.Zoomies.Port.Test

open NUnit.Framework
open WoofWare.Zoomies.Port

[<TestFixture>]
type VarTests() =
    
    [<Test>]
    member _.``Create var should work``() =
        let var = Var.create 42
        Assert.IsNotNull(var, "Variable should be created successfully")
    
    [<Test>]
    member _.``Get should return current value``() =
        let var = Var.create 42
        let value = Var.get var
        Assert.AreEqual(42, value, "Variable should return its current value")
    
    [<Test>]
    member _.``Set should update value``() =
        let var = Var.create 42
        Var.set var 100
        let newValue = Var.get var
        Assert.AreEqual(100, newValue, "Variable should be updated with new value")
    
    [<Test>]
    member _.``Update should modify value using function``() =
        let var = Var.create 10
        Var.update var (fun x -> x * 2)
        let newValue = Var.get var
        Assert.AreEqual(20, newValue, "Variable should be updated using the update function")
    
    [<Test>]
    member _.``Value should create Value from Var``() =
        let var = Var.create "hello"
        let value = Var.value var
        Assert.IsNotNull(value, "Value should be created from variable")
    
    [<Test>]
    member _.``IncrVar should return the underlying incremental var``() =
        let var = Var.create 123
        let incrVar = Var.incrVar var
        Assert.AreSame(var, incrVar, "IncrVar should return the same variable")
    
    [<Test>]
    member _.``Multiple set operations should work``() =
        let var = Var.create 1
        
        Var.set var 10
        Assert.AreEqual(10, Var.get var, "First set should work")
        
        Var.set var 20
        Assert.AreEqual(20, Var.get var, "Second set should work")
        
        Var.set var 30
        Assert.AreEqual(30, Var.get var, "Third set should work")
    
    [<Test>]
    member _.``Update with complex function should work``() =
        let var = Var.create [1; 2; 3]
        Var.update var (fun list -> 0 :: list)
        let newValue = Var.get var
        Assert.AreEqual([0; 1; 2; 3], newValue, "Update should prepend element to list")