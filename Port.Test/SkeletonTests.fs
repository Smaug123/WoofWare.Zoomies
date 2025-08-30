namespace WoofWare.Zoomies.Port.Test

open NUnit.Framework
open WoofWare.Zoomies.Port

[<TestFixture>]
type SkeletonTests() =
    
    [<Test>]
    member _.``Id toString should work``() =
        let id = Skeleton.Id.ofIntForTesting 42
        let str = Skeleton.Id.toString id
        
        Assert.IsNotEmpty(str, "ID toString should return non-empty string")
        Assert.IsTrue(str.Contains("42"), "ID toString should contain the test value")
    
    [<Test>]
    member _.``Id ofTypeId should work``() =
        let typeId = TypeId.create<int> "test"
        let id = Skeleton.Id.ofTypeId typeId
        
        Assert.IsNotNull(id, "ID from TypeId should be created successfully")
    
    [<Test>]
    member _.``Value fromValue should work``() =
        let path = Path.empty
        let value = Value.return' 42
        let skeleton = Skeleton.Value.fromValue path value
        
        Assert.IsNotNull(skeleton, "Value skeleton should be created successfully")
        Assert.AreEqual(Skeleton.Value.Constant, skeleton.Kind, "Constant value should have Constant kind")
    
    [<Test>]
    member _.``Computation fromComputation should work``() =
        let path = Path.empty
        let computation = Computation.Return (Value.return' "test")
        let skeleton = Skeleton.Computation.fromComputation path computation
        
        Assert.IsNotNull(skeleton, "Computation skeleton should be created successfully")
    
    [<Test>]
    member _.``Analyze should work``() =
        let computation = Computation.Return (Value.return' 123)
        let skeleton = Skeleton.analyze computation
        
        Assert.IsNotNull(skeleton, "Analysis should return non-null skeleton")
    
    [<Test>]
    member _.``CountNodes should work``() =
        let computation = Computation.Return (Value.return' "test")
        let skeleton = Skeleton.analyze computation
        let count = Skeleton.countNodes skeleton
        
        Assert.IsTrue(count > 0, "Node count should be positive")
    
    [<Test>]
    member _.``FindSubComputations should work``() =
        let computation = Computation.Return (Value.return' 42)
        let skeleton = Skeleton.analyze computation
        let subComputations = Skeleton.findSubComputations skeleton
        
        Assert.IsNotNull(subComputations, "SubComputations list should not be null")