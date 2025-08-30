namespace WoofWare.Zoomies.Port.Test

#nowarn "3559" // Type implicitly inferred as 'obj'

open NUnit.Framework
open WoofWare.Zoomies.Port

[<TestFixture>]
type FixTransformTests() =
    
    [<Test>]
    member _.``Unit Up should work``() =
        let up = FixTransform.Unit.up
        let combined = up.Combine () ()
        Assert.AreEqual((), combined, "Unit Up combine should return unit")
        Assert.AreEqual((), up.Empty, "Unit Up empty should be unit")
        Assert.AreEqual((), up.EmptyForLazy, "Unit Up empty for lazy should be unit")
    
    [<Test>]
    member _.``CreatePassThrough should create valid transform``() =
        let up = FixTransform.Unit.up
        let transform = FixTransform.createPassThrough up
        
        Assert.IsNotNull(transform, "Transform should not be null")
        Assert.IsNotNull(transform.TransformC, "TransformC should not be null")
        Assert.IsNotNull(transform.TransformV, "TransformV should not be null")
    
    [<Test>]
    member _.``IdentityTransform should work``() =
        let transform = FixTransform.identityTransform ()
        
        Assert.IsNotNull(transform, "Identity transform should not be null")
        Assert.IsNotNull(transform.TransformC, "TransformC should not be null")
        Assert.IsNotNull(transform.TransformV, "TransformV should not be null")
    
    [<Test>]
    member _.``TransformComputation should not crash``() =
        let transform = FixTransform.identityTransform ()
        let computation = box (Computation.Return (Value.return' 42))
        
        let result = FixTransform.transformComputation transform () () computation
        let resultValue = Trampoline.run result
        
        Assert.IsNotNull(resultValue, "Transform result should not be null")
    
    [<Test>]
    member _.``TransformValue should not crash``() =
        let transform = FixTransform.identityTransform ()
        let value = box (Value.return' "test")
        
        let result = FixTransform.transformValue transform () () value
        
        Assert.IsNotNull(result, "Transform result should not be null")
    
    [<Test>]
    member _.``Up with int should work``() =
        let intUp : FixTransform.Up<int> = {
            Combine = fun a b -> a + b
            Empty = 0
            EmptyForLazy = -1
        }
        
        let combined = intUp.Combine 5 3
        Assert.AreEqual(8, combined, "Int Up combine should add values")
        Assert.AreEqual(0, intUp.Empty, "Int Up empty should be 0")
        Assert.AreEqual(-1, intUp.EmptyForLazy, "Int Up empty for lazy should be -1")