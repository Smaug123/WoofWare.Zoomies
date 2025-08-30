namespace WoofWare.Zoomies.Port.Test

open NUnit.Framework
open WoofWare.Zoomies.Port

[<TestFixture>]
type LifecycleTests() =
    
    [<Test>]
    member _.``Collection empty should create empty collection``() =
        let emptyCollection = Lifecycle.Collection.empty
        Assert.IsNotNull(emptyCollection, "Empty lifecycle collection should be created successfully")
    
    [<Test>]
    member _.``Collection merge should combine collections``() =
        let collection1 = Lifecycle.Collection.empty
        let collection2 = Lifecycle.Collection.empty
        let merged = Lifecycle.Collection.merge collection1 collection2
        
        Assert.IsNotNull(merged, "Merged lifecycle collection should be created successfully")
    
    [<Test>]
    member _.``Collection hasAfterDisplay should work on empty collection``() =
        let emptyCollection = Lifecycle.Collection.empty
        let hasAfterDisplay = Lifecycle.Collection.hasAfterDisplay emptyCollection
        
        Assert.IsFalse(hasAfterDisplay, "Empty collection should not have after display effects")