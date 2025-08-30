namespace WoofWare.Zoomies.Port.Test

open NUnit.Framework
open WoofWare.Zoomies.Port

[<TestFixture>]
type ValueTests() =
    
    [<Test>]
    member _.``Return value should create successfully``() =
        let value = Value.return' 42
        Assert.IsNotNull(value, "Value should be created successfully")
    
    [<Test>]
    member _.``Map should transform value type``() =
        let intValue = Value.return' 42
        let stringValue = Value.map string intValue
        Assert.IsNotNull(stringValue, "Mapped value should be created successfully")
    
    [<Test>]
    member _.``Map2 should combine two values``() =
        let value1 = Value.return' 10
        let value2 = Value.return' 20
        let combined = Value.map2 (+) value1 value2
        Assert.IsNotNull(combined, "Combined value should be created successfully")
    
    [<Test>]
    member _.``Both should create tuple value``() =
        let value1 = Value.return' "hello"
        let value2 = Value.return' 42
        let tuple = Value.both value1 value2
        Assert.IsNotNull(tuple, "Tuple value should be created successfully")
    
    [<Test>]
    member _.``Cutoff should create cutoff value``() =
        let value = Value.return' 42
        let cutoffValue = Value.cutoff (=) value
        Assert.IsNotNull(cutoffValue, "Cutoff value should be created successfully")
    
    [<Test>]
    member _.``Named value should create successfully``() =
        let nameSource = NameSource.Sub None
        let namedValue : Value<int> = Value.named nameSource
        Assert.IsNotNull(namedValue, "Named value should be created successfully")
    
    [<Test>]
    member _.``Exception value should create successfully``() =
        let exn = System.Exception("test exception")
        let exceptionValue : Value<int> = Value.returnExn exn
        Assert.IsNotNull(exceptionValue, "Exception value should be created successfully")
    
    [<Test>]
    member _.``FromIncr should create value from incremental node``() =
        runIncrementalTest (fun incr ->
            let node = incr.Return 42
            let value = Value.fromIncr node
            Assert.IsNotNull(value, "Value from incremental should be created successfully")
        )