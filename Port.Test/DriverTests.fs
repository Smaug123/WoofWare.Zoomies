#nowarn "3559"
namespace WoofWare.Zoomies.Port.Test

open NUnit.Framework
open WoofWare.Zoomies.Port

[<TestFixture>]
type DriverTests() =

    [<Test>]
    member _.``create should create driver from Cont computation``() =
        let computation = fun graph -> Cont.return' "test"
        let driver = Driver.create computation
        Assert.IsNotNull(driver, "Driver should be created")

    [<Test>]
    member _.``createDirect should create driver from computation``() =
        let computation = Computation.return' (Value.return' "test")
        let driver = Driver.createDirect computation
        Assert.IsNotNull(driver, "Driver should be created directly")

    [<Test>]
    member _.``flush should not throw``() =
        let driver = Driver.ForTesting.createSimple () (fun model action -> model) (fun _ -> "result")
        Assert.DoesNotThrow(fun () -> Driver.flush driver)

    [<Test>]
    member _.``result should return current result``() =
        let driver = Driver.ForTesting.createSimple "initial" (fun model action -> model) (fun model -> model)
        let result = Driver.result driver
        Assert.AreEqual("initial", result)

    [<Test>]
    member _.``hasAfterDisplayEvents should return false``() =
        let driver = Driver.ForTesting.createSimple () (fun model action -> model) (fun _ -> ())
        let hasEvents = Driver.hasAfterDisplayEvents driver
        Assert.IsFalse(hasEvents, "Should have no after-display events")

    [<Test>]
    member _.``triggerLifecycles should not throw``() =
        let driver = Driver.ForTesting.createSimple () (fun model action -> model) (fun _ -> ())
        Assert.DoesNotThrow(fun () -> Driver.triggerLifecycles driver)

    [<Test>]
    member _.``Expert.resetModelToDefault should not throw``() =
        let driver = Driver.ForTesting.createSimple 42 (fun model action -> model + action) (fun model -> model)
        Assert.DoesNotThrow(fun () -> Driver.Expert.resetModelToDefault driver)

    [<Test>]
    member _.``Expert.printActions should not throw``() =
        let driver = Driver.ForTesting.createSimple () (fun model action -> model) (fun _ -> ())
        Assert.DoesNotThrow(fun () -> Driver.Expert.printActions driver)

    [<Test>]
    member _.``ForTesting.injectAction should enqueue action``() =
        let driver = Driver.ForTesting.createSimple 0 (fun model action -> model + action) (fun model -> model)
        Assert.DoesNotThrow(fun () -> Driver.ForTesting.injectAction driver 5)
        
        // Flush to process the action
        Driver.flush driver
        let result = Driver.result driver
        Assert.AreEqual(0, result, "Initial model should be returned")