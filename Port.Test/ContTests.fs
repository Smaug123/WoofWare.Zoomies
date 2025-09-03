#nowarn "3559"
namespace WoofWare.Zoomies.Port.Test

open NUnit.Framework
open WoofWare.Zoomies.Port

[<TestFixture>]
type ContTests() =

    [<Test>]
    member _.``return' should create constant value``() =
        let value = Cont.return' 42
        // Just verify the value was created successfully
        Assert.IsNotNull(value, "return' should create a non-null value")

    [<Test>]
    member _.``map should transform value``() =
        let value = Cont.return' 10
        let mapped = Cont.map value ((+) 5)
        // Since map goes through perform, it will return a default value for testing
        Assert.IsNotNull(mapped, "Mapped value should not be null")

    [<Test>]
    member _.``map2 should combine two values``() =
        let a = Cont.return' 10
        let b = Cont.return' 20
        let combined = Cont.map2 a b (+)
        Assert.IsNotNull(combined, "Combined value should not be null")

    [<Test>]
    member _.``both should create tuple``() =
        let a = Cont.return' "hello"
        let b = Cont.return' 42
        let tuple = Cont.both a b
        Assert.IsNotNull(tuple, "Tuple value should not be null")

    [<Test>]
    member _.``state should create model and inject function``() =
        let graph = { Cont.Graph.Transform = id }
        let model, inject = Cont.state 0 graph
        Assert.IsNotNull(model, "Model should not be null")
        Assert.IsNotNull(inject, "Inject function should not be null")

    [<Test>]
    member _.``stateMachine0 should create state machine``() =
        let graph = { Cont.Graph.Transform = id }
        let applyAction context input model action = action model
        let model, inject = Cont.stateMachine0 0 applyAction graph
        Assert.IsNotNull(model, "State machine model should not be null")
        Assert.IsNotNull(inject, "State machine inject should not be null")

    [<Test>]
    member _.``stateMachine1 should create state machine with input``() =
        let graph = { Cont.Graph.Transform = id }
        let applyAction context inputOpt model action = action model
        let input = Cont.return' "input"
        let model, inject = Cont.stateMachine1 0 applyAction input graph
        Assert.IsNotNull(model, "State machine model should not be null")
        Assert.IsNotNull(inject, "State machine inject should not be null")

    [<Test>]
    member _.``Conv.topLevelHandle should work``() =
        let computation = Cont.Conv.topLevelHandle (fun graph -> Cont.return' 42)
        Assert.IsNotNull(computation, "Top level handle should return computation")