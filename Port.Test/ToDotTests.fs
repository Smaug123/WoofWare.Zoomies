namespace WoofWare.Zoomies.Port.Test

open NUnit.Framework
open WoofWare.Zoomies.Port
open System.Text

[<TestFixture>]
type ToDotTests() =
    
    [<Test>]
    member _.``CreateState should create valid state``() =
        let state = ToDot.createState ()
        
        Assert.IsNotNull(state, "State should not be null")
        Assert.AreEqual(0, state.NextId, "Initial ID should be 0")
        Assert.IsNotNull(state.Buffer, "Buffer should not be null")
    
    [<Test>]
    member _.``GenerateId should create unique IDs``() =
        let state = ToDot.createState ()
        let id1 = ToDot.generateId state "test"
        let id2 = ToDot.generateId state "test"
        
        Assert.AreNotEqual(id1, id2, "Generated IDs should be unique")
        Assert.IsTrue(id1.Contains("test"), "ID should contain the base name")
        Assert.IsTrue(id2.Contains("test"), "ID should contain the base name")
    
    [<Test>]
    member _.``NodeKindToStyle should generate valid DOT styling``() =
        let computationStyle = ToDot.nodeKindToStyle (ToDot.Computation "test_comp")
        let leafStyle = ToDot.nodeKindToStyle ToDot.Leaf
        let valueStyle = ToDot.nodeKindToStyle (ToDot.Value ("constant", None))
        
        Assert.IsTrue(computationStyle.Contains("test_comp"), "Computation style should contain name")
        Assert.IsTrue(leafStyle.Contains("state machine"), "Leaf style should contain label")
        Assert.IsTrue(valueStyle.Contains("constant"), "Value style should contain kind")
        Assert.IsTrue(computationStyle.Contains("#86E3CE"), "Should contain computation color")
    
    [<Test>]
    member _.``AddNode should add node to buffer``() =
        let state = ToDot.createState ()
        let nodeId = "test_node"
        let kind = ToDot.Value ("constant", None)
        
        ToDot.addNode state nodeId kind
        let output = state.Buffer.ToString()
        
        Assert.IsTrue(output.Contains("test_node"), "Output should contain node ID")
        Assert.IsTrue(output.Contains("constant"), "Output should contain node kind")
    
    [<Test>]
    member _.``AddEdge should add edge to buffer``() =
        let state = ToDot.createState ()
        
        ToDot.addEdge state "node1" "node2"
        let output = state.Buffer.ToString()
        
        Assert.IsTrue(output.Contains("node1 -> node2"), "Output should contain edge")
    
    [<Test>]
    member _.``ValueToNode should create valid node``() =
        let state = ToDot.createState ()
        let value = Value.return' 42
        
        let nodeId = ToDot.valueToNode state value
        let output = state.Buffer.ToString()
        
        Assert.IsNotNull(nodeId, "Node ID should not be null")
        Assert.IsTrue(output.Contains("constant"), "Output should contain value type")
    
    [<Test>]
    member _.``ComputationToNode should create valid node``() =
        let state = ToDot.createState ()
        let computation = Computation.Return (Value.return' "test")
        
        let nodeId = ToDot.computationToNode state computation
        let output = state.Buffer.ToString()
        
        Assert.IsNotNull(nodeId, "Node ID should not be null")
        Assert.IsTrue(output.Contains("return"), "Output should contain computation type")
    
    [<Test>]
    member _.``ComputationToDot should generate complete DOT``() =
        let computation = Computation.Return (Value.return' 123)
        let dotContent = ToDot.computationToDot computation
        
        Assert.IsTrue(dotContent.Contains("digraph"), "Should contain digraph declaration")
        Assert.IsTrue(dotContent.Contains("{"), "Should contain opening brace")
        Assert.IsTrue(dotContent.Contains("}"), "Should contain closing brace")
        Assert.IsTrue(dotContent.Contains("return"), "Should contain computation info")
    
    [<Test>]
    member _.``GraphInfoToDot should generate DOT from GraphInfo``() =
        let graphInfo = GraphInfo.createSample ()
        let dotContent = ToDot.graphInfoToDot graphInfo
        
        Assert.IsTrue(dotContent.Contains("digraph"), "Should contain digraph declaration")
        Assert.IsTrue(dotContent.Contains("bonsai_graph_info"), "Should contain graph name")
        Assert.IsTrue(dotContent.Contains("{"), "Should contain opening brace")
        Assert.IsTrue(dotContent.Contains("}"), "Should contain closing brace")
    
    [<Test>]
    member _.``CreateExample should generate example DOT``() =
        let exampleDot = ToDot.createExample ()
        
        Assert.IsTrue(exampleDot.Contains("digraph example"), "Should contain example graph")
        Assert.IsTrue(exampleDot.Contains("root"), "Should contain root node")
        Assert.IsTrue(exampleDot.Contains("child"), "Should contain child node")
        Assert.IsTrue(exampleDot.Contains("->"), "Should contain edge")
    
    [<Test>]
    member _.``GenerateHeader should create proper DOT header``() =
        let state = ToDot.createState ()
        
        ToDot.generateHeader state "test_graph"
        let output = state.Buffer.ToString()
        
        Assert.IsTrue(output.Contains("digraph test_graph"), "Should contain graph declaration")
        Assert.IsTrue(output.Contains("rankdir"), "Should contain layout direction")
        Assert.IsTrue(output.Contains("fontname"), "Should contain font specification")
    
    [<Test>]
    member _.``GenerateFooter should close DOT properly``() =
        let state = ToDot.createState ()
        
        ToDot.generateFooter state
        let output = state.Buffer.ToString()
        
        Assert.IsTrue(output.Contains("}"), "Should contain closing brace")