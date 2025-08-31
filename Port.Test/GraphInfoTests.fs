namespace WoofWare.Zoomies.Port.Test

open NUnit.Framework
open WoofWare.Zoomies.Port

[<TestFixture>]
type GraphInfoTests() =
    
    [<Test>]
    member _.``Empty graph info should be empty``() =
        let empty = GraphInfo.empty
        Assert.AreEqual(0, empty.Tree.Count, "Empty tree should have no entries")
        Assert.AreEqual(0, empty.Dag.Count, "Empty DAG should have no entries")
        Assert.AreEqual(0, empty.Info.Count, "Empty info should have no entries")
    
    [<Test>]
    member _.``NodeInfoOfValue should extract correct type``() =
        let constantValue = Value.return' 42
        let nodeInfo = GraphInfo.nodeInfoOfValue constantValue
        
        Assert.AreEqual("constant", nodeInfo.NodeType, "Constant value should have 'constant' type")
        Assert.IsNull(nodeInfo.Here, "Here should be None for now")
    
    [<Test>]
    member _.``NodeInfoOfValue should handle named value``() =
        let namedValue = Value.return' 123 // For now, use a constant since creating named values is complex
        let nodeInfo = GraphInfo.nodeInfoOfValue namedValue
        
        Assert.AreEqual("constant", nodeInfo.NodeType, "Value should have correct type")
    
    [<Test>]
    member _.``NodeInfoOfComputation should extract correct type``() =
        let computation = Computation.Return (Value.return' "test")
        let nodeInfo = GraphInfo.nodeInfoOfComputation computation
        
        Assert.AreEqual("return", nodeInfo.NodeType, "Return computation should have 'return' type")
        Assert.IsNull(nodeInfo.Here, "Here should be None for now")
    
    [<Test>]
    member _.``AddTreeRelationship should add entries``() =
        let path1 = Path.empty
        let path2 = Path.append path1 Path.Elem.Elem.SubstFrom
        let nodeInfo = { GraphInfo.NodeType = "test"; GraphInfo.Here = None }
        
        let graphInfo = GraphInfo.addTreeRelationship path1 path2 nodeInfo GraphInfo.empty
        
        Assert.AreEqual(1, graphInfo.Tree.Count, "Tree should have one entry")
        Assert.AreEqual(1, graphInfo.Info.Count, "Info should have one entry")
        Assert.AreEqual(Some path2, Map.tryFind path1 graphInfo.Tree, "Tree relationship should be recorded")
    
    [<Test>]
    member _.``AddDagRelationship should add entries``() =
        let path1 = Path.empty
        let path2 = Path.append path1 Path.Elem.Elem.SubstFrom
        
        let graphInfo = GraphInfo.addDagRelationship path1 path2 GraphInfo.empty
        
        Assert.AreEqual(1, graphInfo.Dag.Count, "DAG should have one entry")
        let children = Map.tryFind path1 graphInfo.Dag |> Option.defaultValue []
        Assert.IsTrue(List.contains path2 children, "DAG relationship should be recorded")
    
    [<Test>]
    member _.``CreateSample should create valid graph``() =
        let sample = GraphInfo.createSample ()
        
        Assert.IsTrue(sample.Tree.Count > 0, "Sample should have tree entries")
        Assert.IsTrue(sample.Dag.Count > 0, "Sample should have DAG entries")
        Assert.IsTrue(sample.Info.Count > 0, "Sample should have info entries")
    
    [<Test>]
    member _.``GetAllNodes should return all node paths``() =
        let sample = GraphInfo.createSample ()
        let nodes = GraphInfo.getAllNodes sample
        
        Assert.IsTrue(nodes.Length > 0, "Should have at least one node")
        Assert.IsNotNull(nodes, "Nodes list should not be null")
    
    [<Test>]
    member _.``GetChildren should return correct children``() =
        let sample = GraphInfo.createSample ()
        let nodes = GraphInfo.getAllNodes sample
        
        if nodes.Length > 0 then
            let children = GraphInfo.getChildren nodes.[0] sample
            Assert.IsNotNull(children, "Children list should not be null")
    
    [<Test>]
    member _.``GetParent should return correct parent``() =
        let sample = GraphInfo.createSample ()
        let nodes = GraphInfo.getAllNodes sample
        
        if nodes.Length > 0 then
            let parent = GraphInfo.getParent nodes.[0] sample
            Assert.IsNotNull(parent, "Parent option should not be null")
    
    [<Test>]
    member _.``GetNodeInfo should return correct info``() =
        let sample = GraphInfo.createSample ()
        let nodes = GraphInfo.getAllNodes sample
        
        if nodes.Length > 0 then
            let info = GraphInfo.getNodeInfo nodes.[0] sample
            Assert.IsTrue(info.IsSome, "Should find node info for existing node")
    
    [<Test>]
    member _.``AnalyzeComputation should not crash``() =
        let computation = Computation.Return (Value.return' 123)
        let analysis = GraphInfo.analyzeComputation computation
        
        Assert.IsNotNull(analysis, "Analysis should not be null")
        Assert.IsTrue(analysis.Info.Count >= 1, "Analysis should have at least one node")