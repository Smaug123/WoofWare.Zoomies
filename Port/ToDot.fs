namespace WoofWare.Zoomies.Port

open System.Text

/// ToDot module for generating Graphviz DOT files from computation graphs  
/// Provides visualization capabilities for debugging and analysis
[<RequireQualifiedAccess>]
module ToDot =
    
    /// Unique identifier for DOT nodes
    type DotId = string
    
    /// Visual styling information for nodes
    type NodeKind =
        | Computation of string
        | Leaf  
        | Value of Kind: string * Here: GraphInfo.SourceCodePosition option
        | Subst of GraphInfo.SourceCodePosition option
        | ResetId
        | Dynamic
    
    /// State for DOT generation
    type DotState = {
        mutable NextId : int
        TypeIdToName : Map<int, DotId>
        Buffer : StringBuilder
    }
    
    /// Create initial DOT generation state
    let createState () : DotState = {
        NextId = 0
        TypeIdToName = Map.empty
        Buffer = StringBuilder()
    }
    
    /// Generate a unique DOT node identifier
    let generateId (state : DotState) (name : string) : DotId =
        let sanitized = 
            name 
            |> Seq.filter (fun c -> System.Char.IsLetterOrDigit c || c = '_')
            |> Seq.toArray
            |> System.String
        let id = sprintf "%s_%d" sanitized state.NextId
        state.NextId <- state.NextId + 1
        id
    
    /// Convert node kind to DOT styling attributes
    let nodeKindToStyle (kind : NodeKind) : string =
        match kind with
        | Computation name ->
            sprintf """[ style=filled, shape = "Mrecord", label = "%s", fillcolor = "#86E3CE" ]""" name
        | Leaf ->
            sprintf """[ style=filled, shape = "Mrecord", label = "{state machine}", fillcolor = "#D0E6A5" ]"""
        | Value (valueKind, here) ->
            let tooltip = 
                match here with
                | Some pos -> sprintf """, tooltip = "%s:%d" """ pos.FileName pos.LineNumber
                | None -> ""
            sprintf """[ style=filled%s, shape = "oval", label = "%s", fillcolor = "#FFDD94" ]""" tooltip valueKind
        | Subst here ->
            let tooltip = 
                match here with
                | Some pos -> sprintf """, tooltip = "%s:%d" """ pos.FileName pos.LineNumber
                | None -> ""
            sprintf """[ style=filled%s, shape = "oval", label = "subst", fillcolor = "#FFFFFF", width=.1, height=.1 ]""" tooltip
        | ResetId | Dynamic ->
            sprintf """[ style=filled, shape = "circle", label = "", fillcolor = "#000000", width=.1, height=.1 ]"""
    
    /// Add a node to the DOT output
    let addNode (state : DotState) (nodeId : DotId) (kind : NodeKind) : unit =
        let style = nodeKindToStyle kind
        state.Buffer.AppendLine(sprintf "    %s %s;" nodeId style) |> ignore
    
    /// Add an edge to the DOT output
    let addEdge (state : DotState) (fromId : DotId) (toId : DotId) : unit =
        state.Buffer.AppendLine(sprintf "    %s -> %s;" fromId toId) |> ignore
    
    /// Convert a Value to DOT representation
    let valueToNode (state : DotState) (value : Value<'a>) : DotId =
        let nodeInfo = GraphInfo.nodeInfoOfValue value
        let nodeId = generateId state nodeInfo.NodeType
        let kind = Value (nodeInfo.NodeType, nodeInfo.Here)
        addNode state nodeId kind
        nodeId
    
    /// Convert a Computation to DOT representation
    let computationToNode (state : DotState) (computation : 'a Computation) : DotId =
        let nodeInfo = GraphInfo.nodeInfoOfComputation computation
        let nodeId = generateId state nodeInfo.NodeType
        let kind = Computation nodeInfo.NodeType
        addNode state nodeId kind
        
        // Add edges for computation dependencies
        match computation with
        | Computation.Return value ->
            let valueId = valueToNode state value
            addEdge state nodeId valueId
        | _ -> () // TODO: Add other computation types
        
        nodeId
    
    /// Generate DOT header
    let generateHeader (state : DotState) (graphName : string) : unit =
        state.Buffer.AppendLine(sprintf "digraph %s {" graphName) |> ignore
        state.Buffer.AppendLine("    rankdir = TB;") |> ignore
        state.Buffer.AppendLine("    node [fontname = \"Arial\"];") |> ignore
    
    /// Generate DOT footer
    let generateFooter (state : DotState) : unit =
        state.Buffer.AppendLine("}") |> ignore
    
    /// Convert a computation graph to DOT format
    let computationToDot<'a> (computation : 'a Computation) : string =
        let state = createState ()
        generateHeader state "bonsai_graph"
        
        let _ = computationToNode state computation
        
        generateFooter state
        state.Buffer.ToString()
    
    /// Convert a graph info structure to DOT format
    let graphInfoToDot (graphInfo : GraphInfo.GraphInfo) : string =
        let state = createState ()
        generateHeader state "bonsai_graph_info"
        
        // Add all nodes
        for kvp in graphInfo.Info do
            let nodeId = generateId state kvp.Value.NodeType
            let kind = Value (kvp.Value.NodeType, kvp.Value.Here)
            addNode state nodeId kind
        
        // Add all edges from the tree structure
        for kvp in graphInfo.Tree do
            let fromPath = kvp.Key
            let toPath = kvp.Value
            if fromPath <> toPath then // Don't add self-loops
                let fromId = sprintf "path_%d" (fromPath.GetHashCode()) // Simplified path representation
                let toId = sprintf "path_%d" (toPath.GetHashCode())
                addEdge state fromId toId
        
        generateFooter state
        state.Buffer.ToString()
    
    /// Save DOT content to a file
    let saveDotToFile (filename : string) (dotContent : string) : unit =
        System.IO.File.WriteAllText(filename, dotContent)
    
    /// Generate and save computation graph to DOT file
    let saveComputationAsDot<'a> (filename : string) (computation : 'a Computation) : unit =
        let dotContent = computationToDot computation
        saveDotToFile filename dotContent
    
    /// Create a simple example DOT graph
    let createExample () : string =
        let state = createState ()
        generateHeader state "example"
        
        let rootId = generateId state "root"
        let childId = generateId state "child"
        
        addNode state rootId (Computation "example_root")
        addNode state childId (Value ("constant", None))
        addEdge state rootId childId
        
        generateFooter state
        state.Buffer.ToString()