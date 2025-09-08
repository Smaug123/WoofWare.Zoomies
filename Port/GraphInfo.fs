namespace WoofWare.Zoomies.Port

/// GraphInfo module for analyzing and tracking computation graph structure
/// Provides debugging and introspection capabilities for Bonsai graphs
[<RequireQualifiedAccess>]
module GraphInfo =
    
    /// Source code position information (simplified version of OCaml's Source_code_position)
    type SourceCodePosition = {
        FileName : string
        LineNumber : int
        BeginOfLine : int  
        CharacterNumber : int
    }
    
    /// Information about a single node in the computation graph
    type NodeInfo = {
        NodeType : string
        Here : SourceCodePosition option
    }
    
    /// Complete graph structure with tree relationships, DAG edges, and node metadata
    type GraphInfo = {
        /// Tree structure: each node points to its parent
        Tree : Map<Path.Path, Path.Path>
        /// DAG structure: each node points to list of children/dependents
        Dag : Map<Path.Path, Path.Path list>
        /// Node metadata: type and source location information
        Info : Map<Path.Path, NodeInfo>
    }
    
    /// Empty graph info
    let empty : GraphInfo = {
        Tree = Map.empty
        Dag = Map.empty  
        Info = Map.empty
    }
    
    /// Extract node information from a Value
    let nodeInfoOfValue<'a> (value : Value<'a>) : NodeInfo =
        let nodeType =
            match value.WithoutPosition with
            | ValueWithoutPosition.Constant _ -> "constant"
            | ValueWithoutPosition.Exception _ -> "exception"
            | ValueWithoutPosition.Incr _ -> "incr"
            | ValueWithoutPosition.Named _ -> "named"
        
        // TODO: Add 'Here' information to Value type
        { NodeType = nodeType; Here = None }
    
    /// Extract node information from a Computation
    let nodeInfoOfComputation<'a> (computation : 'a Computation) : NodeInfo =
        let nodeType =
            match computation with
            | Computation.Return _ -> "return"
            | _ -> "unknown_computation" // TODO: Add other computation types
        
        // TODO: Extract 'here' information from computation
        { NodeType = nodeType; Here = None }
    
    /// Add a tree relationship between two nodes
    let addTreeRelationship (from : Path.Path) (to_ : Path.Path) (nodeInfo : NodeInfo) (graphInfo : GraphInfo) : GraphInfo =
        { graphInfo with
            Tree = Map.add from to_ graphInfo.Tree
            Info = Map.add from nodeInfo graphInfo.Info }
    
    /// Add a DAG relationship (dependency edge) between two nodes
    let addDagRelationship (from : Path.Path) (to_ : Path.Path) (graphInfo : GraphInfo) : GraphInfo =
        let existingDeps = Map.tryFind from graphInfo.Dag |> Option.defaultValue []
        let newDeps = to_ :: existingDeps
        { graphInfo with Dag = Map.add from newDeps graphInfo.Dag }
    
    /// Analyze a computation graph and extract structural information
    let analyzeComputation<'a> (computation : 'a Computation) : GraphInfo =
        // TODO: Implement full graph traversal with Transform module
        // For now, create basic info for the root node
        let rootPath = Path.empty
        let rootInfo = nodeInfoOfComputation computation
        addTreeRelationship rootPath rootPath rootInfo empty
    
    /// Pull source code locations from nearest parent nodes
    /// This fills in missing location info by propagating from parents
    let pullSourceLocationsFromNearestParent (graphInfo : GraphInfo) : Map<Path.Path, NodeInfo> =
        let mutableInfo = ref graphInfo.Info
        
        let rec findAndUpdateNearestHere (key : Path.Path) : SourceCodePosition option =
            match Map.tryFind key !mutableInfo with
            | None -> None
            | Some nodeInfo ->
                match nodeInfo.Here with
                | Some here -> Some here
                | None ->
                    match Map.tryFind key graphInfo.Tree with
                    | None -> None
                    | Some parent ->
                        match findAndUpdateNearestHere parent with
                        | None -> None
                        | Some parentHere ->
                            // Mark inherited locations with "~" prefix
                            let inheritedHere = { parentHere with FileName = "~" + parentHere.FileName }
                            let updatedInfo = { nodeInfo with Here = Some inheritedHere }
                            mutableInfo := Map.add key updatedInfo !mutableInfo
                            Some inheritedHere
        
        // Update all nodes
        for kvp in graphInfo.Info do
            let _ = findAndUpdateNearestHere kvp.Key
            ()
        
        !mutableInfo
    
    /// Create a simple graph info for testing
    let createSample () : GraphInfo =
        let path1 = Path.empty
        let path2 = Path.append path1 Path.Elem.Elem.SubstFrom // Use an actual Elem
        let info1 = { NodeType = "root"; Here = None }
        let info2 = { NodeType = "child"; Here = None }
        
        empty
        |> addTreeRelationship path1 path1 info1
        |> addTreeRelationship path2 path1 info2
        |> addDagRelationship path1 path2
    
    /// Get all nodes in the graph
    let getAllNodes (graphInfo : GraphInfo) : Path.Path list =
        graphInfo.Info |> Map.keys |> List.ofSeq
    
    /// Get children of a specific node
    let getChildren (path : Path.Path) (graphInfo : GraphInfo) : Path.Path list =
        Map.tryFind path graphInfo.Dag |> Option.defaultValue []
    
    /// Get parent of a specific node
    let getParent (path : Path.Path) (graphInfo : GraphInfo) : Path.Path option =
        Map.tryFind path graphInfo.Tree
    
    /// Get node info for a specific path
    let getNodeInfo (path : Path.Path) (graphInfo : GraphInfo) : NodeInfo option =
        Map.tryFind path graphInfo.Info