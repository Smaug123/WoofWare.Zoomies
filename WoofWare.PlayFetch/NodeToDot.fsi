namespace WoofWare.PlayFetch

[<RequireQualifiedAccess>]
module NodeToDot =
    val saveDot : emitBindEdges:bool -> Format.formatter -> NodeCrate list -> unit
    val saveDotToFile : emitBindEdges:bool -> string -> NodeCrate list -> unit
