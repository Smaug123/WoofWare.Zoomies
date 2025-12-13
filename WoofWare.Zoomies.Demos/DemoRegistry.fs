namespace WoofWare.Zoomies.Demos

open WoofWare.Zoomies
open WoofWare.Zoomies.Demos.DemoDefinitions

/// Registry of all available demos, mapping names to their run functions.
[<RequireQualifiedAccess>]
module DemoRegistry =

    /// All available demos, keyed by their command-line name.
    let demos : Map<string, (string -> string option) -> AppHandle> =
        Map.ofList [ "checkbox-toggle", CheckboxToggle.run ]

    /// List all available demo names.
    let names : string list = demos |> Map.keys |> Seq.toList |> List.sort
