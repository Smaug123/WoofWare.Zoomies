namespace WoofWare.Zoomies

open System

[<RequireQualifiedAccess>]
module private Object =

    let referenceEquals<'a when 'a : not struct> (x : 'a) (y : 'a) =
        // ANALYZER: ReferenceEquals allowed, this is our type-safe wrapper
        Object.ReferenceEquals (x, y)
