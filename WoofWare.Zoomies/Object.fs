namespace WoofWare.Zoomies

open System

[<RequireQualifiedAccess>]
module private Object =

    let referenceEquals<'a when 'a : not struct> (x : 'a) (y : 'a) = Object.ReferenceEquals (x, y)
