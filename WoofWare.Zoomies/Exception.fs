namespace WoofWare.Zoomies

open System.Runtime.ExceptionServices

[<RequireQualifiedAccess>]
module internal Exception =

    let reraiseWithOriginalStackTrace (exc : exn) : 'a =
        let edi = ExceptionDispatchInfo.Capture exc
        edi.Throw ()
        failwith "unreachable"
