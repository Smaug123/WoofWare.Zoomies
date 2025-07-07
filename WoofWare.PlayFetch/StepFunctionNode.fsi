namespace WoofWare.PlayFetch

[<RequireQualifiedAccess>]
module internal StepFunctionNode =
    val advance : StepFunctionNode<'a> -> TimeNs -> unit
