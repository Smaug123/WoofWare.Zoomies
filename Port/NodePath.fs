// Human reviewed
namespace WoofWare.Zoomies.Port

/// Represents the shortest unambiguous path through the Computation.t data
/// structure, including the path through any contained Value.t's. Several of
/// the constructors in Computation.t only contain a single inner computation
/// or value; thus, keeping track of these segments in all the paths is often
/// unnecessary. Thus, we can more properly model this path as with two parts:
/// - the list of choices made; nodes which do not require a choice do not make
///   it into the list.
/// - a number representing the amount of nodes descended into after the last
///   choice point.
/// Whenever a choice point gets added, we reset the number of nodes descended
/// back to 0, since that number is no longer helpful for keeping the paths
/// unique.

type NodePath = string

[<RequireQualifiedAccess>]
module NodePath =

    /// Builder for constructing node paths incrementally
    type Builder =
        {
            Choices : int list
            Depth : int
        }

    let toString (builder : Builder) : string =
        let buffer = System.Text.StringBuilder (10)

        match builder.Choices with
        | [] -> ()
        | choice :: choices ->
            buffer.Append (choice.ToString ()) |> ignore

            choices
            |> List.iter (fun choice ->
                buffer.Append ('-') |> ignore
                buffer.Append (choice.ToString ()) |> ignore
            )

        buffer.Append ('_') |> ignore
        buffer.Append (builder.Depth.ToString ()) |> ignore
        buffer.ToString ()

    /// The empty node path
    let empty : Builder =
        {
            Choices = []
            Depth = 0
        }

    /// Adds a choice point to that input path. The provided number says which of
    /// the choices at that point was taken. When traversing a computation or
    /// value, if any case has multiple recursive calls, you should add a choice
    /// point to the path built up in each of those calls, with each call using a
    /// different number.
    let choicePoint (builder : Builder) (n : int) : Builder =
        {
            Choices = builder.Choices @ [ n ]
            Depth = 0
        }

    /// Adds an extra segment to the input path. All such segments get forgotten
    /// when the next choice point is added.
    let descend (builder : Builder) : Builder =
        { builder with
            Depth = builder.Depth + 1
        }

    let finalize (builder : Builder) : NodePath = toString builder
