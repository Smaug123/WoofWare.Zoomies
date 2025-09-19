namespace WoofWare.Zoomies.Test

open System
open System.Threading
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.Zoomies
open FsCheck

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestWorldFreezer =
    let neverTickingStopwatch () : IStopwatch =
        let mutable time = 0L

        {
            Frequency = fun () -> 1_000_000_000L
            GetTimestamp = fun () -> Interlocked.Increment &time
        }
        :> _

    [<Test>]
    let ``clears previous changes on change dump`` () =
        task {
            let mutable callCount = 0

            let keys =
                [|
                    ConsoleKeyInfo ('x', ConsoleKey.X, false, false, false)
                    ConsoleKeyInfo ('y', ConsoleKey.Y, false, false, false)
                |]

            let keyAvailable () = callCount < keys.Length

            let readKey () =
                let key = keys.[callCount]
                Interlocked.Increment &callCount |> ignore<int>
                key

            use freezer =
                WorldFreezer.listen' UnrecognisedEscapeCodeBehaviour.Throw StopwatchMock.Empty keyAvailable readKey

            let seen = ResizeArray ()
            let mutable cont = true

            while cont do
                freezer.RefreshExternal ()

                let result =
                    freezer.Changes ()
                    |> ValueOption.defaultValue [||]
                    |> Array.map (fun change ->
                        match change with
                        | WorldStateChange.Keystroke c -> c.KeyChar
                        | ApplicationEvent () -> failwith "no app events"
                        | MouseEvent _ -> failwith "no mouse events"
                        | KeyboardEvent _ -> failwith "no keyboard events"
                        | ApplicationEventException _ -> failwith "no exceptions possible"
                    )

                seen.AddRange result

                match Array.tryLast result with
                | Some 'y' -> cont <- false
                | _ -> ()

            seen |> Seq.toList |> shouldEqual [ 'x' ; 'y' ]

            freezer.RefreshExternal ()
            freezer.Changes () |> shouldEqual ValueNone
        }

    let charToKeyInfo (c : char) : ConsoleKeyInfo =
        if c = '\u001B' then // ESC
            ConsoleKeyInfo (c, ConsoleKey.Escape, false, false, false)
        else
            // ConsoleKey value is only ever compared to Escape
            ConsoleKeyInfo (c, ConsoleKey.A, false, false, false)

    /// The `ref` is "how far through the list are we allowed to consume" (basically specifying which characters
    /// have been sent yet).
    let makeFreezerOverList (inputList : ConsoleKeyInfo list) (initialAllowed : int) : WorldFreezer<'a> * int ref =
        let allowed = ref initialAllowed
        let mutable index = 0

        let keyAvailable () =
            index < allowed.Value && index < inputList.Length

        let readKey () =
            let i = index
            // assume caller only calls ReadKey when KeyAvailable returned true
            index <- i + 1
            inputList.[i]

        let sw = neverTickingStopwatch ()

        let wf =
            WorldFreezer.listen' UnrecognisedEscapeCodeBehaviour.PassThrough sw keyAvailable readKey

        wf, allowed

    let drainChanges (wf : WorldFreezer<unit>) =
        let out = ResizeArray<WorldStateChange<unit>> ()

        let rec loop () =
            match wf.Changes () with
            | ValueNone -> ()
            | ValueSome arr ->
                for a in arr do
                    out.Add a

                loop ()

        loop ()
        out.ToArray ()

    type ChunkingInput =
        {
            InputChar1 : char
            InputRest : char list
            ChangesPerChunk : int list
        }

        member this.Input = this.InputChar1 :: this.InputRest

    let computeActual (input : ChunkingInput) =
        let keyInfos = input.Input |> List.map charToKeyInfo

        // determine chunk count: use the provided changesPerChunk length if nonzero,
        // otherwise default to 1. Clamp chunk count to [1 .. inputLen].
        let inputLen = input.Input.Length

        let rawChunkCount =
            match input.ChangesPerChunk with
            | [] -> 1
            | xs -> xs.Length

        let chunkCount = max 1 (min inputLen rawChunkCount)

        // even-ish partition of the input into chunkCount chunks (deterministic)
        let baseChunk = inputLen / chunkCount
        let rem = inputLen % chunkCount

        let chunkSizes =
            [
                for i in 0 .. chunkCount - 1 do
                    yield if i < rem then baseChunk + 1 else baseChunk
            ]

        // Use the provided changesPerChunk values (clamped) to decide how many Changes() calls after each chunk.
        let truncatedChangesPerChunk =
            if List.isEmpty input.ChangesPerChunk then
                // default: 1 `Changes` call per chunk
                List.init chunkCount (fun _ -> 1)
            else
                // If caller gave more chunks than inputLen, we already clamped chunkCount,
                // but we need an array of length chunkCount for calls; reuse / truncate as needed.
                let adjusted = input.ChangesPerChunk |> List.take chunkCount
                // if shorter, pad with 1
                let pad n xs =
                    xs @ List.init (n - List.length xs) (fun _ -> 1)

                if List.length adjusted < chunkCount then
                    pad chunkCount adjusted
                else
                    adjusted

        let wfChunk, cursor = makeFreezerOverList keyInfos 0
        let chunkedOutputs = ResizeArray<WorldStateChange<_>> ()

        // call Changes once up-front (simulate UI calling Changes before any keys arrive)
        match wfChunk.Changes () with
        | ValueNone -> ()
        | ValueSome arr ->
            for a in arr do
                chunkedOutputs.Add a

        // For each chunk: make that many keys available + RefreshExternal, then call Changes as specified.
        let rec processChunks (remainingSizes : int list) (remainingChanges : int list) =
            match remainingSizes, remainingChanges with
            | [], [] -> ()
            | size :: restSizes, ch :: restChanges ->
                // make 'size' more keys available
                cursor.Value <- cursor.Value + size
                // let the world read them into its internal queue
                wfChunk.RefreshExternal ()

                // call Changes the specified number of times (0..n)
                for _ in 1 .. (max 0 ch) do
                    match wfChunk.Changes () with
                    | ValueNone -> ()
                    | ValueSome arr ->
                        for a in arr do
                            chunkedOutputs.Add a

                processChunks restSizes restChanges
            | _ -> failwith "logic error"

        processChunks chunkSizes truncatedChangesPerChunk

        // make any remaining keys visible
        do
            cursor.Value <- keyInfos.Length
            wfChunk.RefreshExternal ()

            match wfChunk.Changes () with
            | ValueNone -> ()
            | ValueSome arr ->
                for a in arr do
                    chunkedOutputs.Add a

        List.ofSeq chunkedOutputs

    let computeExpected (input : ChunkingInput) =
        let keyInfos = input.Input |> List.map charToKeyInfo
        let wfWhole, _ = makeFreezerOverList keyInfos keyInfos.Length
        // make all keys available at once, refresh to push into internal queue
        wfWhole.RefreshExternal ()
        drainChanges wfWhole |> Array.toList

    /// Property: for any nonempty (input := inputChar1 :: inputRest) and changesPerChunk,
    /// the concatenated outputs of the 'chunked' schedule equals the 'whole' schedule.
    /// We interpret changesPerChunk as "how many times to call Changes() after each chunk".
    /// If changesPerChunk is empty we treat it as one chunk (so we still exercise the whole path).
    let chunkingInvariantProperty (input : ChunkingInput) =
        let expected = computeExpected input
        let actual = computeActual input

        actual |> shouldEqual expected

    let mouseMapping =
        [ '0', MouseButton.Left ; '1', MouseButton.Middle ; '2', MouseButton.Right ]

    let isPressMapping = [ 'm', false ; 'M', true ]

    let buttons = List.allPairs mouseMapping isPressMapping |> List.map TestCaseData

    [<TestCaseSource(nameof buttons)>]
    let ``Can emit mouse down event``
        ((buttonChar : char, expectedButton : MouseButton), (pressRelease : char, expectedIsPress : bool))
        =
        let change =
            {
                ChunkingInput.InputChar1 = '\u001B'
                InputRest =
                    [
                        '['
                        '<'
                        buttonChar
                        ';'
                        '4'
                        '3'
                        ';'
                        '8'
                        '4'
                        ';'
                        pressRelease
                    ]
                ChangesPerChunk = []
            }
            |> computeExpected
            |> List.exactlyOne

        match expectedIsPress, change with
        | true, WorldStateChange.MouseEvent (MouseEvent.Press (actualButton, modifiers, coords))
        | false, WorldStateChange.MouseEvent (MouseEvent.Release (actualButton, modifiers, coords)) ->
            coords
            |> shouldEqual
                {
                    X = 43
                    Y = 84
                }

            modifiers |> shouldEqual MouseModifiers.None
            actualButton |> shouldEqual expectedButton
        | _ -> failwith $"unexpected: %O{change}"

    [<Test>]
    let ``Property: the same sequence of inputs eventually results in the same sequence of outputs`` () =
        let ansiCharGen =
            let baseSet = [ '\u001B' ; '[' ; '<' ; ';' ; 'M' ; 'm' ; '~' ]
            let digits = [ '0' .. '9' ]
            let letters = List.concat [ [ 'a' .. 'z' ] ; [ 'A' .. 'Z' ] ]
            let extras = [ ' ' ; ',' ; '.' ; ':' ; '/' ; '(' ; ')' ]
            Gen.elements (baseSet @ digits @ letters @ extras)

        let chunkingInputGen =
            gen {
                let! inputChar1 = ansiCharGen
                let! rest = Gen.listOf ansiCharGen
                let! changes = Gen.listOf (Gen.choose (0, 15))

                return
                    {
                        InputChar1 = inputChar1
                        InputRest = rest
                        ChangesPerChunk = changes
                    }
            }

        let prop = Prop.forAll (Arb.fromGen chunkingInputGen) chunkingInvariantProperty
        Check.One (propConfig, prop)
