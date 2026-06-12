namespace WoofWare.Zoomies.Test

open System
open System.Threading
open System.Threading.Tasks
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.Zoomies
open FsCheck

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestWorldFreezer =
    let neverTickingStopwatch : IStopwatch =
        {
            Frequency = fun () -> 1_000_000_000L
            GetTimestamp = fun () -> 0L
        }
        :> _

    [<Test>]
    let ``clears previous changes on change dump`` () =
        task {
            use freezer =
                WorldFreezer.listen' UnrecognisedEscapeCodeBehaviour.Throw StopwatchMock.Empty

            freezer.DeliverKeystroke (ConsoleKeyInfo ('x', ConsoleKey.X, false, false, false))
            freezer.DeliverKeystroke (ConsoleKeyInfo ('y', ConsoleKey.Y, false, false, false))

            let result =
                freezer.Changes ()
                |> ValueOption.defaultValue [||]
                |> Array.map (fun change ->
                    match change with
                    | WorldStateChange.Keystroke c -> c.KeyChar
                    | ApplicationEvent () -> failwith "no app events"
                    | MouseEvent _ -> failwith "no mouse events"
                    | Paste _ -> failwith "no paste events"
                    | ApplicationEventException _ -> failwith "no exceptions possible"
                )

            result |> Array.toList |> shouldEqual [ 'x' ; 'y' ]

            // A second dump is empty: Changes clears the internal buffer.
            freezer.Changes () |> shouldEqual ValueNone
        }

    let charToKeyInfo (c : char) : ConsoleKeyInfo =
        if c = '\u001B' then // ESC
            ConsoleKeyInfo (c, ConsoleKey.Escape, false, false, false)
        else
            // ConsoleKey value is only ever compared to Escape
            ConsoleKeyInfo (c, ConsoleKey.A, false, false, false)

    /// The returned `release n` delivers the next `n` keys from the list into the freezer
    /// (clamped to the end of the list), as the platform input thread would.
    let makeFreezerOverList
        (sw : IStopwatch)
        (inputList : ConsoleKeyInfo list)
        (initialAllowed : int)
        : WorldFreezer<'a> * (int -> unit)
        =
        let wf = WorldFreezer.listen' UnrecognisedEscapeCodeBehaviour.PassThrough sw

        let mutable index = 0

        let release (n : int) =
            let limit = min inputList.Length (index + n)

            while index < limit do
                wf.DeliverKeystroke inputList.[index]
                index <- index + 1

        release initialAllowed
        wf, release

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

        let wfChunk, release = makeFreezerOverList neverTickingStopwatch keyInfos 0
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
                // deliver 'size' more keys
                release size

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

        // deliver any remaining keys
        do
            release keyInfos.Length

            match wfChunk.Changes () with
            | ValueNone -> ()
            | ValueSome arr ->
                for a in arr do
                    chunkedOutputs.Add a

        List.ofSeq chunkedOutputs

    let computeExpected (input : ChunkingInput) =
        let keyInfos = input.Input |> List.map charToKeyInfo
        let mutable timestamp = 0L

        let sw =
            { new IStopwatch with
                member _.Frequency = 1L
                member _.GetTimestamp () = timestamp
            }

        let wfWhole, _ = makeFreezerOverList sw keyInfos keyInfos.Length
        let result = drainChanges wfWhole |> Array.toList

        // If we didn't manage to parse an escape code, we eventually re-emit the same sequence we got in.
        timestamp <- 1L
        let tail = drainChanges wfWhole |> Array.toList
        let entirelyDrained = result @ tail

        // The emitted keystrokes should always be a subsequence of the input characters.
        // Some escape sequences are silently consumed (e.g., spurious EndBracketedPaste, or
        // recognized mouse/paste sequences), but characters are never invented or reordered.
        let keystrokeChars =
            entirelyDrained
            |> List.choose (fun evt ->
                match evt with
                | Keystroke k -> Some k.KeyChar
                | _ -> None
            )

        let rec isSubsequence (sub : char list) (super : char list) =
            match sub, super with
            | [], _ -> true
            | _, [] -> false
            | x :: xs, y :: ys -> if x = y then isSubsequence xs ys else isSubsequence sub ys

        if not (isSubsequence keystrokeChars input.Input) then
            failwith $"Keystroke chars %A{keystrokeChars} is not a subsequence of input %A{input.Input}"

        result

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
                InputRest = [ '[' ; '<' ; buttonChar ; ';' ; '4' ; '3' ; ';' ; '8' ; '4' ; pressRelease ]
                ChangesPerChunk = []
            }

        match expectedIsPress, computeExpected change with
        | true, [ WorldStateChange.MouseEvent (MouseEvent.Press (actualButton, modifiers, coords)) ]
        | false, [ WorldStateChange.MouseEvent (MouseEvent.Release (actualButton, modifiers, coords)) ] ->
            coords
            |> shouldEqual
                {
                    X = 43
                    Y = 84
                }

            modifiers |> shouldEqual MouseModifiers.None
            actualButton |> shouldEqual expectedButton
        | _ -> failwith $"unexpected: %O{change}"

        let prop =
            fun l ->
                chunkingInvariantProperty
                    { change with
                        ChangesPerChunk = l
                    }
            |> Prop.forAll (Arb.fromGen (Gen.listOf (Gen.choose (0, 11))))

        Check.One (propConfig, prop)

    [<Test>]
    let ``Begin-bracketed-paste enters paste mode without emitting event`` () =
        // BeginBracketedPaste just enters paste mode; it doesn't emit any event
        let change =
            {
                ChunkingInput.InputChar1 = '\u001B'
                InputRest = [ '[' ; '2' ; '0' ; '0' ; '~' ]
                ChangesPerChunk = []
            }

        let actual = computeExpected change
        // Nothing should be emitted - we're just entering paste mode
        actual |> shouldEqual []

    [<Test>]
    let ``End-bracketed-paste without begin emits nothing`` () =
        // Spurious EndBracketedPaste (without BeginBracketedPaste) should be ignored
        let change =
            {
                ChunkingInput.InputChar1 = '\u001B'
                InputRest = [ '[' ; '2' ; '0' ; '1' ; '~' ]
                ChangesPerChunk = []
            }

        let actual = computeExpected change
        // Nothing should be emitted for spurious end marker
        actual |> shouldEqual []

    [<Test>]
    let ``Empty paste emits Paste event with empty string`` () =
        // BeginBracketedPaste followed immediately by EndBracketedPaste
        let beginPaste = [ '\u001B' ; '[' ; '2' ; '0' ; '0' ; '~' ]
        let endPaste = [ '\u001B' ; '[' ; '2' ; '0' ; '1' ; '~' ]
        let allChars = beginPaste @ endPaste

        let change =
            {
                ChunkingInput.InputChar1 = List.head allChars
                InputRest = List.tail allChars
                ChangesPerChunk = []
            }

        let actual = computeExpected change
        actual |> List.exactlyOne |> shouldEqual (WorldStateChange.Paste "")

    [<Test>]
    let ``Paste with text emits Paste event with buffered content`` () =
        // Paste "hello"
        let beginPaste = [ '\u001B' ; '[' ; '2' ; '0' ; '0' ; '~' ]
        let content = [ 'h' ; 'e' ; 'l' ; 'l' ; 'o' ]
        let endPaste = [ '\u001B' ; '[' ; '2' ; '0' ; '1' ; '~' ]
        let allChars = beginPaste @ content @ endPaste

        let change =
            {
                ChunkingInput.InputChar1 = List.head allChars
                InputRest = List.tail allChars
                ChangesPerChunk = []
            }

        let actual = computeExpected change
        actual |> List.exactlyOne |> shouldEqual (WorldStateChange.Paste "hello")

    [<Test>]
    let ``Paste batches all characters into single event`` () =
        // This is the key test: pasting many characters should result in ONE event, not many
        let beginPaste = [ '\u001B' ; '[' ; '2' ; '0' ; '0' ; '~' ]
        let content = List.init 100 (fun i -> char (int 'a' + (i % 26)))
        let endPaste = [ '\u001B' ; '[' ; '2' ; '0' ; '1' ; '~' ]
        let allChars = beginPaste @ content @ endPaste

        let change =
            {
                ChunkingInput.InputChar1 = List.head allChars
                InputRest = List.tail allChars
                ChangesPerChunk = []
            }

        let actual = computeExpected change

        // Should be exactly one Paste event
        actual |> List.length |> shouldEqual 1

        match List.head actual with
        | WorldStateChange.Paste s -> s.Length |> shouldEqual 100
        | other -> failwith $"Expected Paste event, got %O{other}"

    [<Test>]
    let ``Paste containing escape characters buffers them correctly`` () =
        // If pasted content contains ESC, it should be buffered (not interpreted as escape sequence)
        // Note: This test verifies that aborted escape sequences within paste mode get buffered
        let beginPaste = [ '\u001B' ; '[' ; '2' ; '0' ; '0' ; '~' ]
        // Paste contains: "a<ESC>b" - the ESC is a literal character, not an escape sequence start
        // But since ESC is followed by 'b' (not '['), the escape sequence will be aborted
        // and both ESC and 'b' should be buffered
        let content = [ 'a' ; '\u001B' ; 'b' ]
        let endPaste = [ '\u001B' ; '[' ; '2' ; '0' ; '1' ; '~' ]
        let allChars = beginPaste @ content @ endPaste

        let change =
            {
                ChunkingInput.InputChar1 = List.head allChars
                InputRest = List.tail allChars
                ChangesPerChunk = []
            }

        // Use drainChanges which handles the timeout-based re-emit of aborted escape sequences
        let keyInfos = allChars |> List.map charToKeyInfo
        let mutable timestamp = 0L

        let sw =
            { new IStopwatch with
                member _.Frequency = 1L
                member _.GetTimestamp () = timestamp
            }

        let wf, _ = makeFreezerOverList sw keyInfos keyInfos.Length
        let result1 = drainChanges wf |> Array.toList

        // Trigger timeout to flush any pending escape sequences
        timestamp <- 1L
        let result2 = drainChanges wf |> Array.toList

        let actual = result1 @ result2

        // Should have exactly one Paste event containing "a<ESC>b"
        actual |> List.length |> shouldEqual 1

        match List.head actual with
        | WorldStateChange.Paste s ->
            s.Length |> shouldEqual 3
            s.[0] |> shouldEqual 'a'
            s.[1] |> shouldEqual '\u001B'
            s.[2] |> shouldEqual 'b'
        | other -> failwith $"Expected Paste event, got %O{other}"

    let ansiCharGen =
        let baseSet = [ '\u001B' ; '[' ; '<' ; ';' ; 'M' ; 'm' ; '~' ]
        let digits = [ '0' .. '9' ]
        let letters = List.concat [ [ 'a' .. 'z' ] ; [ 'A' .. 'Z' ] ]
        let extras = [ ' ' ; ',' ; '.' ; ':' ; '/' ; '(' ; ')' ]

        gen {
            let! tag = Gen.choose (0, 3)

            match tag with
            | 0 -> return! Gen.elements baseSet
            | 1 -> return! Gen.elements digits
            | 2 -> return! Gen.elements letters
            | 3 -> return! Gen.elements extras
            | _ -> return failwith "logic error"
        }

    [<Test>]
    let ``Property: the same sequence of inputs eventually results in the same sequence of outputs, ESC`` () =
        let chunkingInputGen =
            gen {
                let! rest = Gen.listOf ansiCharGen
                let! changes = Gen.listOf (Gen.choose (0, 15))

                return
                    {
                        InputChar1 = '\u001B'
                        InputRest = rest
                        ChangesPerChunk = changes
                    }
            }

        let prop = Prop.forAll (Arb.fromGen chunkingInputGen) chunkingInvariantProperty
        Check.One (propConfig, prop)

    let chunkingInputGenEscBracket =
        gen {
            let! rest = Gen.listOf ansiCharGen
            let! changes = Gen.listOf (Gen.choose (0, 15))

            return
                {
                    InputChar1 = '\u001B'
                    InputRest = '[' :: rest
                    ChangesPerChunk = changes
                }
        }

    [<Test>]
    let ``Property: the same sequence of inputs results in the same sequence of outputs, ESC bracket`` () =
        let prop =
            Prop.forAll (Arb.fromGen chunkingInputGenEscBracket) chunkingInvariantProperty

        Check.One (propConfig, prop)

    [<TestCase(3856024818419232693UL, 5423926211778887271UL, 79)>]
    [<TestCase(1761304542359499027UL, 1080833032316032411UL, 10)>]
    let ``Property: the same sequence of inputs results in the same sequence of outputs, ESC bracket, regressions``
        (seed : uint64, gamma : uint64, size : int)
        =
        let prop =
            Prop.forAll (Arb.fromGen chunkingInputGenEscBracket) chunkingInvariantProperty

        Check.One (Config.QuickThrowOnFailure.WithReplay (seed, gamma, size), prop)

    /// Generator biased toward bracketed paste sequences.
    /// This helps find bugs involving paste mode entry/exit sequences followed by other characters.
    let chunkingInputGenBracketedPaste =
        gen {
            // Generate the bracketed paste marker (200 for begin, 201 for end)
            let! pasteCode = Gen.elements [ "200" ; "201" ]
            let pasteMarker = '[' :: (pasteCode |> Seq.toList) @ [ '~' ]
            // Generate some trailing characters that follow the paste marker
            let! trailing = Gen.listOf ansiCharGen
            let! changes = Gen.listOf (Gen.choose (0, 15))

            return
                {
                    InputChar1 = '\u001B'
                    InputRest = pasteMarker @ trailing
                    ChangesPerChunk = changes
                }
        }

    [<Test>]
    let ``Property: the same sequence of inputs results in the same sequence of outputs, bracketed paste`` () =
        let prop =
            Prop.forAll (Arb.fromGen chunkingInputGenBracketedPaste) chunkingInvariantProperty

        Check.One (propConfig, prop)

    [<Test>]
    let ``Property: the same sequence of inputs results in the same sequence of outputs, ESC bracket angle`` () =
        let chunkingInputGen =
            gen {
                let! rest = Gen.listOf ansiCharGen
                let! changes = Gen.listOf (Gen.choose (0, 15))

                return
                    {
                        InputChar1 = '\u001B'
                        InputRest = '[' :: '<' :: rest
                        ChangesPerChunk = changes
                    }
            }

        let prop = Prop.forAll (Arb.fromGen chunkingInputGen) chunkingInvariantProperty
        Check.One (propConfig, prop)

    [<Test>]
    let ``Property: the same sequence of inputs results in the same sequence of outputs, unrestricted`` () =
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

    // ============================================================
    // NextDeadline tests: the event loop must wake no later than
    // NextDeadline to deliver timed-out partial input.
    // ============================================================

    /// Frequency 1e9 means stopwatch ticks are nanoseconds; the 10ms re-emit
    /// timeout is then 10_000_000 ticks (+1 for the strict comparison).
    let private deadlineTicks =
        int64 (WorldFreezerTimeouts.REEMIT_TIMEOUT_SECONDS * 1e9) + 1L

    let private makeTickingFreezer () : WorldFreezer<unit> * int64 ref * (char -> unit) =
        let ts = ref 0L

        let stopwatch =
            { new IStopwatch with
                member _.GetTimestamp () = ts.Value
                member _.Frequency = 1_000_000_000L
            }

        let freezer = WorldFreezer.listen' UnrecognisedEscapeCodeBehaviour.Throw stopwatch

        let send (c : char) =
            let key =
                if c = '\u001B' then
                    ConsoleKeyInfo (c, ConsoleKey.Escape, false, false, false)
                else
                    ConsoleKeyInfo (c, ConsoleKey.A, false, false, false)

            freezer.DeliverKeystroke key

        freezer, ts, send

    [<Test>]
    let ``NextDeadline is ValueNone when nothing is pending`` () =
        task {
            let freezer, _, send = makeTickingFreezer ()
            use _ = freezer
            freezer.NextDeadline () |> shouldEqual ValueNone

            // A complete ordinary keystroke leaves nothing pending either.
            send 'x'
            freezer.Changes () |> ValueOption.isSome |> shouldEqual true
            freezer.NextDeadline () |> shouldEqual ValueNone
        }

    [<Test>]
    let ``a pending lone Esc sets a deadline, and waking at it delivers the Esc`` () =
        task {
            let freezer, ts, send = makeTickingFreezer ()
            use _ = freezer

            ts.Value <- 42L
            send '\u001B'

            // The Esc is swallowed pending disambiguation; nothing is emitted yet.
            freezer.Changes () |> shouldEqual ValueNone
            freezer.NextDeadline () |> shouldEqual (ValueSome (42L + deadlineTicks))

            // Waking at exactly the deadline suffices to flush it.
            ts.Value <- 42L + deadlineTicks

            match freezer.Changes () with
            | ValueNone -> failwith "expected the lone Esc to be re-emitted at the deadline"
            | ValueSome changes ->
                changes
                |> Array.map (fun c ->
                    match c with
                    | WorldStateChange.Keystroke k -> k.KeyChar
                    | other -> failwith $"unexpected change: %O{other}"
                )
                |> shouldEqual [| '\u001B' |]

            freezer.NextDeadline () |> shouldEqual ValueNone
        }

    [<Test>]
    let ``entering bracketed paste sets a deadline, and waking at it re-emits the entry keys`` () =
        task {
            let freezer, ts, send = makeTickingFreezer ()
            use _ = freezer

            ts.Value <- 100L

            for c in "\u001B[200~" do
                send c


            // Paste-begin is recognised and swallowed; we are now waiting for content/end marker.
            freezer.Changes () |> shouldEqual ValueNone
            freezer.NextDeadline () |> shouldEqual (ValueSome (100L + deadlineTicks))

            // No end marker arrives; waking at the deadline re-emits the entry sequence raw.
            ts.Value <- 100L + deadlineTicks

            match freezer.Changes () with
            | ValueNone -> failwith "expected the paste-entry keys to be re-emitted at the deadline"
            | ValueSome changes ->
                changes
                |> Array.map (fun c ->
                    match c with
                    | WorldStateChange.Keystroke k -> k.KeyChar
                    | other -> failwith $"unexpected change: %O{other}"
                )
                |> shouldEqual [| '\u001B' ; '[' ; '2' ; '0' ; '0' ; '~' |]

            freezer.NextDeadline () |> shouldEqual ValueNone
        }

    [<Test>]
    let ``with both a paste timeout and a pending Esc, NextDeadline is the earlier`` () =
        task {
            let freezer, ts, send = makeTickingFreezer ()
            use _ = freezer

            // Enter paste mode at t=0.
            for c in "\u001B[200~" do
                send c

            freezer.Changes () |> shouldEqual ValueNone
            freezer.NextDeadline () |> shouldEqual (ValueSome deadlineTicks)

            // A lone Esc arrives 2ms later (possibly the start of the end marker).
            ts.Value <- 2_000_000L
            send '\u001B'
            freezer.Changes () |> ignore<WorldStateChange<unit>[] voption>

            // Whatever the Esc contributes, the paste deadline is earlier and must win.
            match freezer.NextDeadline () with
            | ValueNone -> failwith "expected a deadline while the paste timeout is pending"
            | ValueSome deadline -> deadline |> shouldEqual deadlineTicks
        }

    // ============================================================
    // Wake signal and blocking-input tests.
    // ============================================================

    let private waitTimeout = TimeSpan.FromSeconds 5.0

    [<Test>]
    let ``WaitForChange completes when a keystroke is delivered, and re-arms`` () =
        task {
            use freezer =
                WorldFreezer.listen'<unit> UnrecognisedEscapeCodeBehaviour.Throw StopwatchMock.Empty

            let wait = freezer.WaitForChange ()
            wait.IsCompleted |> shouldEqual false

            freezer.DeliverKeystroke (ConsoleKeyInfo ('x', ConsoleKey.X, false, false, false))
            do! wait.WaitAsync waitTimeout

            // Re-arm: a fresh wait does not complete until the next change.
            let wait2 = freezer.WaitForChange ()
            wait2.IsCompleted |> shouldEqual false

            freezer.DeliverKeystroke (ConsoleKeyInfo ('y', ConsoleKey.Y, false, false, false))
            do! wait2.WaitAsync waitTimeout
        }

    [<Test>]
    let ``WaitForChange completes on PostEvent from another thread`` () =
        task {
            use freezer =
                WorldFreezer.listen'<int> UnrecognisedEscapeCodeBehaviour.Throw StopwatchMock.Empty

            let bridge = freezer :> IWorldBridge<int>

            let wait = freezer.WaitForChange ()
            do! Task.Run (fun () -> bridge.PostEvent 5)
            do! wait.WaitAsync waitTimeout

            match freezer.Changes () with
            | ValueSome [| WorldStateChange.ApplicationEvent 5 |] -> ()
            | other -> failwith $"unexpected changes: %A{other}"
        }

    [<Test>]
    let ``WaitForChange completes on terminal resize notification`` () =
        task {
            use freezer =
                WorldFreezer.listen'<unit> UnrecognisedEscapeCodeBehaviour.Throw StopwatchMock.Empty

            let wait = freezer.WaitForChange ()
            freezer.NotifyTerminalResize ()
            do! wait.WaitAsync waitTimeout
        }

    [<Test>]
    let ``WaitForChange does not complete on a quiet freezer`` () =
        task {
            use freezer =
                WorldFreezer.listen'<unit> UnrecognisedEscapeCodeBehaviour.Throw StopwatchMock.Empty

            let wait = freezer.WaitForChange ()
            do! Task.Delay 50
            wait.IsCompleted |> shouldEqual false
        }

    [<Test>]
    let ``a change between arming and awaiting is not lost`` () =
        task {
            use freezer =
                WorldFreezer.listen'<int> UnrecognisedEscapeCodeBehaviour.Throw StopwatchMock.Empty

            let bridge = freezer :> IWorldBridge<int>

            // Arm, then signal before awaiting: the await must return immediately.
            let wait = freezer.WaitForChange ()
            bridge.PostEvent 1
            do! wait.WaitAsync waitTimeout
        }

    [<Test>]
    let ``concurrent PostEvents all arrive and at least one signal fires`` () =
        task {
            use freezer =
                WorldFreezer.listen'<int> UnrecognisedEscapeCodeBehaviour.Throw StopwatchMock.Empty

            let bridge = freezer :> IWorldBridge<int>
            let wait = freezer.WaitForChange ()

            let! _ = Task.WhenAll [| for i in 1..100 -> Task.Run (fun () -> bridge.PostEvent i) |]

            do! wait.WaitAsync waitTimeout

            match freezer.Changes () with
            | ValueNone -> failwith "expected 100 events"
            | ValueSome changes -> changes.Length |> shouldEqual 100
        }

    [<Test>]
    let ``listenBlocking delivers keys read by the input thread`` () =
        task {
            use keys = new System.Collections.Concurrent.BlockingCollection<ConsoleKeyInfo> ()

            use freezer =
                WorldFreezer.listenBlocking<unit>
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    (fun ct ->
                        try
                            ValueSome (keys.Take ct)
                        with :? OperationCanceledException ->
                            ValueNone
                    )

            let wait = freezer.WaitForChange ()
            keys.Add (ConsoleKeyInfo ('x', ConsoleKey.X, false, false, false))
            do! wait.WaitAsync waitTimeout

            match freezer.Changes () with
            | ValueSome [| WorldStateChange.Keystroke k |] -> k.KeyChar |> shouldEqual 'x'
            | other -> failwith $"unexpected changes: %A{other}"
        }

    [<Test>]
    let ``disposal completes while a blocking read is in flight, and unblocks it`` () =
        task {
            use started = new SemaphoreSlim (0)
            use exited = new SemaphoreSlim (0)

            let freezer =
                WorldFreezer.listenBlocking<unit>
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    (fun ct ->
                        started.Release () |> ignore<int>
                        ct.WaitHandle.WaitOne () |> ignore<bool>
                        exited.Release () |> ignore<int>
                        ValueNone
                    )

            let! enteredRead = started.WaitAsync waitTimeout
            enteredRead |> shouldEqual true

            // Disposal must complete even though the read is still blocked...
            do! (freezer :> IAsyncDisposable).DisposeAsync ()

            // ...and its cancellation token must unblock the read.
            let! readUnblocked = exited.WaitAsync waitTimeout
            readUnblocked |> shouldEqual true
        }

    [<Test>]
    let ``input thread exceptions surface as ApplicationEventException`` () =
        task {
            use freezer =
                WorldFreezer.listenBlocking<unit>
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    (fun _ -> failwith "console went away")

            // The consumer protocol: arm, drain, await only if the drain came up empty,
            // drain again. (The exception may arrive before or after the arm.)
            let wait = freezer.WaitForChange ()

            let! changes =
                task {
                    match freezer.Changes () with
                    | ValueSome changes -> return changes
                    | ValueNone ->
                        do! wait.WaitAsync waitTimeout

                        match freezer.Changes () with
                        | ValueSome changes -> return changes
                        | ValueNone -> return failwith "signal fired but no changes arrived"
                }

            match changes with
            | [| WorldStateChange.ApplicationEventException e |] -> e.Message |> shouldEqual "console went away"
            | other -> failwith $"unexpected changes: %A{other}"
        }
