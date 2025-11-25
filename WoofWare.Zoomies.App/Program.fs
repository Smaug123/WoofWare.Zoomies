namespace WoofWare.Zoomies

open System
open System.IO
open System.Runtime.ExceptionServices
open System.Threading.Tasks
open WoofWare.Zoomies

type AppEvent =
    | FileLoaded of filename : string * content : string * generation : int
    | FileLoadError of filename : string * error : string * generation : int

type State =
    {
        File1Path : string
        File2Path : string
        ShowingFile1 : bool
        FileContent : string option
        IsLoading : bool
        Generation : int
    }

    static member Create (file1 : string, file2 : string) =
        {
            File1Path = file1
            File2Path = file2
            ShowingFile1 = true
            FileContent = None
            IsLoading = false
            Generation = 1
        }

    member this.CurrentFile = if this.ShowingFile1 then this.File1Path else this.File2Path

type StateBuilder =
    {
        mutable File1Path : string
        mutable File2Path : string
        mutable ShowingFile1 : bool
        mutable FileContent : string option
        mutable IsLoading : bool
        mutable Generation : int
    }

    member state.ToImmutable () : State =
        {
            File1Path = state.File1Path
            File2Path = state.File2Path
            ShowingFile1 = state.ShowingFile1
            FileContent = state.FileContent
            IsLoading = state.IsLoading
            Generation = state.Generation
        }

    static member Create (state : State) : StateBuilder =
        {
            File1Path = state.File1Path
            File2Path = state.File2Path
            ShowingFile1 = state.ShowingFile1
            FileContent = state.FileContent
            IsLoading = state.IsLoading
            Generation = state.Generation
        }

    member this.CurrentFile = if this.ShowingFile1 then this.File1Path else this.File2Path

module FileBrowser =

    /// For simplicity I'm not making these cancellable.
    let loadFileAsync (generation : int) (worldBridge : IWorldBridge<_>) (filepath : string) : unit Task =
        task {
            try
                let! content = File.ReadAllTextAsync filepath
                return FileLoaded (filepath, content, generation) |> worldBridge.PostEvent
            with e ->
                FileLoadError (filepath, e.Message, generation) |> worldBridge.PostEvent
        }

    let processWorld (initialState : State) (worldBridge : IWorldBridge<AppEvent>) =
        // Trigger initial file load
        let _ = loadFileAsync initialState.Generation worldBridge initialState.CurrentFile

        { new WorldProcessor<AppEvent, State> with
            member _.ProcessWorld (changes, prevVdom, state) =
                let state = StateBuilder.Create state

                for change in changes do
                    match change with
                    | WorldStateChange.MouseEvent _ ->
                        // ignore mouse events
                        ()
                    | WorldStateChange.KeyboardEvent _ ->
                        // ignore keyboard events
                        ()
                    | WorldStateChange.Keystroke key when key.KeyChar = ' ' ->
                        // Toggle which file we're showing.
                        state.ShowingFile1 <- not state.ShowingFile1
                        state.IsLoading <- true
                        state.FileContent <- None

                        // Trigger async load of the new file
                        state.Generation <- state.Generation + 1

                        loadFileAsync state.Generation worldBridge state.CurrentFile
                        |> ignore<Task<unit>>
                    | WorldStateChange.Keystroke _ -> ()

                    | WorldStateChange.ApplicationEvent (FileLoaded (filename, content, generation)) ->
                        // Only update if this is still the file we're expecting
                        if state.Generation = generation then
                            state.FileContent <- Some content
                            state.IsLoading <- false

                    | WorldStateChange.ApplicationEvent (FileLoadError (filename, error, generation)) ->
                        if state.Generation = generation then
                            state.FileContent <- Some $"Error loading file: {error}"
                            state.IsLoading <- false

                    | WorldStateChange.ApplicationEventException e ->
                        ExceptionDispatchInfo.Throw e
                        failwith "unreachable"

                ProcessWorldResult.make (state.ToImmutable ())
        }

    let view (vdomContext : VdomContext) (state : State) : Vdom<DesiredBounds, Unkeyed> =
        let topPane =
            let label = $"[{state.File1Path}] / [{state.File2Path}]"

            let checkboxKey = NodeKey.make "checkbox"
            let currentFocus = VdomContext.focusedKey vdomContext

            let checkbox =
                Vdom.checkbox (currentFocus = Some checkboxKey) (not state.ShowingFile1)
                |> Vdom.withKey checkboxKey
                |> Vdom.withFocusTracking

            Vdom.panelSplitAuto (SplitDirection.Vertical, checkbox, Vdom.textContent false label)
            |> Vdom.bordered

        let bottomPane =
            let content =
                match state.IsLoading, state.FileContent with
                | true, _ -> "Loading..."
                | false, Some content -> content
                | false, None -> "Press space to load a file"

            Vdom.textContent false content |> Vdom.bordered

        Vdom.panelSplitAuto (SplitDirection.Horizontal, topPane, bottomPane)

    let run (getEnv : string -> string option) (file1 : string) (file2 : string) =
        let state = State.Create (file1, file2)

        let initialState =
            { state with
                IsLoading = true
            }

        App.run
            getEnv
            initialState
            (fun _ -> true) // framework handles focus
            (processWorld initialState)
            view
            ActivationResolver.none

// Usage:
module Program =
    let getEnv (varName : string) : string option =
        match Environment.GetEnvironmentVariable varName with
        | null -> None
        | value -> Some value

    [<EntryPoint>]
    let main argv =
        let cwd = DirectoryInfo Environment.CurrentDirectory

        let file1, file2 =
            match cwd.EnumerateFiles () |> Seq.take 2 |> Seq.toList with
            | [ a ; b ] -> a, b
            | _ -> failwith "oh no"

        FileBrowser.run getEnv file1.FullName file2.FullName |> _.Wait()
        0
