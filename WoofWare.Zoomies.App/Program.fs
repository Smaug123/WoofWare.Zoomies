namespace WoofWare.Zoomies

open System
open System.IO
open System.Runtime.ExceptionServices
open System.Threading
open System.Threading.Tasks
open WoofWare.Zoomies

type AppEvent =
    | FileLoaded of filename : string * content : string
    | FileLoadError of filename : string * error : string

type State =
    {
        File1Path : string
        File2Path : string
        mutable ShowingFile1 : bool
        mutable FileContent : string option
        mutable IsLoading : bool
    }

    static member Create (file1 : string, file2 : string) =
        {
            File1Path = file1
            File2Path = file2
            ShowingFile1 = true
            FileContent = None
            IsLoading = false
        }

    member this.CurrentFile = if this.ShowingFile1 then this.File1Path else this.File2Path

module FileBrowser =

    let loadFileAsync (postEvent : (CancellationToken -> Task<AppEvent>) -> unit) (filepath : string) : unit =
        fun ct ->
            task {
                let! content = File.ReadAllTextAsync (filepath, ct)
                return FileLoaded (filepath, content)
            }
        |> postEvent

    let processWorld (postEvent : (CancellationToken -> Task<AppEvent>) -> unit) =
        { new WorldProcessor<AppEvent, State> with
            member _.ProcessWorld (changes, prevVdom, state) =
                for change in changes do
                    match change with
                    | WorldStateChange.MouseEvent _ ->
                        // ignore mouse events
                        ()
                    | WorldStateChange.KeyboardEvent _ ->
                        // ignore keyboard events
                        ()
                    | WorldStateChange.Keystroke key when key.KeyChar = ' ' ->
                        // Toggle which file we're showing
                        state.ShowingFile1 <- not state.ShowingFile1
                        state.IsLoading <- true
                        state.FileContent <- None
                        // Trigger async load of the new file
                        loadFileAsync postEvent state.CurrentFile
                    | WorldStateChange.Keystroke _ -> ()

                    | WorldStateChange.ApplicationEvent (FileLoaded (filename, content)) ->
                        // Only update if this is still the file we're expecting
                        if filename = state.CurrentFile then
                            state.FileContent <- Some content
                            state.IsLoading <- false

                    | WorldStateChange.ApplicationEvent (FileLoadError (filename, error)) ->
                        if filename = state.CurrentFile then
                            state.FileContent <- Some $"Error loading file: {error}"
                            state.IsLoading <- false

                    | WorldStateChange.ApplicationEventException e ->
                        ExceptionDispatchInfo.Throw e
                        failwith "unreachable"
        }

    let view (renderState : RenderState) (state : State) : Vdom<DesiredBounds, Unkeyed> =
        let topPane =
            let label = $"[{state.File1Path}] / [{state.File2Path}]"

            let checkboxKey = NodeKey.make "checkbox"
            let currentFocus = RenderState.focusedKey renderState

            let checkbox =
                Vdom.checkbox
                    (currentFocus = Some checkboxKey)
                    (not state.ShowingFile1)
                |> Vdom.withKey checkboxKey
                |> Vdom.focusable

            Vdom.panelSplitAbsolute (Direction.Vertical, 3, checkbox, Vdom.textContent false label)
            |> Vdom.bordered

        let bottomPane =
            let content =
                match state.IsLoading, state.FileContent with
                | true, _ -> "Loading..."
                | false, Some content -> content
                | false, None -> "Press space to load a file"

            Vdom.textContent false content |> Vdom.bordered

        Vdom.panelSplitAbsolute (Direction.Horizontal, 3, topPane, bottomPane)

    let run (file1 : string) (file2 : string) =
        let state = State.Create (file1, file2)

        state.IsLoading <- true

        App.run
            state
            (fun _ -> true) // framework handles focus
            processWorld
            view

// Usage:
module Program =
    [<EntryPoint>]
    let main argv =
        let cwd = DirectoryInfo Environment.CurrentDirectory

        let file1, file2 =
            match cwd.EnumerateFiles () |> Seq.take 2 |> Seq.toList with
            | [ a ; b ] -> a, b
            | _ -> failwith "oh no"

        FileBrowser.run file1.FullName file2.FullName |> _.Wait()
        0
