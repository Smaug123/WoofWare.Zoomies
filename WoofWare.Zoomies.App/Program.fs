namespace WoofWare.Zoomies

open System
open System.IO
open System.Runtime.ExceptionServices
open System.Threading.Tasks
open WoofWare.Zoomies
open WoofWare.Zoomies.Components

type AppEvent =
    | TextEdit of TextBoxAction
    | LoadButtonClicked
    | FileLoaded of content : string * generation : int
    | FileLoadError of error : string * generation : int

type State =
    {
        PathInput : string
        CursorPos : int
        FileContent : string option
        IsLoading : bool
        Generation : int
    }

    static member Initial =
        {
            PathInput = ""
            CursorPos = 0
            FileContent = None
            IsLoading = false
            Generation = 0
        }

module FileBrowser =
    let textBoxKey = NodeKey.make "pathInput"
    let loadButtonKey = NodeKey.make "loadButton"

    let loadFileAsync (generation : int) (worldBridge : IWorldBridge<_>) (filepath : string) : unit Task =
        task {
            try
                let! content = File.ReadAllTextAsync filepath
                return FileLoaded (content, generation) |> worldBridge.PostEvent
            with e ->
                FileLoadError (e.Message, generation) |> worldBridge.PostEvent
        }

    let processWorld (worldBridge : IWorldBridge<AppEvent>) =
        { new WorldProcessor<AppEvent, State> with
            member _.ProcessWorld (changes, _prevVdom, state) =
                let mutable pathInput = state.PathInput
                let mutable cursorPos = state.CursorPos
                let mutable fileContent = state.FileContent
                let mutable isLoading = state.IsLoading
                let mutable generation = state.Generation

                for change in changes do
                    match change with
                    | WorldStateChange.MouseEvent _
                    | WorldStateChange.KeyboardEvent _
                    | WorldStateChange.Keystroke _ -> ()

                    | WorldStateChange.Paste text ->
                        // Handle pasted text by inserting it at the cursor position
                        let newContent, newCursor =
                            TextBoxHelpers.applyAction pathInput cursorPos (InsertString text)

                        pathInput <- newContent
                        cursorPos <- newCursor

                    | WorldStateChange.ApplicationEvent (TextEdit action) ->
                        let newContent, newCursor = TextBoxHelpers.applyAction pathInput cursorPos action
                        pathInput <- newContent
                        cursorPos <- newCursor

                    | WorldStateChange.ApplicationEvent LoadButtonClicked ->
                        if not isLoading && pathInput.Length > 0 then
                            isLoading <- true
                            fileContent <- None
                            generation <- generation + 1
                            loadFileAsync generation worldBridge pathInput |> ignore<Task<unit>>

                    | WorldStateChange.ApplicationEvent (FileLoaded (content, gen)) ->
                        if generation = gen then
                            fileContent <- Some content
                            isLoading <- false

                    | WorldStateChange.ApplicationEvent (FileLoadError (error, gen)) ->
                        if generation = gen then
                            fileContent <- Some $"Error: {error}"
                            isLoading <- false

                    | WorldStateChange.ApplicationEventException e ->
                        ExceptionDispatchInfo.Throw e
                        failwith "unreachable"

                ProcessWorldResult.make
                    {
                        PathInput = pathInput
                        CursorPos = cursorPos
                        FileContent = fileContent
                        IsLoading = isLoading
                        Generation = generation
                    }
        }

    let view (ctx : VdomContext) (state : State) : Vdom<DesiredBounds> =
        let topPane =
            let textBox =
                TextBox.make (ctx, textBoxKey, state.PathInput, state.CursorPos, isInitiallyFocused = true)
                |> Vdom.bordered

            let button = Button.make (ctx, loadButtonKey, "Load")

            Vdom.panelSplitAutoExpand (SplitDirection.Vertical, textBox, button)

        let bottomPane =
            let content =
                match state.IsLoading, state.FileContent with
                | true, _ -> "Loading..."
                | false, Some content -> content
                | false, None -> "Enter a file path and press Load"

            Vdom.textContent content |> Vdom.bordered

        Vdom.panelSplitAuto (SplitDirection.Horizontal, topPane, bottomPane)

    let resolver : ActivationResolver<AppEvent, State> =
        ActivationResolver.combine
            [
                ActivationResolver.textBox textBoxKey TextEdit
                ActivationResolver.button loadButtonKey LoadButtonClicked
            ]

    let run (getEnv : string -> string option) =
        App.run getEnv State.Initial (fun _ -> true) processWorld view resolver

module Program =
    let getEnv (varName : string) : string option =
        match Environment.GetEnvironmentVariable varName with
        | null -> None
        | value -> Some value

    [<EntryPoint>]
    let main _argv =
        (FileBrowser.run getEnv).Wait ()
        0
