namespace WoofWare.Zoomies

open System
open System.IO
open System.Runtime.ExceptionServices
open System.Threading.Tasks
open WoofWare.Zoomies
open WoofWare.Zoomies.Components

/// Events from user input (via ActivationResolver) or async operations (via WorldBridge)
type AppEvent =
    | SelectFile of index : int
    | CursorUp
    | CursorDown
    | LoadButtonClicked
    | FileLoaded of content : string * generation : int
    | FileLoadError of error : string * generation : int

/// Events posted during rendering (via ctx.PostLayoutEvent)
type PostLayoutEvent = | ViewportInfo of SelectionListViewportInfo

type FileEntry =
    {
        Name : string
        FullPath : string
        Key : NodeKey
    }

type State =
    {
        Files : FileEntry[]
        SelectedFileIndex : int option
        FileContent : string option
        IsLoading : bool
        Generation : int
        ListState : SelectionListState
    }

    static member Initial =
        let files =
            try
                Directory.GetFiles (Environment.CurrentDirectory)
                |> Array.map (fun path ->
                    let name = Path.GetFileName path

                    {
                        Name = name
                        FullPath = path
                        Key = NodeKey.make $"file-{name}"
                    }
                )
                |> Array.sortBy _.Name
            with _ ->
                [||]

        {
            Files = files
            SelectedFileIndex = None
            FileContent = None
            IsLoading = false
            Generation = 0
            ListState = SelectionListState.AtStart
        }

module FileBrowser =
    let fileListKey = NodeKey.make "file-list"
    let loadButtonKey = NodeKey.make "loadButton"

    let loadFileAsync (generation : int) (worldBridge : IWorldBridge<_>) (filepath : string) : unit Task =
        task {
            try
                let! content = File.ReadAllTextAsync filepath
                return FileLoaded (content, generation) |> worldBridge.PostEvent
            with e ->
                return FileLoadError (e.Message, generation) |> worldBridge.PostEvent
        }

    let processWorld (worldBridge : IWorldBridge<AppEvent>) =
        { new WorldProcessor<AppEvent, PostLayoutEvent, State> with
            member _.ProcessWorld (changes, _prevVdom, state) =
                let mutable selectedFileIndex = state.SelectedFileIndex
                let mutable fileContent = state.FileContent
                let mutable isLoading = state.IsLoading
                let mutable generation = state.Generation
                let mutable listState = state.ListState

                for change in changes do
                    match change with
                    | WorldStateChange.MouseEvent _
                    | WorldStateChange.Keystroke _
                    | WorldStateChange.Paste _ -> ()

                    | WorldStateChange.ApplicationEvent (SelectFile index) ->
                        // Select the file at the given index
                        if index >= 0 && index < state.Files.Length then
                            selectedFileIndex <- Some index

                    | WorldStateChange.ApplicationEvent CursorUp -> listState <- listState.MoveUp state.Files.Length

                    | WorldStateChange.ApplicationEvent CursorDown -> listState <- listState.MoveDown state.Files.Length

                    | WorldStateChange.ApplicationEvent LoadButtonClicked ->
                        match selectedFileIndex with
                        | Some index when not isLoading && index >= 0 && index < state.Files.Length ->
                            let selectedPath = state.Files.[index].FullPath
                            isLoading <- true
                            fileContent <- None
                            generation <- generation + 1
                            loadFileAsync generation worldBridge selectedPath |> ignore<Task<unit>>
                        | _ -> ()

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
                        Files = state.Files
                        SelectedFileIndex = selectedFileIndex
                        FileContent = fileContent
                        IsLoading = isLoading
                        Generation = generation
                        ListState = listState
                    }

            member _.ProcessPostLayoutEvents (events, _ctx, state) =
                let mutable listState = state.ListState

                for event in events do
                    match event with
                    | ViewportInfo info -> listState <- listState.EnsureVisible info.ViewportHeight

                { state with
                    ListState = listState
                }
        }

    let view (ctx : IVdomContext<PostLayoutEvent>) (state : State) : Vdom<DesiredBounds> =
        let leftPane =
            let title = Vdom.textContent "Files in current directory:"

            let fileList =
                if Array.isEmpty state.Files then
                    Vdom.textContent "(no files found)"
                else
                    let items =
                        state.Files
                        |> Array.map (fun entry ->
                            {
                                Id = entry.Key
                                Label = entry.Name
                            }
                        )

                    (SingleSelection.make (
                        ctx,
                        fileListKey,
                        items,
                        state.SelectedFileIndex,
                        state.ListState,
                        ViewportInfo,
                        isFirstToFocus = true
                    ))
                        .Vdom

            let buttonLabel =
                match state.SelectedFileIndex with
                | None -> "Select a file"
                | Some _ -> "Load selected file"

            let button = Button.make (ctx, loadButtonKey, buttonLabel)

            Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 1, title, fileList)
            |> fun content -> Vdom.panelSplitAbsolute (SplitDirection.Horizontal, -1, content, button)
            |> Vdom.bordered

        let rightPane =
            let content =
                match state.IsLoading, state.FileContent, state.SelectedFileIndex with
                | true, _, _ -> "Loading..."
                | false, Some content, _ -> content
                | false, None, None -> "Select a file and press the button to view its contents."
                | false, None, Some _ -> "Press the button to load the selected file."

            Vdom.textContent content |> Vdom.bordered

        Vdom.panelSplitProportion (SplitDirection.Vertical, 0.3, leftPane, rightPane)

    let resolver : ActivationResolver<AppEvent, State> =
        ActivationResolver.combine
            [
                ActivationResolver.selectionList
                    fileListKey
                    (fun s -> s.ListState.CursorIndex)
                    CursorUp
                    CursorDown
                    SelectFile
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
        (FileBrowser.run getEnv).Finished.Wait ()
        0
