namespace WoofWare.Zoomies

open System
open System.IO
open System.Runtime.ExceptionServices
open System.Threading.Tasks
open WoofWare.Incremental
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
                Directory.GetFiles Environment.CurrentDirectory
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

    /// Store world bridge for async operations. Set via OnSetup.
    let worldBridgeRef : IWorldBridge<AppEvent> option ref = ref None

    /// Pure transition function: state -> event -> state
    /// (Note: LoadButtonClicked spawns an async task, which is an effect,
    /// but the state transition itself is deterministic.)
    let transition (state : State) (event : AppEvent) : State =
        match event with
        | SelectFile index ->
            if index >= 0 && index < state.Files.Length then
                { state with
                    SelectedFileIndex = Some index
                }
            else
                state

        | CursorUp ->
            { state with
                ListState = state.ListState.MoveUp state.Files.Length
            }

        | CursorDown ->
            { state with
                ListState = state.ListState.MoveDown state.Files.Length
            }

        | LoadButtonClicked ->
            match state.SelectedFileIndex, !worldBridgeRef with
            | Some index, Some worldBridge when not state.IsLoading && index >= 0 && index < state.Files.Length ->
                let selectedPath = state.Files.[index].FullPath
                let newGeneration = state.Generation + 1
                loadFileAsync newGeneration worldBridge selectedPath |> ignore<Task<unit>>

                { state with
                    IsLoading = true
                    FileContent = None
                    Generation = newGeneration
                }
            | _ -> state

        | FileLoaded (content, gen) ->
            if gen = state.Generation then
                { state with
                    FileContent = Some content
                    IsLoading = false
                }
            else
                state

        | FileLoadError (error, gen) ->
            if gen = state.Generation then
                { state with
                    FileContent = Some $"Error: {error}"
                    IsLoading = false
                }
            else
                state

    /// Convert raw input to app events.
    /// Return Some to inject the event; None to ignore (or let framework handle, e.g. Tab for focus).
    let handleInput (change : WorldStateChange<AppEvent>) : AppEvent option =
        match change with
        | WorldStateChange.ApplicationEvent ev -> Some ev
        | WorldStateChange.ApplicationEventException e ->
            ExceptionDispatchInfo.Throw e
            failwith "unreachable"
        | _ -> None

    /// Handle post-layout events (e.g., viewport info for scroll position).
    let handlePostLayout (event : PostLayoutEvent) (state : State) : State =
        match event with
        | ViewportInfo info ->
            { state with
                ListState = state.ListState.EnsureVisible info.ViewportHeight
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

    let config : AppConfig<State, AppEvent, PostLayoutEvent> =
        {
            Initial = State.Initial
            Transition = transition
            View = App.pureView view
            HandleInput = handleInput
            HandlePostLayout = handlePostLayout
            FocusHandling = FocusHandling.FrameworkManaged
            ActivationResolver = resolver
            OnSetup = fun bridge -> worldBridgeRef.Value <- Some bridge
        }

    let run (getEnv : string -> string option) = App.runWithConfig getEnv config

module Program =
    let getEnv (varName : string) : string option =
        match Environment.GetEnvironmentVariable varName with
        | null -> None
        | value -> Some value

    [<EntryPoint>]
    let main _argv =
        (FileBrowser.run getEnv).Finished.Wait ()
        0
