namespace WoofWare.Zoomies

open System
open System.IO
open System.Runtime.ExceptionServices
open System.Threading.Tasks
open WoofWare.Zoomies
open WoofWare.Zoomies.Components

type AppEvent =
    | ToggleFile of NodeKey
    | LoadButtonClicked
    | FileLoaded of content : string * generation : int
    | FileLoadError of error : string * generation : int

type FileEntry =
    {
        Name : string
        FullPath : string
        Key : NodeKey
    }

type State =
    {
        Files : FileEntry[]
        SelectedFiles : Set<string>
        FileContent : string option
        IsLoading : bool
        Generation : int
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
            SelectedFiles = Set.empty
            FileContent = None
            IsLoading = false
            Generation = 0
        }

module FileBrowser =
    let multiSelectPrefix = NodeKey.make "file-list"
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
                let mutable selectedFiles = state.SelectedFiles
                let mutable fileContent = state.FileContent
                let mutable isLoading = state.IsLoading
                let mutable generation = state.Generation

                for change in changes do
                    match change with
                    | WorldStateChange.MouseEvent _
                    | WorldStateChange.Keystroke _
                    | WorldStateChange.Paste _ -> ()

                    | WorldStateChange.ApplicationEvent (ToggleFile key) ->
                        // Find the file entry with this key
                        let fileEntry = state.Files |> Array.tryFind (fun f -> f.Key = key)

                        match fileEntry with
                        | Some entry ->
                            if Set.contains entry.FullPath selectedFiles then
                                selectedFiles <- Set.remove entry.FullPath selectedFiles
                            else
                                selectedFiles <- Set.add entry.FullPath selectedFiles
                        | None -> ()

                    | WorldStateChange.ApplicationEvent LoadButtonClicked ->
                        if not isLoading && Set.count selectedFiles = 1 then
                            let selectedPath = Set.minElement selectedFiles
                            isLoading <- true
                            fileContent <- None
                            generation <- generation + 1
                            loadFileAsync generation worldBridge selectedPath |> ignore<Task<unit>>

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
                        SelectedFiles = selectedFiles
                        FileContent = fileContent
                        IsLoading = isLoading
                        Generation = generation
                    }
        }

    let view (ctx : VdomContext) (state : State) : Vdom<DesiredBounds> =
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
                                IsSelected = Set.contains entry.FullPath state.SelectedFiles
                            }
                        )

                    MultiSelection.make (ctx, multiSelectPrefix, items, isFirstToFocus = true)

            let buttonLabel =
                match Set.count state.SelectedFiles with
                | 0 -> "Select a file"
                | 1 -> "Load selected file"
                | n -> $"{n} files selected"

            let button = Button.make (ctx, loadButtonKey, buttonLabel)

            Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 1, title, fileList)
            |> fun content -> Vdom.panelSplitAbsolute (SplitDirection.Horizontal, -1, content, button)
            |> Vdom.bordered

        let rightPane =
            let content =
                match state.IsLoading, state.FileContent, Set.count state.SelectedFiles with
                | true, _, _ -> "Loading..."
                | false, Some content, _ -> content
                | false, None, 0 -> "Select a file and press the button to view its contents."
                | false, None, 1 -> "Press the button to load the selected file."
                | false, None, n -> $"You have {n} files selected. Select exactly one to view its contents."

            Vdom.textContent content |> Vdom.bordered

        Vdom.panelSplitProportion (SplitDirection.Vertical, 0.3, leftPane, rightPane)

    let resolver : ActivationResolver<AppEvent, State> =
        // File keys are determined at startup and don't change
        let fileKeys = State.Initial.Files |> Array.map _.Key

        ActivationResolver.combine
            [
                ActivationResolver.multiSelection fileKeys ToggleFile
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
