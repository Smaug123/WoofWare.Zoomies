namespace WoofWare.Zoomies

open System
open System.Collections.Generic

/// So that we can do early cutoff.
type RenderedNode =
    internal
        {
            Bounds : Rectangle
            OverlaidChildren : RenderedNode list
            VDomSource : Vdom<DesiredBounds>
            Self : Vdom<Rectangle>
            ArrangedSource : Layout.ArrangedNode option
        }

type RenderState =
    private
        {
            Console : IConsole
            mutable PreviousVdom : RenderedNode option
            mutable Buffer : TerminalCell voption[,]
            mutable CursorVisible : bool
            Output : TerminalOp -> unit
            mutable BackgroundColor : ConsoleColor
            mutable ForegroundColor : ConsoleColor
            KeyToNode : Dictionary<NodeKey, RenderedNode>
            /// List of focusable keys in tree order (for Tab navigation)
            FocusableKeys : OrderedSet<NodeKey>
            /// The key marked with isFirstToFocus=true, if any
            FirstToFocusKey : NodeKey option ref
            /// The key marked with isInitiallyFocused=true, if any
            InitiallyFocusedKey : NodeKey option ref
            /// This gets handed out to users every so often: it's the fragment of state that they will want to
            /// construct the vdom with.
            VdomContext : VdomContext
            /// Debug file writer for layout diagnostics (if WOOFWARE_ZOOMIES_DEBUG_TO_FILE is enabled)
            DebugWriter : IO.StreamWriter option
        }

    interface IDisposable with
        member this.Dispose () =
            match this.DebugWriter with
            | Some writer -> writer.Dispose ()
            | None -> ()

[<RequireQualifiedAccess>]
module RenderState =
    let isCursorVisible (s : RenderState) = s.CursorVisible

    let setCursorVisible (s : RenderState) =
        s.Output (TerminalOp.SetCursorVisibility true)
        s.CursorVisible <- true

    let setCursorInvisible (s : RenderState) =
        s.Output (TerminalOp.SetCursorVisibility false)
        s.CursorVisible <- false

    let clearScreen (s : RenderState) = s.Output TerminalOp.ClearScreen

    let enterAlternateScreen (s : RenderState) =
        s.Output TerminalOp.EnterAlternateScreen

    let exitAlternateScreen (s : RenderState) = s.Output TerminalOp.ExitAlternateScreen

    let registerMouseMode (s : RenderState) = s.Output TerminalOp.RegisterMouseMode
    let unregisterMouseMode (s : RenderState) = s.Output TerminalOp.UnregisterMouseMode

    let registerBracketedPaste (s : RenderState) =
        s.Output TerminalOp.RegisterBracketedPaste

    let unregisterBracketedPaste (s : RenderState) =
        s.Output TerminalOp.UnregisterBracketedPaste

    /// Query the rendered bounds of a keyed node
    let layoutOf (key : NodeKey) (s : RenderState) : Rectangle option =
        match s.KeyToNode.TryGetValue key with
        | true, node -> Some node.Bounds
        | false, _ -> None

    let private getBounds (c : IConsole) : Rectangle =
        let width = c.WindowWidth ()
        let height = c.WindowHeight ()

        {
            TopLeftX = 0
            TopLeftY = 0
            Width = width
            Height = height
        }

    let refreshTerminalSize (rs : RenderState) : unit =
        VdomContext.setTerminalBounds (getBounds rs.Console) rs.VdomContext

    /// Advance focus to the next focusable node (Tab key)
    let advanceFocus (s : RenderState) : unit =
        if s.FocusableKeys.Count = 0 then
            // nothing to do, nothing can ever have focus
            VdomContext.setFocusedKey None s.VdomContext
        else

        match VdomContext.focusedKey s.VdomContext with
        | None ->
            // No current focus, use first-to-focus key if available, otherwise first focusable element
            match s.FirstToFocusKey.Value with
            | Some firstKey when s.FocusableKeys.Contains firstKey ->
                VdomContext.setFocusedKey (Some firstKey) s.VdomContext
            | _ -> VdomContext.setFocusedKey (Some s.FocusableKeys.[0]) s.VdomContext
        | Some currentKey ->
            // Find the current key in the list and move to the next one
            match s.FocusableKeys |> Seq.tryFindIndex ((=) currentKey) with
            | Some index ->
                let nextIndex = (index + 1) % s.FocusableKeys.Count
                VdomContext.setFocusedKey (Some s.FocusableKeys.[nextIndex]) s.VdomContext
            | None ->
                // Current key is not in the focusable list, use first-to-focus key if available
                match s.FirstToFocusKey.Value with
                | Some firstKey when s.FocusableKeys.Contains firstKey ->
                    VdomContext.setFocusedKey (Some firstKey) s.VdomContext
                | _ -> VdomContext.setFocusedKey (Some s.FocusableKeys.[0]) s.VdomContext

    /// Retreat focus to the previous focusable node (Shift+Tab key)
    let retreatFocus (s : RenderState) : unit =
        if s.FocusableKeys.Count = 0 then
            // nothing to do, nothing can ever have focus
            VdomContext.setFocusedKey None s.VdomContext
        else

        match VdomContext.focusedKey s.VdomContext with
        | None ->
            // No current focus, use first-to-focus key if available, otherwise last focusable element
            match s.FirstToFocusKey.Value with
            | Some firstKey when s.FocusableKeys.Contains firstKey ->
                VdomContext.setFocusedKey (Some firstKey) s.VdomContext
            | _ -> VdomContext.setFocusedKey (Some s.FocusableKeys.[s.FocusableKeys.Count - 1]) s.VdomContext
        | Some currentKey ->
            // Find the current key in the list and move to the previous one
            match s.FocusableKeys |> Seq.tryFindIndex ((=) currentKey) with
            | Some index ->
                let prevIndex = (index - 1 + s.FocusableKeys.Count) % s.FocusableKeys.Count
                VdomContext.setFocusedKey (Some s.FocusableKeys.[prevIndex]) s.VdomContext
            | None ->
                // Current key is not in the focusable list, use first-to-focus key if available
                match s.FirstToFocusKey.Value with
                | Some firstKey when s.FocusableKeys.Contains firstKey ->
                    VdomContext.setFocusedKey (Some firstKey) s.VdomContext
                | _ -> VdomContext.setFocusedKey (Some s.FocusableKeys.[s.FocusableKeys.Count - 1]) s.VdomContext

    let internal vdomContext (rs : RenderState) = rs.VdomContext

    /// Get the currently focused key, if any
    let focusedKey (rs : RenderState) : NodeKey option = VdomContext.focusedKey rs.VdomContext

    let internal make (c : IConsole) (getUtcNow : unit -> DateTime) (debugWriter : IO.StreamWriter option) =
        let bounds = getBounds c

        let changeBuffer = Array2D.zeroCreate bounds.Height bounds.Width

        let bg = c.BackgroundColor ()
        let fg = c.ForegroundColor ()

        {
            Console = c
            Buffer = changeBuffer
            PreviousVdom = None
            Output = c.Execute
            CursorVisible = true
            BackgroundColor = bg
            ForegroundColor = fg
            KeyToNode = Dictionary<NodeKey, RenderedNode> ()
            FocusableKeys = OrderedSet ()
            FirstToFocusKey = ref None
            InitiallyFocusedKey = ref None
            VdomContext = VdomContext.empty getUtcNow bounds
            DebugWriter = debugWriter
        }

    // Internal accessors for Render module
    let internal previousVdom (rs : RenderState) = rs.PreviousVdom
    let internal setPreviousVdom (v : RenderedNode option) (rs : RenderState) = rs.PreviousVdom <- v
    let internal buffer (rs : RenderState) = rs.Buffer
    let internal setBuffer (b : TerminalCell voption[,]) (rs : RenderState) = rs.Buffer <- b
    let internal keyToNode (rs : RenderState) = rs.KeyToNode
    let internal focusableKeys (rs : RenderState) = rs.FocusableKeys
    let internal firstToFocusKey (rs : RenderState) = rs.FirstToFocusKey
    let internal initiallyFocusedKey (rs : RenderState) = rs.InitiallyFocusedKey
    let internal debugWriter (rs : RenderState) = rs.DebugWriter
    let internal output (rs : RenderState) = rs.Output
