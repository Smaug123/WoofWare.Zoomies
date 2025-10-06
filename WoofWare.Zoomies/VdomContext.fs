namespace WoofWare.Zoomies

[<Struct>]
type Rectangle =
    {
        TopLeftX : int
        TopLeftY : int
        Width : int
        Height : int
    }

/// Context provided to vdom construction, containing information about the layout of the previous render cycle.
/// This is mutable (although you aren't given the tools to mutate it), so don't persist it.
type VdomContext =
    private
        {
            mutable _FocusedKey : NodeKey option
            mutable _TerminalBounds : Rectangle
            mutable IsDirty : bool
        }

[<RequireQualifiedAccess>]
module VdomContext =
    let internal empty (terminalBounds : Rectangle) =
        {
            _TerminalBounds = terminalBounds
            _FocusedKey = None
            IsDirty = true
        }

    let internal setFocusedKey (key : NodeKey option) (v : VdomContext) =
        if v._FocusedKey <> key then
            v.IsDirty <- true
            v._FocusedKey <- key

    let internal setTerminalBounds (tb : Rectangle) (v : VdomContext) =
        if v._TerminalBounds <> tb then
            v.IsDirty <- true
            v._TerminalBounds <- tb

    let internal markClean (v : VdomContext) = v.IsDirty <- false

    /// Get the dimensions of the terminal (on the previous render).
    let terminalBounds (v : VdomContext) : Rectangle = v._TerminalBounds
    /// Get the NodeKey of the Vdom element, if any, which was focused in the last render.
    /// If you're not using the automatic focus handling mechanism, this is always None.
    let focusedKey (v : VdomContext) : NodeKey option = v._FocusedKey
