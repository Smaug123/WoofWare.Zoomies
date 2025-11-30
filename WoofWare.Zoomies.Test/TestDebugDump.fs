namespace WoofWare.Zoomies.Test

open NUnit.Framework
open WoofWare.Expect
open WoofWare.Zoomies

[<TestFixture>]
[<NonParallelizable>]
module TestDebugDump =
    let mutable oldVdomTagging = false

    [<OneTimeSetUp>]
    let setUp () =
        oldVdomTagging <- VdomTagging.Enabled
        VdomTagging.Enabled <- true
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let tearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()
        VdomTagging.Enabled <- oldVdomTagging

    [<Test>]
    let ``debugDump simple text content`` () =
        let vdom = Vdom.textContent false "Hello, World!"
        let dump = Vdom.debugDump vdom

        expect {
            snapshot
                @"Text: ""Hello, World!"" [top-left]
"

            return dump
        }

    [<Test>]
    let ``debugDump text with focus`` () =
        let vdom = Vdom.textContent true "Focused text"
        let dump = Vdom.debugDump vdom

        expect {
            snapshot
                @"Text: ""Focused text"" [top-left] (focused)
"

            return dump
        }

    [<Test>]
    let ``debugDump empty vdom`` () =
        let vdom = Vdom.empty
        let dump = Vdom.debugDump vdom

        expect {
            snapshot
                @"Empty
"

            return dump
        }

    [<Test>]
    let ``debugDump bordered content`` () =
        let inner =
            Vdom.textContent false "Bordered text" |> Vdom.withKey (NodeKey.make "inner")

        let vdom = Vdom.bordered (Vdom.Keyed inner)
        let dump = Vdom.debugDump vdom

        expect {
            snapshot
                @"Bordered:
  Key: inner
    Text: ""Bordered text"" [top-left]
"

            return dump
        }

    [<Test>]
    let ``debugDump panel split with proportion`` () =
        let left = Vdom.textContent false "Left" |> Vdom.withKey (NodeKey.make "left")
        let right = Vdom.textContent false "Right" |> Vdom.withKey (NodeKey.make "right")

        let vdom =
            Vdom.panelSplitProportion (SplitDirection.Vertical, 0.3, Vdom.Keyed left, Vdom.Keyed right)

        let dump = Vdom.debugDump vdom

        expect {
            snapshot
                @"PanelSplit: Vertical Proportion 0.30
  First:
    Key: left
      Text: ""Left"" [top-left]
  Second:
    Key: right
      Text: ""Right"" [top-left]
"

            return dump
        }

    [<Test>]
    let ``debugDump panel split with absolute sizing`` () =
        let top = Vdom.textContent false "Top" |> Vdom.withKey (NodeKey.make "top")
        let bottom = Vdom.textContent false "Bottom" |> Vdom.withKey (NodeKey.make "bottom")

        let vdom =
            Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 5, Vdom.Keyed top, Vdom.Keyed bottom)

        let dump = Vdom.debugDump vdom

        expect {
            snapshot
                @"PanelSplit: Horizontal Absolute 5
  First:
    Key: top
      Text: ""Top"" [top-left]
  Second:
    Key: bottom
      Text: ""Bottom"" [top-left]
"

            return dump
        }

    [<Test>]
    let ``debugDump panel split with auto sizing`` () =
        let left =
            Vdom.textContent false "Auto left" |> Vdom.withKey (NodeKey.make "auto-left")

        let right =
            Vdom.textContent false "Auto right" |> Vdom.withKey (NodeKey.make "auto-right")

        let vdom =
            Vdom.panelSplitAuto (SplitDirection.Vertical, Vdom.Keyed left, Vdom.Keyed right)

        let dump = Vdom.debugDump vdom

        expect {
            snapshot
                @"PanelSplit: Vertical Auto
  First:
    Key: auto-left
      Text: ""Auto left"" [top-left]
  Second:
    Key: auto-right
      Text: ""Auto right"" [top-left]
"

            return dump
        }

    [<Test>]
    let ``debugDump with tags`` () =
        let vdom =
            Vdom.textContent false "Tagged content"
            |> Vdom.withTag "my-component"
            |> Vdom.withKey (NodeKey.make "tagged-node")
            |> KeyedVdom.withTag "outer-tag"

        let dump = Vdom.debugDump vdom

        expect {
            snapshot
                @"Key: tagged-node
  Tag: outer-tag
    Tag: my-component
      Text: ""Tagged content"" [top-left]
"

            return dump
        }

    [<Test>]
    let ``debugDump with focus tracking`` () =
        let inner =
            Vdom.textContent false "Focusable"
            |> Vdom.withKey (NodeKey.make "focusable-key")

        let vdom =
            Vdom.withFocusTracking (inner, isFirstToFocus = true, isInitiallyFocused = true)

        let dump = Vdom.debugDump vdom

        expect {
            snapshot
                @"Focusable [firstToFocus, initiallyFocused]:
  Key: focusable-key
    Text: ""Focusable"" [top-left]
"

            return dump
        }

    [<Test>]
    let ``debugDump complex nested structure`` () =
        let title =
            Vdom.textContent false "Title"
            |> Vdom.withTag "title"
            |> Vdom.withKey (NodeKey.make "title-key")
            |> Vdom.Keyed
            |> Vdom.bordered

        let content =
            Vdom.textContent false "Content area"
            |> Vdom.withTag "content"
            |> Vdom.withKey (NodeKey.make "content-key")

        let footer =
            Vdom.textContent false "Footer"
            |> Vdom.withTag "footer"
            |> Vdom.withKey (NodeKey.make "footer-key")
            |> Vdom.withFocusTracking

        let body =
            Vdom.panelSplitProportion (SplitDirection.Horizontal, 0.8, Vdom.Keyed content, footer)

        let vdom = Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 3, title, body)
        let dump = Vdom.debugDump vdom

        expect {
            snapshot
                @"PanelSplit: Horizontal Absolute 3
  First:
    Bordered:
      Key: title-key
        Tag: title
          Text: ""Title"" [top-left]
  Second:
    PanelSplit: Horizontal Proportion 0.80
      First:
        Key: content-key
          Tag: content
            Text: ""Content area"" [top-left]
      Second:
        Focusable:
          Key: footer-key
            Tag: footer
              Text: ""Footer"" [top-left]
"

            return dump
        }
