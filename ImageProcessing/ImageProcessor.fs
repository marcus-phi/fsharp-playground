namespace ImageProcessing

open Avalonia.FuncUI.DSL

module ImageProcessor =

    type Image = Image of string
    type Filter = Filter of string

    type State = { Image: Image; Filter: Filter }

    let init =
        { Image = Image("")
          Filter = Filter("") }

    let update msg state = state

    let view stage dispatch = DockPanel.create []
