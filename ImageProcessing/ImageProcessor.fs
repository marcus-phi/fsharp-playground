namespace ImageProcessing

open Avalonia.Controls
open Avalonia.FuncUI.Components
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Media.Imaging

module ImageProcessor =

    type Filter =
        | None
        | BlackWhite
        | Blur

    type Message =
        | FileSelected of Bitmap
        | FilterSelected of Filter

    type State =
        { SelectedImage: Bitmap
          SelectedFilter: Filter }

    let init () =
        { SelectedImage = null
          SelectedFilter = None }

    let update msg state =
        match msg with
        | FileSelected file -> { state with SelectedImage = file }
        | FilterSelected filter -> { state with SelectedFilter = filter }

    let view host (state: State) dispatch =

        let openImageDialog =
            async {
                let dlg = OpenFileDialog()
                dlg.AllowMultiple <- false

                let! file = dlg.ShowAsync(host) |> Async.AwaitTask

                try
                    let bmp = new Bitmap(file[0])
                    dispatch (FileSelected bmp)
                with
                | _ -> ()
            }

        let selectFileBtn =
            Button.create [ Button.content "Select file..."
                            Button.horizontalAlignment HorizontalAlignment.Stretch
                            Button.onClick (fun _ -> Async.Start openImageDialog) ]

        let emptyPrompt =
            TextBlock.create [ TextBlock.text "No image selected"
                               TextBlock.horizontalAlignment HorizontalAlignment.Center
                               TextBlock.verticalAlignment VerticalAlignment.Center ]

        let imageBox image =
            Image.create [ Image.source image
                           Image.horizontalAlignment HorizontalAlignment.Stretch
                           Image.verticalAlignment VerticalAlignment.Stretch
                           Image.stretch Stretch.Uniform ]

        let filterList =
            let nameMap =
                Map [ (None, "None")
                      (BlackWhite, "Black & White")
                      (Blur, "Blur") ]

            let itemTemplate item =
                TextBlock.create [ TextBlock.text nameMap.[item] ]

            ListBox.create [ ListBox.dataItems [ None
                                                 BlackWhite
                                                 Blur ]
                             ListBox.selectedItem None
                             ListBox.itemTemplate (DataTemplateView<Filter>.create (fun item -> itemTemplate item))
                             ListBox.onSelectedItemChanged (fun item -> dispatch (FilterSelected(item :?> Filter))) ]

        let panel =
            StackPanel.create [ StackPanel.dock Dock.Left
                                StackPanel.width 150
                                StackPanel.background "#222222"
                                StackPanel.children [ selectFileBtn
                                                      filterList ] ]

        DockPanel.create [ DockPanel.lastChildFill true
                           DockPanel.children [ panel
                                                if isNull state.SelectedImage then
                                                    emptyPrompt :> IView
                                                else
                                                    imageBox state.SelectedImage :> IView ] ]
