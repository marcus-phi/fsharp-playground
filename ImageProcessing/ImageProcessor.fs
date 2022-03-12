namespace ImageProcessing

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Media.Imaging

module ImageProcessor =

    type Message = FileSelected of Bitmap

    type State = { SelectedImage: Bitmap }

    let init () = { SelectedImage = null }

    let update msg state =
        match msg with
        | FileSelected file -> { state with SelectedImage = file }

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
                            Button.dock Dock.Top
                            Button.horizontalAlignment HorizontalAlignment.Center
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

        DockPanel.create [ DockPanel.lastChildFill true
                           DockPanel.children [ selectFileBtn
                                                if isNull state.SelectedImage then
                                                    emptyPrompt :> IView
                                                else
                                                    imageBox state.SelectedImage :> IView ] ]
