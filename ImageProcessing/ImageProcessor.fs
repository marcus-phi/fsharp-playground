namespace ImageProcessing

open System.IO
open Avalonia.Controls
open Avalonia.FuncUI.Components
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Media.Imaging
open SkiaSharp

module ImageProcessor =

    type Filter =
        | None
        | Grayscale
        | Blur

    type Message =
        | FileSelected of Bitmap
        | FilterSelected of Filter

    type State =
        { SelectedImage: Bitmap
          SelectedFilter: Filter
          ProcessedImage: Bitmap }

    let processPixels (filter: SKColor [] -> SKColor []) (image: Bitmap) =
        let convertToSKBitmap (bitmap: Bitmap) =
            use stream = new MemoryStream()
            bitmap.Save(stream)
            SKBitmap.Decode(stream.ToArray())

        let convertToBitmap (skBitmap: SKBitmap) =
            let skImg = SKImage.FromBitmap(skBitmap)
            new Bitmap(skImg.Encode().AsStream())

        let skBitmap = convertToSKBitmap image
        skBitmap.Pixels <- filter skBitmap.Pixels
        convertToBitmap skBitmap

    let filterGrayscale (imagePixels: SKColor []) =
        imagePixels
        |> Array.map (fun c ->
            let newC =
                (float32 (
                    (int c.Red) * 11
                    + (int c.Green) * 59
                    + (int c.Blue) * 30
                ))
                / 100.f
                |> floor
                |> byte

            SKColor(newC, newC, newC))

    let filterBlur (width: int, height: int) kernel (imagePixels: SKColor []) =
        imagePixels
        |> Array.mapi (fun i c ->
            let x, y, half = i % width, i / height, kernel / 2
            let rx, ry = [| x - half .. x + half |], [| y - half .. y + half |]

            let neighbors =
                Array.allPairs rx ry
                |> Array.map (fun (nx, ny) ->
                    if nx < 0 || ny < 0 || nx >= width || ny >= height then
                        SKColor()
                    else
                        imagePixels[ny * height + nx])

            let (sr, sg, sb) =
                neighbors
                |> Array.fold (fun (r, g, b) c -> (r + int c.Red, g + int c.Green, b + int c.Blue)) (0, 0, 0)

            let sq = float32 ((half * 2 + 1) * (half * 2 + 1))

            SKColor(
                float32 sr / sq |> floor |> byte,
                float32 sg / sq |> floor |> byte,
                float32 sb / sq |> floor |> byte
            ))


    let processImage (image: Bitmap) filter =
        match filter with
        | None -> image
        | Grayscale -> image |> processPixels filterGrayscale
        | Blur ->
            image
            |> processPixels (filterBlur (image.PixelSize.Width, image.PixelSize.Height) 11)


    let init () =
        { SelectedImage = null
          SelectedFilter = None
          ProcessedImage = null }

    let update msg state =
        match msg with
        | FileSelected file ->
            { state with
                SelectedImage = file
                ProcessedImage = processImage file state.SelectedFilter }
        | FilterSelected filter ->
            if isNull state.SelectedImage then
                { state with SelectedFilter = filter }
            else
                { state with
                    SelectedFilter = filter
                    ProcessedImage = processImage state.SelectedImage filter }

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

        let imageBox image attrs =
            Image.create [ Image.source image
                           Image.horizontalAlignment HorizontalAlignment.Stretch
                           Image.verticalAlignment VerticalAlignment.Stretch
                           Image.stretch Stretch.UniformToFill
                           attrs ]

        let splitImageBox imageLeft imageRight =
            Grid.create [ Grid.columnDefinitions "*, 2, *"
                          Grid.children [ imageBox imageLeft (Image.column 0) :> IView
                                          Rectangle.create [ Shapes.Rectangle.fill "#cccccc"
                                                             Shapes.Rectangle.column 1 ]
                                          imageBox imageRight (Image.column 2) :> IView ] ]

        let filterList =
            let nameMap =
                Map [ (None, "None")
                      (Grayscale, "Grayscale")
                      (Blur, "Blur") ]

            let itemTemplate item =
                TextBlock.create [ TextBlock.text nameMap.[item] ]

            ListBox.create [ ListBox.dataItems [ None
                                                 Grayscale
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
                                                    splitImageBox state.SelectedImage state.ProcessedImage :> IView ] ]
