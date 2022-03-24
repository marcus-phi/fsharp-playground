namespace ImageProcessing

open System
open System.Diagnostics
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
        | NoFilter
        | Grayscale
        | Blur

    type Message =
        | FileSelected of Bitmap
        | FilterSelected of Filter
        | ParallelToggled

    type State =
        { SelectedImage: Bitmap
          SelectedFilter: Filter
          ProcessedImage: Bitmap
          ProcessingTimeMs: int64 option
          IsParallel: bool }

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

    let filterGrayscale isParallel (imagePixels: SKColor []) =
        let f =
            fun (c: SKColor) ->
                let newC =
                    (float32 (
                        (int c.Red) * 11
                        + (int c.Green) * 59
                        + (int c.Blue) * 30
                    ))
                    / 100.f
                    |> floor
                    |> byte

                SKColor(newC, newC, newC)

        if isParallel then
            imagePixels |> Array.Parallel.map f
        else
            imagePixels |> Array.map f

    let filterBlur (width: int, height: int) kernel isParallel (imagePixels: SKColor []) =
        let f =
            fun idx ->
                let x, y, half = idx % width, idx / height, kernel / 2
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
                )

        if isParallel then
            imagePixels
            |> Array.Parallel.mapi (fun i c -> f i)
        else
            imagePixels |> Array.mapi (fun i c -> f i)


    let processImage (image: Bitmap) filter isParallel =
        let watch = Stopwatch()
        watch.Start()

        let processedImage, processingTime =
            match filter with
            | NoFilter -> image, None
            | Grayscale ->
                image
                |> processPixels (filterGrayscale isParallel),
                Some(watch.ElapsedMilliseconds)
            | Blur ->
                image
                |> processPixels (filterBlur (image.PixelSize.Width, image.PixelSize.Height) 11 isParallel),
                Some(watch.ElapsedMilliseconds)

        watch.Stop()
        processedImage, processingTime


    let init () =
        { SelectedImage = null
          SelectedFilter = NoFilter
          ProcessedImage = null
          ProcessingTimeMs = None
          IsParallel = false }

    let update msg state =
        match msg with
        | FileSelected file ->
            let processedImage, processingTime =
                processImage file state.SelectedFilter state.IsParallel

            { state with
                SelectedImage = file
                ProcessedImage = processedImage
                ProcessingTimeMs = processingTime }
        | FilterSelected filter ->
            if isNull state.SelectedImage then
                { state with SelectedFilter = filter }
            else
                let processedImage, processingTime =
                    processImage state.SelectedImage filter state.IsParallel

                { state with
                    SelectedFilter = filter
                    ProcessedImage = processedImage
                    ProcessingTimeMs = processingTime }
        | ParallelToggled ->
            if isNull state.SelectedImage then
                { state with IsParallel = not state.IsParallel }
            else
                let isParallel = not state.IsParallel

                let processedImage, processingTime =
                    processImage state.SelectedImage state.SelectedFilter isParallel

                { state with
                    ProcessedImage = processedImage
                    ProcessingTimeMs = processingTime
                    IsParallel = isParallel }


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

        let toggleParallelBtn =
            ToggleButton.create [ Button.content "Parallel"
                                  Button.horizontalAlignment HorizontalAlignment.Stretch
                                  Button.onClick (fun _ -> dispatch ParallelToggled) ]

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

        let splitImageBox imageLeft imageRight processingTimeMs =
            let timePrompt =
                match processingTimeMs with
                | Some ms -> TimeSpan.FromMilliseconds(float ms).ToString("g")
                | None -> "N/A"

            Grid.create [ Grid.columnDefinitions "*, 2, *"
                          Grid.children [ imageBox imageLeft (Image.column 0)
                                          Rectangle.create [ Shapes.Rectangle.fill "#cccccc"
                                                             Shapes.Rectangle.column 1 ]
                                          imageBox imageRight (Image.column 2)
                                          TextBlock.create [ TextBlock.text $"Processing time: {timePrompt}"
                                                             TextBlock.background "#88333333"
                                                             TextBlock.column 2
                                                             TextBlock.padding (7., 5.)
                                                             TextBlock.horizontalAlignment HorizontalAlignment.Center
                                                             TextBlock.verticalAlignment VerticalAlignment.Top ] ] ]

        let filterList =
            let nameMap =
                Map [ (NoFilter, "None")
                      (Grayscale, "Grayscale")
                      (Blur, "Blur") ]

            let itemTemplate item =
                TextBlock.create [ TextBlock.text nameMap.[item] ]

            ListBox.create [ ListBox.dataItems [ NoFilter
                                                 Grayscale
                                                 Blur ]
                             ListBox.selectedItem NoFilter
                             ListBox.itemTemplate (DataTemplateView<Filter>.create (fun item -> itemTemplate item))
                             ListBox.onSelectedItemChanged (fun item -> dispatch (FilterSelected(item :?> Filter))) ]

        let panel =
            StackPanel.create [ StackPanel.dock Dock.Left
                                StackPanel.width 150
                                StackPanel.background "#222222"
                                StackPanel.children [ selectFileBtn
                                                      toggleParallelBtn
                                                      filterList ] ]

        DockPanel.create [ DockPanel.lastChildFill true
                           DockPanel.children [ panel
                                                if isNull state.SelectedImage then
                                                    emptyPrompt :> IView
                                                else
                                                    splitImageBox
                                                        state.SelectedImage
                                                        state.ProcessedImage
                                                        state.ProcessingTimeMs
                                                    :> IView ] ]
