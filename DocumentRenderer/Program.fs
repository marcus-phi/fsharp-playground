open SkiaSharp
open System.IO

type Rect =
    { Left: float32
      Top: float32
      Width: float32
      Height: float32 }

type TextContent = { Text: string; Font: SKFont }

type ScreenElement =
    | TextElement of TextContent * Rect
    | ImageElement of string * Rect

type Orientation =
    | Vertical
    | Horizontal

type DocumentPart =
    | SplitPart of Orientation * DocumentPart List
    | TitledPart of TextContent * DocumentPart
    | TextPart of TextContent
    | ImagePart of string

let rec documentToScreen doc rc =
    match doc with
    | SplitPart (Vertical, parts) ->
        let divH = rc.Height / (float32 parts.Length)

        parts
        |> List.mapi (fun i part ->
            let rc =
                { rc with
                    Height = divH
                    Top = rc.Top + (float32 i) * divH }

            documentToScreen part rc)
        |> List.concat
    | SplitPart (Horizontal, parts) ->
        let divW = rc.Width / (float32 parts.Length)

        parts
        |> List.mapi (fun i part ->
            let rc =
                { rc with
                    Width = divW
                    Left = rc.Left + (float32 i) * divW }

            documentToScreen part rc)
        |> List.concat

    | TitledPart (tc, doc) ->
        let titleRc = { rc with Height = 35f }

        let restRc =
            { rc with
                Height = rc.Height - 35f
                Top = rc.Top + 35f }

        TextElement(tc, titleRc)
        :: (documentToScreen doc restRc)
    | TextPart (tc) -> [ TextElement(tc, rc) ]
    | ImagePart (img) -> [ ImageElement(img, rc) ]

let deflate rc dw dh =
    { Left = rc.Left + dw
      Top = rc.Top + dh
      Width = rc.Width - dw * 2f
      Height = rc.Height - dh * 2f }

let toSKRect rc =
    SKRect(rc.Left, rc.Top, rc.Left + rc.Width, rc.Top + rc.Height)

let drawString (str: string) x y font brush (canvas: SKCanvas) =
    str.Split "\n"
    |> Seq.fold
        (fun y line ->
            let blob = SKTextBlob.Create(line, font)
            canvas.DrawText(blob, x, y + blob.Bounds.Height, brush)
            y + blob.Bounds.Height)
        y
    |> ignore

let drawElements elements borderDebug (canvas: SKCanvas) =
    use brush = new SKPaint(Color = SKColors.Black)
    use debugBrush = new SKPaint(Color = SKColors.Red, IsStroke = true)

    elements
    |> List.map (fun e ->
        match e with
        | TextElement (tc, rc) ->
            drawString tc.Text rc.Left rc.Top tc.Font brush canvas
            canvas.DrawRect(toSKRect rc, debugBrush)
        | ImageElement (filename, rc) ->
            let dw, dh = rc.Width / 10f, rc.Height / 10f
            let rc = toSKRect (deflate rc dw dh)
            use bmp = SKBitmap.Decode(filename)
            canvas.DrawBitmap(bmp, rc)
            canvas.DrawRect(rc, debugBrush))

    |> ignore

let drawImage (w, h) space f =
    use surface = SKSurface.Create(SKImageInfo(w, h))
    let canvas = surface.Canvas
    canvas.Clear SKColors.White
    canvas.Translate(space, space)
    f canvas
    surface.Snapshot()

let main =
    use font = SKTypeface.FromFamilyName "Arial"
    use textFont = new SKFont(font, 12f)
    use headFont = new SKFont(font, 15f)

    let doc =
        TitledPart(
            { Text = "Functional programming in .NET"
              Font = headFont },
            SplitPart(
                Vertical,
                [ ImagePart("cover.jpg")
                  TextPart(
                      { Text =
                          [ "In this book, we'll introduce you to the essential"
                            "concepts of functional programming, but thanks to the .NET"
                            "framework, we won't be limited to theoretical examples and we"
                            "will use many of the rich .NET libraries to show how functional"
                            "programming can be used in a real-world." ]
                          |> String.concat "\n"
                        Font = textFont }
                  ) ]
            )
        )

    let elements =
        documentToScreen
            doc
            { Left = 0f
              Top = 0f
              Width = 500f
              Height = 600f }

    let image = drawImage (550, 650) 25f (drawElements elements true)
    let png = image.Encode()
    use file = File.OpenWrite "output.png"
    png.SaveTo file

main
