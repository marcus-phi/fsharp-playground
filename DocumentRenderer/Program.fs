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

let drawElements elements (canvas: SKCanvas) =
    use brush = new SKPaint(Color = SKColors.Black)

    elements
    |> List.map (fun e ->
        match e with
        | TextElement (tc, rc) -> drawString tc.Text rc.Left rc.Top tc.Font brush canvas
        | ImageElement (filename, rc) ->
            let dw, dh = rc.Width / 10f, rc.Height / 10f
            let rc = toSKRect (deflate rc dw dh)
            use bmp = SKBitmap.Decode(filename)
            canvas.DrawBitmap(bmp, rc))

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

    let elements =
        [ TextElement(
              { Text = "Functional programming in .NET"
                Font = headFont },
              { Left = 10f
                Top = 0f
                Width = 400f
                Height = 30f }
          )
          ImageElement(
              "cover.jpg",
              { Left = 120f
                Top = 30f
                Width = 150f
                Height = 200f }
          )
          TextElement(
              { Text =
                  [ "In this book, we'll introduce you to the essential"
                    "concepts of functional programming, but thanks to the .NET"
                    "framework, we won't be limited to theoretical examples and we"
                    "will use many of the rich .NET libraries to show how functional"
                    "programming can be used in a real-world." ]
                  |> String.concat "\n"
                Font = textFont },
              { Left = 10f
                Top = 230f
                Width = 400f
                Height = 400f }
          ) ]

    let image = drawImage (400, 450) 20f (drawElements elements)
    let png = image.Encode()
    use file = File.OpenWrite "output.png"
    png.SaveTo file

main
