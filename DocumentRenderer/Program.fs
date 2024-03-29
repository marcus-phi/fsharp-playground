﻿open SkiaSharp
open System.IO
open System

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

let drawString (str: string) (rc: SKRect) (font: SKFont) (brush: SKPaint) (canvas: SKCanvas) =
    str.Split " "
    |> Seq.fold
        (fun (phrases: string []) (word: string) ->
            let newPhrase =
                if phrases.Length = 0 then
                    word
                else
                    phrases[phrases.Length - 1] + " " + word

            let blob = SKTextBlob.Create(newPhrase, font)

            if blob.Bounds.Width < rc.Width then
                if phrases.Length = 0 then
                    [| newPhrase |]
                else
                    Array.append phrases[0 .. phrases.Length - 2] [| newPhrase |]
            else
                Array.append phrases [| word |])
        [||]
    |> Seq.fold
        (fun y line ->
            let blob = SKTextBlob.Create(line, font)
            canvas.DrawText(blob, rc.Left, y + blob.Bounds.Height, brush)
            y + blob.Bounds.Height)
        rc.Top
    |> ignore

let drawBitmap (bmp: SKBitmap) (rc: SKRect) (canvas: SKCanvas) =
    let ratio = (float32 bmp.Width) / (float32 bmp.Height)

    let w, h =
        if rc.Width > rc.Height then
            rc.Height * ratio, rc.Height
        else
            rc.Width, rc.Width / ratio

    let bmpRc =
        { Left = rc.Left + rc.Width / 2f - w / 2f
          Top = rc.Top + rc.Height / 2f - h / 2f
          Width = w
          Height = h }

    canvas.DrawBitmap(bmp, toSKRect bmpRc)

let drawElements elements borderDebug (canvas: SKCanvas) =
    use brush = new SKPaint(Color = SKColors.Black)
    use debugBrush = new SKPaint(Color = SKColors.Red, IsStroke = true)

    elements
    |> List.map (fun e ->
        match e with
        | TextElement (tc, rc) ->
            let rc = toSKRect rc
            drawString tc.Text rc tc.Font brush canvas

            if borderDebug then
                canvas.DrawRect(rc, debugBrush)
        | ImageElement (filename, rc) ->
            let dw, dh = rc.Width / 10f, rc.Height / 10f
            let rc = toSKRect (deflate rc dw dh)
            use bmp = SKBitmap.Decode(filename)
            drawBitmap bmp rc canvas

            if borderDebug then
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
                          |> String.concat " "
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

    let image = drawImage (550, 650) 25f (drawElements elements false)
    let png = image.Encode()
    use file = File.OpenWrite "output.png"
    png.SaveTo file

main
