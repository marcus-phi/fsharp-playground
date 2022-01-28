open SkiaSharp
open System.IO

let main =
    let info = SKImageInfo(256, 256)
    use surface = SKSurface.Create info
    let canvas = surface.Canvas

    canvas.Clear SKColors.White

    use redBrush = new SKPaint(Color = SKColor(0xffuy, 0uy, 0uy), IsStroke = true)

    use blueBrush = new SKPaint(Color = SKColor(0uy, 0uy, 0xffuy), IsStroke = true)

    [ 0..8..64 ]
    |> List.map (fun i ->
        let rect =
            SKRect(float32 i, float32 i, 256f - float32 i - 1f, 256f - float32 i - 1f)

        let brush =
            if i % 16 = 0 then
                redBrush
            else
                blueBrush

        canvas.DrawRect(rect, brush))
    |> ignore

    let image = surface.Snapshot()
    let png = image.Encode()
    use file = File.OpenWrite "output.png"
    png.SaveTo file

main
