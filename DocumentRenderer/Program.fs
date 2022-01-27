open SkiaSharp

type Rect = 
    {
        Left: float32
        Top: float32
        Width: float32
        Height: float32
    }

type TextContent =
    {
        Text: string
        Font: SKFont
    }

type ScreenElement =
    | TextElement of TextContent * Rect
    | ImageElement of string * Rect


let main =
    use font = SKTypeface.FromFamilyName "Arial"
    use textFont = new SKFont (font, 12f)
    use headFont = new SKFont (font, 15f)

    let elements = 
        [
            TextElement 
                ({  Text = "Functional programming in .NET"; Font = headFont },
                 {  Left = 10f; Top = 0f; Width = 400f; Height = 30f });
            ImageElement
                ("cover.jpg", { Left =120f; Top = 30f; Width = 150f; Height = 200f });
            TextElement 
                ({  Text = @"In this book, we'll introduce you to the essential
                            concepts of functional programming, but thanks to the .NET
                            framework, we won't be limited to theoretical examples and we
                            will use many of the rich .NET libraries to show how functional
                            programming can be used in a real-world."; 
                    Font = textFont },
                 { Left = 10f; Top = 230f; Width = 400f; Height = 400f });
        ]
    ignore

main ()