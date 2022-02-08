open Raylib_CsLo
open System.Numerics
open System

let rec mainLoop update render state =
    if Raylib.WindowShouldClose() then
        ()
    else
        let newState = update state
        render newState
        mainLoop update render newState

type State =
    { Angle: float32
      Speed: float32
      Size: float32 }

let update state =
    let dT = Raylib.GetFrameTime()

    let angle = state.Angle + state.Speed * dT

    let size =
        128f
        + 64f
          * (float32 (Math.Sin((float angle) * Math.PI / 180.)))

    { state with
        Angle = angle
        Size = size }

let render state =
    Raylib.BeginDrawing()
    Raylib.ClearBackground(Raylib.WHITE)

    let size = state.Size
    let rect = new Rectangle(128f, 128f, size, size)
    let rotationOrigin = new Vector2(size / 2f, size / 2f)
    Raylib.DrawRectanglePro(rect, rotationOrigin, state.Angle, Raylib.BLUE)

    Raylib.EndDrawing()

[<EntryPoint>]
let main _ =
    Raylib.InitWindow(256, 256, "Hello Raylib")
    Raylib.SetTargetFPS(60)
    mainLoop update render { Angle = 0f; Speed = 60f; Size = 128f }
    Raylib.CloseWindow()
    0
