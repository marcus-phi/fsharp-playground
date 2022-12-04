namespace ImageProcessing

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Hosts
open Avalonia.Themes.Fluent
open Elmish

type MainWindow() as this =
    inherit HostWindow()

    do
        base.Title <- "Image Processor"
        base.Width <- 1024.0
        base.Height <- 600.0

        let init = ImageProcessor.init
        let update = ImageProcessor.update
        let view = ImageProcessor.view this

        Program.mkSimple init update view
        |> Program.withHost this
        |> Program.withConsoleTrace
        |> Program.run

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add(FluentTheme(baseUri = null, Mode = FluentThemeMode.Dark))

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifeTime ->
            let mainWindow = MainWindow()
            desktopLifeTime.MainWindow <- mainWindow
        | _ -> ()

module Program =

    [<EntryPoint>]
    let main args =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)
