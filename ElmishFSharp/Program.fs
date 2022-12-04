open System
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Hosts
open Avalonia.Layout
open Avalonia.Themes.Fluent
open Elmish

module Counter =

    type Count = Count of int

    type Msg =
        | Increment
        | Decrement
        | SetCount of int
        | Reset

    let init = Count(0)

    let update msg (Count c) =
        match msg with
        | Increment -> Count(c + 1)
        | Decrement -> Count(c - 1)
        | SetCount v -> Count(v)
        | Reset -> Count(0)

    let view (Count c) dispatch =
        DockPanel.create [ DockPanel.children [ Button.create [ Button.dock Dock.Bottom
                                                                Button.onClick (fun _ -> dispatch Reset)
                                                                Button.content "reset"
                                                                Button.horizontalAlignment HorizontalAlignment.Stretch ]
                                                Button.create [ Button.dock Dock.Bottom
                                                                Button.onClick (fun _ -> dispatch Decrement)
                                                                Button.content "-"
                                                                Button.horizontalAlignment HorizontalAlignment.Stretch ]
                                                Button.create [ Button.dock Dock.Bottom
                                                                Button.onClick (fun _ -> dispatch Increment)
                                                                Button.content "+"
                                                                Button.horizontalAlignment HorizontalAlignment.Stretch ]
                                                TextBox.create [ TextBox.dock Dock.Bottom
                                                                 TextBox.text (string c)
                                                                 TextBox.onTextChanged (fun text ->
                                                                     let isNumber, number = Int32.TryParse text

                                                                     if isNumber then
                                                                         number |> SetCount |> dispatch)
                                                                 TextBox.horizontalAlignment HorizontalAlignment.Stretch ]
                                                TextBlock.create [ TextBlock.fontSize 48.0
                                                                   TextBlock.horizontalAlignment
                                                                       HorizontalAlignment.Center
                                                                   TextBlock.verticalAlignment VerticalAlignment.Center
                                                                   TextBlock.text (string c) ] ] ]

type MainWindow() as this =
    inherit HostWindow()

    do
        base.Title <- "Counter Example"
        base.Height <- 400.0
        base.Width <- 400.0

        Program.mkSimple (fun () -> Counter.init) Counter.update Counter.view
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
