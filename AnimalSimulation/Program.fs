open System
open System.Linq
open System.Threading.Tasks
open FSharp.Collections.ParallelSeq
open Raylib_CsLo

module SafeRandom =
    let private rnd = Random()
    let New () = lock rnd (fun () -> Random(rnd.Next()))

[<Struct>]
type Location(x: float, y: float) =
    member t.X = x
    member t.Y = y
    static member (+)(l1: Location, l2: Location) = Location(l1.X + l2.X, l1.Y + l2.Y)
    static member (*)(l: Location, f) = Location(l.X * f, l.Y * f)
    static member (-)(l1: Location, l2: Location) = l1 + (l2 * -1.)

type Simulation =
    { Animals: list<Location>
      Predators: list<Location> }

let distance (a: Location) (b: Location) =
    Math.Sqrt(
        (a.X - b.X) * (a.X - b.X)
        + (a.Y - b.Y) * (a.Y - b.Y)
    )

let getPathPoints (a: Location, b: Location) =
    [ 1..11 ]
    |> Seq.ofList
    |> Seq.map (fun idx -> (b - a) * (float idx / 12.) + a)

let randomLocations cnt =
    let rnd = SafeRandom.New()

    [ 1..cnt ]
    |> Seq.ofList
    |> Seq.map (fun idx -> Location(rnd.Next(512), rnd.Next(512)))

let moveAnimal (state: Simulation) (animal: Location) =
    let nearestPredatorFrom pos =
        state.Predators
        |> Seq.map (distance pos)
        |> Seq.min

    let nearestPredatorOnPath target =
        getPathPoints (animal, target)
        |> Seq.map nearestPredatorFrom
        |> Seq.min

    let target =
        randomLocations 10
        |> Seq.maxBy nearestPredatorOnPath

    animal
    + (target - animal)
      * (20. / (distance target animal))

let movePredator (state: Simulation) (predator: Location) =
    let locationsClose locations pos =
        locations
        |> Seq.filter (fun l -> (distance l pos) < 50)
        |> Seq.length

    let locationsOnPath locations from target =
        getPathPoints (from, target)
        |> Seq.sumBy (fun p -> locationsClose locations p)

    let target =
        randomLocations 20
        |> Seq.maxBy (fun x ->
            (locationsOnPath state.Animals predator x)
            - (locationsOnPath state.Predators predator x) * 3)

    predator
    + (target - predator)
      * (10. / (distance target predator))

let update (state: Simulation) =
    let animals =
        state.Animals
        |> PSeq.map (moveAnimal state)
        |> List.ofSeq

    let predators =
        state.Predators
        |> PSeq.map (movePredator state)
        |> List.ofSeq

    { Animals = animals
      Predators = predators }

let render (state: Simulation) =
    Raylib.BeginDrawing()
    Raylib.ClearBackground(Raylib.WHITE)

    state.Animals
    |> List.map (fun x -> Raylib.DrawCircle(int x.X, int x.Y, 3f, Raylib.GREEN))
    |> ignore

    state.Predators
    |> List.map (fun x -> Raylib.DrawCircle(int x.X, int x.Y, 5f, Raylib.RED))
    |> ignore

    Raylib.EndDrawing()

let rec mainLoop update render state =
    if Raylib.WindowShouldClose() then
        ()
    else
        render state
        state |> update |> mainLoop update render

[<EntryPoint>]
let main _ =
    Raylib.InitWindow(512, 512, "Animal-Predator Simulation")
    Raylib.SetTargetFPS(60)

    let initState =
        { Animals = randomLocations 150 |> List.ofSeq
          Predators = randomLocations 15 |> List.ofSeq }

    mainLoop update render initState
    Raylib.CloseWindow()
    0
