open System.Net.Http

let downloadUrl (url: string) =
    async {
        use client = new HttpClient()
        let! res = client.GetAsync(url) |> Async.AwaitTask
        return! res.Content.ReadAsStringAsync() |> Async.AwaitTask
    }

//downloadUrl "https://marcusphi.com" |> Async.RunSynchronously
//
//let tasks = [ downloadUrl "https://www.marcusphi.net"
//              downloadUrl "https://www.google.com" ]
//
//tasks |> Async.Parallel |> Async.RunSynchronously

let worldBankUrl apiPath props =
    seq {
        yield $"http://api.worldbank.org/V2/{apiPath}?"
        yield! props |> Seq.map (fun (k, v) -> k + "=" + v)
    }
    |> String.concat "&"

//let url = worldBankUrl [ "region", "EAP"; "incomeLevel", "UMC" ]
//downloadUrl url |> Async.RunSynchronously

let worldBankDownload apiPath props =
    let url = worldBankUrl apiPath props

    let rec retry count =
        async {
            try
                return! downloadUrl url
            with
            | e when count > 0 ->
                printfn $"Failed, retrying attempt remaining: %d{count}, apiPath %s{apiPath}, %A{props}"
                do! Async.Sleep(500)
                return! retry (count - 1)
        }

    retry 5

//[ "region", "EAP"; "incomeLevel", "UMC" ] |> worldBankDownload |> Async.RunSynchronously

open System.Xml.Linq

let xattr s (el: XElement) = el.Attribute(XName.Get(s)).Value

let xelem s (el: XContainer) =
    el.Element(XName.Get(s, "http://www.worldbank.org"))

let xvalue (el: XElement) = el.Value

let xelems s (el: XContainer) =
    el.Elements(XName.Get(s, "http://www.worldbank.org"))

let xpath path (el: XContainer) =
    let res =
        path
        |> Seq.fold (fun xn s -> xn |> xelem s :> XContainer) el

    res :?> XElement

let worldBankRequest apiPath props =
    async {
        let! text = worldBankDownload apiPath props
        return XDocument.Parse(text)
    }

let rec getCountries page =
    async {
        let! doc =
            [ "per_page", "100" ]
            |> worldBankRequest "country"

        let pages =
            doc
            |> xpath [ "countries" ]
            |> xattr "pages"
            |> int

        let countries =
            doc
            |> xpath [ "countries" ]
            |> xelems "country"
            |> Seq.map (fun x -> x |> xelem "iso2Code" |> xvalue, x |> xelem "name" |> xvalue)

        if pages = page then
            return countries
        else
            let! rest = getCountries (page + 1)

            return
                seq {
                    yield! countries
                    yield! rest
                }
    }

let countries = getCountries 1 |> Async.RunSynchronously

let indicators = [ "AG.SRF.TOTL.K2"; "AG.LND.FRST.ZS" ]
let dates = [ "1990"; "2000"; "2005" ]

let rec getIndicatorData indicator date page =
    async {
        let! doc =
            [ "date", date
              "page", string page
              "per_page", "100" ]
            |> worldBankRequest $"country/all/indicator/%s{indicator}"

        let pages = doc |> xpath [ "data" ] |> xattr "pages" |> int

        if pages = page then
            return [ doc ]
        else
            let! rest = getIndicatorData indicator date (page + 1)
            return doc :: rest
    }

let data =
    (indicators, dates)
    ||> Seq.allPairs
    |> Seq.map (fun (ind, date) -> getIndicatorData ind date 1)
    |> Async.Parallel
    |> Async.RunSynchronously

let readValue format node =
    let value = node |> xelem "value" |> xvalue
    let country = node |> xelem "country" |> xattr "id"
    let year = node |> xelem "date" |> xvalue |> int

    if value = "" then
        []
    else
        [ (year, country), format value ]

let readValues format data =
    seq {
        for page in data do
            let root = page |> xpath [ "data" ]

            for node in root |> xelems "data" do
                yield! node |> readValue format
    }

[<Measure>]
type km

[<Measure>]
type percent

let floatInv s =
    let inv = System.Globalization.CultureInfo.InvariantCulture
    System.Double.Parse(s)

let areas =
    data.[0..2]
    |> Seq.concat
    |> readValues (fun a -> (floatInv a) * 1.0<km^2>)
    |> Map.ofSeq

let forests =
    data.[3..5]
    |> Seq.concat
    |> readValues (fun a -> (floatInv a) * 1.0<percent>)
    |> Map.ofSeq

let calculateForests (area: float<km^2>, forest: float<percent>) = area * forest / 100.0<percent>

let years = [ 1990; 2000; 2005 ]

let dataAvailable key =
    years
    |> Seq.forall (fun y ->
        (Map.containsKey (y, key) areas)
        && (Map.containsKey (y, key) forests))

let getForestData key =
    years
    |> Seq.map (fun y -> calculateForests (areas.[y, key], forests.[y, key]))

let stats =
    seq {
        for key, title in countries do
            if dataAvailable key then
                yield title, getForestData key
    }

open Plotly.NET

let chart =
    let countries = stats |> Seq.map (fun (country, _) -> country)

    [ "1990", 0; "2000", 1; "2005", 2 ]
    |> Seq.map (fun (year, index) ->
        let values =
            stats
            |> Seq.map (fun (_, values) -> values |> Seq.item index |> float)

        Chart.Column(values, countries, Name = year))
    |> Chart.combine
    |> Chart.withLayoutStyle ( Width = 1920, Height = 900 )
    |> Chart.withXAxis (LayoutObjects.LinearAxis.init (TickAngle = 45))

chart |> Chart.show
