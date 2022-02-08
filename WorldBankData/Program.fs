open System.Net.Http

let downloadUrl (url: string) = async {
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

let worldBankUrl props =
    seq {
        yield "http://api.worldbank.org/V2/country?"
        yield! props |> Seq.map (fun (k, v) -> k + "=" + v)
    } |> String.concat "&"

//let url = worldBankUrl [ "region", "EAP"; "incomeLevel", "UMC" ]
//downloadUrl url |> Async.RunSynchronously

let worldBankDownload props =
    let url = worldBankUrl props
    let rec retry count = async {
        try
            return! downloadUrl url
        with e when count > 0 ->
            printfn $"Failed, retrying attempt left: (%d{count}): %A{props}"
            do! Async.Sleep(500)
            return! retry (count - 1)
    }
    retry 5

//[ "region", "EAP"; "incomeLevel", "UMC" ] |> worldBankDownload |> Async.RunSynchronously

open System.Xml.Linq

let xattr s (el: XElement) =
    el.Attribute(XName.Get(s)).Value
let xelem s (el: XContainer) =
    el.Element(XName.Get(s, "http://www.worldbank.org"))
let xvalue (el: XElement) =
    el.Value
let xelems s (el: XContainer) =
    el.Elements(XName.Get(s, "http://www.worldbank.org"))
    
let xpath path (el: XContainer) =
    let res = path |> Seq.fold (fun xn s -> xn |> xelem s :> XContainer) el
    res :?> XElement
    
let worldBankRequest props = async {
    let! text = worldBankDownload props
    return XDocument.Parse(text)
}
    
//let doc = [ "region", "NAC" ] |> worldBankRequest |> Async.RunSynchronously
//doc |> xpath [ "countries"; ] |> xelems "country" |> Seq.map (fun x -> x |> xattr "id", x |> xelem "name" |> xvalue )