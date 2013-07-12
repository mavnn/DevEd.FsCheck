module DevEd.FsCheck
open System.Xml
open System.Xml.Linq

let filterInvalidChars (input : string) =
    input
    |> Seq.filter (fun c -> XmlConvert.IsXmlChar c)
    |> Seq.map string
    |> String.concat ""    

let (|ValidXml|InvalidXml|) str =
    let filtered = filterInvalidChars str
    if String.length filtered = String.length str then
        ValidXml str
    else
        InvalidXml filtered

let AddEnhancement (xDoc : XDocument) (input : string) =
    match input with
    | ValidXml text ->
        if xDoc.Root.Elements(XName.Get "Enhancement") |> Seq.exists(fun enhance -> enhance.Value = text) then
            xDoc.Root.Add(XElement(XName.Get "Enhancement", text))
    | InvalidXml text ->
        if xDoc.Root.Elements(XName.Get "Error") |> Seq.exists(fun error -> error.Value = text) then
            xDoc.Root.Add(XElement(XName.Get "Error", text))
    xDoc
