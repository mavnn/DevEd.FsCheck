module FsCheck.Examples.Tests

open FsCheck
open DevEd.FsCheck
open System.Xml.Linq

let baseDocText = """<?xml version="1.0" encoding="UTF-8"?>
<root />
"""

type XmlTree = 
    | NodeName of string
    | Container of string * List<XmlTree>

let nodeNames = 
    ["myNode";
     "myOtherNode";
     "someDifferentNode"]

let tree = 
    let rec tree' s = 
        match s with
        | 0 -> Gen.map NodeName (Gen.elements nodeNames)
        | n when n > 0 -> 
            let subtrees = 
                Gen.sized <| fun s -> 
                    Gen.resize (s
                                |> float
                                |> sqrt
                                |> int) (Gen.listOf(tree'(n / 2)))
            Gen.oneof 
                [Gen.map NodeName (Gen.elements nodeNames);
                 
                 Gen.map2 (fun name contents -> Container(name, contents)) 
                     (Gen.elements nodeNames) subtrees]
        | _ -> invalidArg "s" "Size most be positive."
    Gen.sized tree'

let treeToXDoc xmlTree = 
    let rec inner currentNode children = 
        let childMatch child = 
            match child with
            | NodeName name -> XElement(XName.Get name)
            | Container(name, contents) -> 
                let element = XElement(XName.Get name)
                inner element contents
        currentNode.Add(List.map childMatch children |> List.toArray)
        currentNode
    match xmlTree with
    | NodeName name -> XDocument(XElement(XName.Get name))
    | Container(name, contents) -> 
        let doc = XDocument(XElement(XName.Get name))
        inner doc.Root contents |> ignore
        doc

type XmlGenerator() = 
    static member XmlTree() = 
        { new Arbitrary<XmlTree>() with
              member x.Generator = tree
              member x.Shrinker t = 
                  match t with
                  | NodeName _ -> Seq.empty
                  | Container(name, contents) -> 
                      match contents with
                      | [] -> seq { yield NodeName name }
                      | c -> 
                          seq { 
                              for n in c -> n } }

type XmlUpdaterProperties() = 
    static member ``AddEnhancement is idempotent``(data : string) = 
        ((AddEnhancement <| AddEnhancement (XDocument.Parse baseDocText) data) data)
            .ToString() = (AddEnhancement (XDocument.Parse baseDocText) data)
            .ToString()
    static member ``AddEnhancement is idempotent on different xml structures``(xmlDoc : XmlTree, 
                                                                           data : string) = 
        (AddEnhancement (treeToXDoc xmlDoc) data).ToString() = (AddEnhancement (AddEnhancement (treeToXDoc xmlDoc) data) data)
            .ToString()
    static member ``AddEnhancement never reduces the number of nodes`` (xmlDoc : XmlTree, data : string) =
        Seq.length ((treeToXDoc xmlDoc).DescendantNodes()) = Seq.length ((AddEnhancement (treeToXDoc xmlDoc) data).DescendantNodes())

Arb.register<XmlGenerator>() |> ignore
Check.QuickAll<XmlUpdaterProperties>()
System.Console.ReadLine() |> ignore