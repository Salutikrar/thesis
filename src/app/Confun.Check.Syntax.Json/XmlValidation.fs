namespace Confun.Check.Syntax.Json

open System.Diagnostics.CodeAnalysis
open System
open System.IO
open System.Xml.Linq
open System.Linq;

open Confun.Core.Processing.Api
open Confun.Core.Types

module XmlValidation =
    let getBuff (pathXML : string) =
        try
            let doc = XElement.Load pathXML
            (Valid, [doc])
        with
            | :? System.Xml.XmlException as ex -> (Invalid([ParsingError(sprintf "XML Parsing Error at line %d: %s" (ex.LineNumber-1) ex.Message)]), [])
            | :? InvalidOperationException as ex -> (Invalid([ParsingError(sprintf "Invalid XML format: %s" ex.Message)]), [])

    let getNames (node : XElement) =
        let mutable list = []
        for elem in node.Elements() do
            list <- List.append list [sprintf "%A" elem.Name]
        list
    let rec listUnique list =
        match list with
        | [] -> []
        | elem :: tail ->
            if (List.exists (fun str -> str = elem) tail) then
                listUnique tail
            else
                elem :: (listUnique tail)

    let rec getValue x (node : XElement) =
        let mutable list = []
        for elem in node.Elements(x) do
            list <- List.append list [makeConfun elem]
        if (list.Length = 1) then
            list.Head
        else
            Array(list |> Array.ofList)

    and makeConfun elem =
        let listNames = getNames elem
        let listNamesUnique = listUnique listNames
        if (listNamesUnique.Length = 0) then
            Str(elem.Value)
        else
            Group(List.map (fun x -> (x, (getValue (XName.Get(x)) elem))) listNamesUnique)

    let toOneStr strList name =
        name(List.fold (fun acc elem -> (ConfunError.toString elem)+"\n"+acc) "" strList)


    let parsingXml file=
        let (error, buffList) = getBuff file
        match error with
        | Valid ->
            let buff = buffList.Head
            let res = makeConfun buff
            match res with
            | Group(gr) -> Ok(gr)
            | _ -> Error(ParsingError("unexpected structure"))

        | Invalid(err) ->
            Error(toOneStr err ParsingError)
