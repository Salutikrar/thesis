namespace Confun.Check.Syntax.Json

open System.Diagnostics.CodeAnalysis
open System.Collections.Generic
open YamlDotNet.RepresentationModel
open System.IO
open System
open System.Text

open Confun.Core.Processing.Api
open Confun.Core.Types

module YamlValidation =
    let (|Mapping|Scalar|Sequence|) (yamlNode: YamlNode) =
        match yamlNode.NodeType with
        | YamlNodeType.Mapping  ->
            let node = yamlNode :?> YamlMappingNode
            let mapping =
                node.Children
                |> Seq.map (fun kvp ->
                    let keyNode = kvp.Key :?> YamlScalarNode
                    keyNode.Value, kvp.Value)
                |> Map.ofSeq
            Mapping (node, mapping)
        | YamlNodeType.Scalar   ->
            let node = yamlNode :?> YamlScalarNode
            Scalar (node, node.Value)
        | YamlNodeType.Sequence ->
            let node = yamlNode :?> YamlSequenceNode
            Sequence (node, List.ofSeq node.Children)
        | YamlNodeType.Alias
        | _ -> failwith "error"

    let rec getValue (x : YamlNode) =
        match x with
        | Mapping (n, mapping) ->
            let mutable list = []
            for e in (n.Children) do
                list <- List.append list [((sprintf "%A" e.Key), getValue e.Value)]
            Group(list)
        | Sequence (n, sequence) ->
            let mutable list = []
            for elem in n do
                list <- List.append list [(getValue elem)]
            Array(list |> Array.ofList)
        | Scalar (n, scalar) -> Str(sprintf "%A" n)

    let parsingYaml file =
        let f = new FileStream(file, FileMode.Open, FileAccess.Read)
        let stream = new StreamReader(f)
        let yaml = YamlStream()
        yaml.Load(stream)
        let root = yaml.Documents[0].RootNode :?> YamlMappingNode
        stream.Close();
        let res = getValue root
        // printfn "%A\n" res
        match res with
        | Group(gr) -> Ok(gr)
        | _ -> Error(ParsingError("unexpected structure"))
