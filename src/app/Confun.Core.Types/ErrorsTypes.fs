namespace Confun.Core.Types

open System.Diagnostics.CodeAnalysis

[<ExcludeFromCodeCoverage>]
type ConfunError =
    | ValidationError of string
    | GenerationError of string
    | ParsingError of string
    | StructureError of string

module ConfunError =
    let toString =
        function
        | ValidationError error -> error
        | GenerationError error -> error
        | ParsingError error -> error
        | StructureError error -> error


    let addToError prefix =
        function
            | ValidationError error ->
                error
                |> (sprintf "%s\n%s" prefix)
                |> ValidationError
            | GenerationError error ->
                error
                |> (sprintf "%s\n%s" prefix)
                |> GenerationError
            | ParsingError error ->
                error
                |> (sprintf "%s\n%s" prefix)
                |> ParsingError
            | StructureError error ->
                error
                |> (sprintf "%s\n%s" prefix)
                |> StructureError
    let addPrefixToError prefix =
        function
            | ValidationError error ->
                error
                |> (sprintf "%s. %s" prefix)
                |> ValidationError
            | GenerationError error ->
                error
                |> (sprintf "%s. %s" prefix)
                |> GenerationError
            | ParsingError error ->
                error
                |> (sprintf "%s. %s" prefix)
                |> ParsingError
            | StructureError error ->
                error
                |> (sprintf "%s. %s" prefix)
                |> StructureError

    let addPrefixToErrors prefix errorList =
        errorList |> List.map (addPrefixToError prefix)

    let unwrapOkResult =
        function
        | Ok result -> Some result
        | _ -> None

    let unwrapErrorResult =
        function
        | Error errors -> Some errors
        | _ -> None

    let aggregateResults results =
        let allResultsSuccess =
            results
            |> List.forall (function
                | Ok _ -> true
                | _ -> false)

        if allResultsSuccess then
            Ok (results |> List.choose unwrapOkResult)
        else
            Error
                (results
                 |> List.choose unwrapErrorResult
                 |> List.concat)
