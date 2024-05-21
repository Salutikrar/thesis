
open System
open System.IO
open System.Diagnostics.CodeAnalysis
open System.Xml.Linq
open System.Linq;

type ConfunMap = Dict

and Dict = ConfigParam list

and ConfigParam = string * ConfigValue

and [<ExcludeFromCodeCoverage>] ConfigValue =
    | Null
    | Bool of bool
    | Int of int32
    | Float of float
    | Port of uint16
    | Str of string
    | NullableString of string
    | Regex of RegexPattern * Text
    | Group of Dict
    | Array of ConfigValue array
    | Node of NodeName * Dict

and RegexPattern = string

and Text = string

and NodeName = string

[<ExcludeFromCodeCoverage>]
type ValidatedConfunMap = ValidatedConfunMap of ConfunMap

[<ExcludeFromCodeCoverage>]
type ValidatedConfigFile =
    { Name: string
      DirectoryPath: string
      ValidatedParamsMap: ValidatedConfunMap }

[<ExcludeFromCodeCoverage>]
type ConfigFile =
    { Name: string
      DirectoryPath: string
      ParamsMap: ConfunMap }


module ValidatedConfunMap =
    [<ExcludeFromCodeCoverage>]
    let unwrap (ValidatedConfunMap configMap) = configMap



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



// open Confun.Core.Processing.Api
type ValidationResult<'TError> =
    | Valid
    | Invalid of 'TError

let pathYaml = "C:\\start\\vs\\src\\test\\ConfigsForTests\\Test1\\config1.xml"

let regexValidation name path mainKey pattern text =
    let fullPattern = sprintf "^%s$" pattern
    let regexResult = System.Text.RegularExpressions.Regex.Match(text, fullPattern)
    if not regexResult.Success then
        Invalid [ ValidationError (sprintf "In the file with name '%s' text '%s' (of value with key '%s') is not matched by regex '%s'" name text mainKey fullPattern) ]
    else
        Valid

printf "%A" (regexValidation "name" "path" "mainKey" @"[a-zA-Z .]+" "Dan D. Veloper")
