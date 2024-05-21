
open System
open System.IO
open System.Diagnostics.CodeAnalysis
open System.Xml.Linq

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



let pathXML = "C:\\start\\vs\\src\\test\\ConfigsForTests\\Test1\\config3.xml"


// // let doc = new XmlDocument()
// // doc.LoadXml(x)
// let doc = XDocument.Load pathXML

let rec startParsing list  =
    match list with
    | '<' :: tail -> findKey tail ""
    | '\013' :: tail -> startParsing tail
    | '\010' :: tail -> startParsing tail
    | ' ' :: tail -> startParsing tail
    | '\t' :: tail -> startParsing tail
    | [] -> (Valid, [], [])
    | a :: tail -> (Invalid([ParsingError (sprintf "unexpected sign {a}, not found '{'")]), [], [])

let rec checkEnd list r =
    match list with
    | '\013' :: tail -> checkEnd tail r
    | '\010' :: tail -> checkEnd tail r
    | ' ' :: tail -> checkEnd tail r
    | '\t' :: tail -> checkEnd tail r
    | [] -> Ok(r)
    | a :: tail -> Error(ParsingError(sprintf "unexpected sign %c after the end of rules" a))

let toOneStr strList name =
    name(List.fold (fun acc elem -> (ConfunError.toString elem)+"\n"+acc) "" strList)


let parsingJson file=
    let readLines filePath =
        System.IO.File.ReadAllText(filePath) |> Seq.toList
    let info = readLines file
    let res = (startParsing info)
    // printfn "%A" ( info)
    // printfn "%A" ( res)
    match res with
    | (Valid, r, l) -> checkEnd l r
    | (Invalid(err), a, b) -> Error(toOneStr err ParsingError)

printf "%A" doc

































let isDigit a =
    if ('0' <= a) && (a <= '9') then true else false
let appendToList str value list =
    match list with
    | (Valid , res, list) -> (Valid, (str, value)::res, list)
    | (Invalid err, _, _) -> (Invalid(err), [], [])

let appendToListArr value list =
    match list with
    | (Valid, res, list) -> (Valid, (value)::res, list)
    | (Invalid err, _, _) -> (Invalid(err), [], [])


let rec readCommaOrEndArr list value   =
    match list with
    | ' ' :: tail -> readCommaOrEndArr tail value
    | '\013' :: tail -> readCommaOrEndArr tail value
    | '\010' :: tail -> readCommaOrEndArr tail value
    | '\t' :: tail -> readCommaOrEndArr tail value
    | ']' :: tail -> (Valid, [value], tail)
    | ',' :: tail -> appendToListArr value (readValueArr tail)
    | [] -> (Invalid([ParsingError(sprintf "uncontinued expression, ',' or '}' not found" )]), [], [])
    | a :: tail -> (Invalid([ParsingError(sprintf "unexpected sign %c, expected ',' or '}'" a)]), [], [])

and readCommaOrEnd list str value   =
    match list with
    | ' ' :: tail -> readCommaOrEnd tail str value
    | '\013' :: tail -> readCommaOrEnd tail str value
    | '\010' :: tail -> readCommaOrEnd tail str value
    | '\t' :: tail -> readCommaOrEnd tail str  value
    | '}' :: tail -> (Valid, [(str, value)], tail)
    | ',' :: tail -> appendToList str value (endOrBeginOfKey tail)
    | [] -> (Invalid([ParsingError(sprintf "uncontinued expression, ',' or '}' not found" )]), [], [])
    | a :: tail -> (Invalid([ParsingError(sprintf "unexpected sign %c, expected ',' or '}'" a)]), [], [])

and readStringArr list res  =
    match list with
    | '"' :: tail -> readCommaOrEndArr tail (Str(res))
    | '\\' :: ('"' :: tail2) -> readStringArr tail2 (res+"\"")
    | '\\' :: ('\\' :: tail2) -> readStringArr tail2 (res+"\\")
    | '\\' :: ('/' :: tail2) -> readStringArr tail2 (res+"/")
    | '\\' :: ('b' :: tail2) -> readStringArr tail2 (res+"\008")
    | '\\' :: ('f' :: tail2) -> readStringArr tail2 (res+"\012")
    | '\\' :: ('n' :: tail2) -> readStringArr tail2 (res+"\010")
    | '\\' :: ('r' :: tail2) -> readStringArr tail2 (res+"\013")
    | '\\' :: ('t' :: tail2) -> readStringArr tail2 (res+"\009")
    | '\\' :: [] -> (Invalid([ParsingError("uncompleted string")]), [], [])
    | [] -> (Invalid([ParsingError("uncompleted string")]), [], [])
    | a :: tail -> readStringArr tail (res + string(a))
and readString list str res  =
    match list with
    | '"' :: tail -> readCommaOrEnd tail str (Str(res))
    | '\\' :: ('"' :: tail2) -> readString tail2 str (res+"\"")
    | '\\' :: ('\\' :: tail2) -> readString tail2 str (res+"\\")
    | '\\' :: ('/' :: tail2) -> readString tail2 str (res+"/")
    | '\\' :: ('b' :: tail2) -> readString tail2 str (res+"\008")
    | '\\' :: ('f' :: tail2) -> readString tail2 str (res+"\012")
    | '\\' :: ('n' :: tail2) -> readString tail2 str (res+"\010")
    | '\\' :: ('r' :: tail2) -> readString tail2 str (res+"\013")
    | '\\' :: ('t' :: tail2) -> readString tail2 str (res+"\009")
    | '\\' :: [] -> (Invalid([ParsingError("uncompleted string")]), [], [])
    | [] -> (Invalid([ParsingError("uncompleted string")]), [], [])
    | a :: tail -> readString tail str (res + string(a))

and readConstArr list str state =
    match list with
    | '-' :: tail when state = 0 -> readConstArr tail "-" 1
    | a :: tail when state = 0 && (isDigit a) -> readConstArr tail (string a) 2
    | a :: tail when state = 0 && (isDigit a) -> readConstArr tail (str + string a)  2
    | a :: tail when state = 2 && (isDigit a) -> readConstArr tail (str + string a)  2
    | a :: tail when state = 2 && (a = ' ' || a = '\t' || a = '\013' || a = '\010' || a = ',' || a = '}') -> readCommaOrEndArr list (Int(int str))
    | '.' :: tail when state = 2 -> readConstArr tail (str + string '.')  3
    | a :: tail when state = 3 && (isDigit a) -> readConstArr tail (str + string a)  4
    | a :: tail when state = 4 && (isDigit a) -> readConstArr tail (str + string a)  4
    | a :: tail when state = 4 && (a = ' ' || a = '\t' || a = '\013' || a = '\010' || a = ',' || a = '}') -> readCommaOrEndArr list (Int(int str))
    | 'e' :: tail when state = 2 -> readConstArr tail (str + string 'e')  5
    | 'E' :: tail when state = 2 -> readConstArr tail (str + string 'E')  5
    | '+' :: tail when state = 5 -> readConstArr tail (str + string '+')  6
    | '-' :: tail when state = 5 -> readConstArr tail (str + string '-')  6
    | a :: tail when state = 5 -> readConstArr list (str)  6
    | a :: tail when state = 6 && (isDigit a) -> readConstArr tail (str + string a)  7
    | a :: tail when state = 7 && (isDigit a) -> readConstArr tail (str + string a)  7
    | a :: tail when state = 7 && (a = ' ' || a = '\t' || a = '\013' || a = '\010' || a = ',' || a = '}') -> readCommaOrEndArr list (Int(int str))
    | a :: tail -> (Invalid([ParsingError(sprintf "unexpected sign %c for value for '%s' key" a str)]), [], [])
    | [] -> (Invalid([ParsingError(sprintf "uncontinued expression, can not found a value for '%s' key" str)]), [], [])

and readConst list key str state =
    match list with
    | '-' :: tail when state = 0 -> readConst tail key "-" 1
    | a :: tail when state = 0 && (isDigit a) -> readConst tail key (string a) 2
    | a :: tail when state = 0 && (isDigit a) -> readConst tail key (str + string a)  2
    | a :: tail when state = 2 && (isDigit a) -> readConst tail key (str + string a)  2
    | a :: tail when state = 2 && (a = ' ' || a = '\t' || a = '\013' || a = '\010' || a = ',' || a = '}') -> readCommaOrEnd list key (Int(int str))
    | '.' :: tail when state = 2 -> readConst tail key (str + string '.')  3
    | a :: tail when state = 3 && (isDigit a) -> readConst tail key (str + string a)  4
    | a :: tail when state = 4 && (isDigit a) -> readConst tail key (str + string a)  4
    | a :: tail when state = 4 && (a = ' ' || a = '\t' || a = '\013' || a = '\010' || a = ',' || a = '}') -> readCommaOrEnd list key (Int(int str))
    | 'e' :: tail when state = 2 -> readConst tail key (str + string 'e')  5
    | 'E' :: tail when state = 2 -> readConst tail key (str + string 'E')  5
    | '+' :: tail when state = 5 -> readConst tail key (str + string '+')  6
    | '-' :: tail when state = 5 -> readConst tail key (str + string '-')  6
    | a :: tail when state = 5 -> readConst list key (str)  6
    | a :: tail when state = 6 && (isDigit a) -> readConst tail key (str + string a)  7
    | a :: tail when state = 7 && (isDigit a) -> readConst tail key (str + string a)  7
    | a :: tail when state = 7 && (a = ' ' || a = '\t' || a = '\013' || a = '\010' || a = ',' || a = '}') -> readCommaOrEnd list key (Int(int str))
    | a :: tail -> (Invalid([ParsingError(sprintf "unexpected sign %c for value for '%s' key" a str)]), [], [])
    | [] -> (Invalid([ParsingError(sprintf "uncontinued expression, can not found a value for '%s' key" str)]), [], [])

and readValueArr list  =
    match list with
    | ' ' :: tail -> readValueArr tail
    | '\013' :: tail -> readValueArr tail
    | '\010' :: tail -> readValueArr tail
    | '\t' :: tail -> readValueArr tail
    | '"' :: tail -> readStringArr tail ""
    | '{' :: tail ->
        let res = endOrBeginOfKey tail
        match res with
        | (Valid, obj, l) -> readCommaOrEndArr l (Group(obj))
        | (Invalid(err), _, _) -> (Invalid(err), [], [])

    | '[' :: tail ->
        let res = readValueArr tail
        match res with
        | (Valid, obj, l) -> readCommaOrEndArr l (Array(Array.ofList obj))
        | (Invalid(err), _, _) -> (Invalid(err), [], [])

    | 'n' :: 'u' :: 'l' :: 'l' :: tail -> readCommaOrEndArr tail Null
    | 't' :: 'r' :: 'u' :: 'e' :: tail -> readCommaOrEndArr tail (Bool(true))
    | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: tail -> readCommaOrEndArr tail (Bool(true))
    | a :: tail when ((isDigit a) || (a = '-')) -> readConstArr list "" 0
    | [] -> (Invalid([ParsingError(sprintf "uncontinued expression, can not found a value")]), [], [])
    | a :: tail -> (Invalid([ParsingError(sprintf "unexpected sign %c for value" a)]), [], [])

and readValue list str  =
    match list with
    | ' ' :: tail -> readValue tail str
    | '\013' :: tail -> readValue tail str
    | '\010' :: tail -> readValue tail str
    | '\t' :: tail -> readValue tail str
    | '"' :: tail -> readString tail str ""
    | '{' :: tail ->
        let res = endOrBeginOfKey tail
        match res with
        | (Valid, obj, l) -> readCommaOrEnd l str (Group(obj))
        | (Invalid(err), _, _) -> (Invalid(err), [], [])

    | '[' :: tail ->
        let res = readValueArr tail
        match res with
        | (Valid, obj, l) -> readCommaOrEnd l str (Array(Array.ofList obj))
        | (Invalid(err), _, _) -> (Invalid(err), [], [])

    | 'n' :: 'u' :: 'l' :: 'l' :: tail -> readCommaOrEnd tail str Null
    | 't' :: 'r' :: 'u' :: 'e' :: tail -> readCommaOrEnd tail str (Bool(true))
    | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: tail -> readCommaOrEnd tail str (Bool(true))
    | a :: tail when ((isDigit a) || (a = '-')) -> readConst list str "" 0
    | [] -> (Invalid([ParsingError(sprintf "uncontinued expression, can not found a value for '%s' key" str)]), [], [])
    | a :: tail -> (Invalid([ParsingError(sprintf "unexpected sign %c for value for '%s' key" a str)]), [], [])

and findColon list str  =
    match list with
    | ' ' :: tail -> findColon tail str
    | '\013' :: tail -> findColon tail str
    | '\010' :: tail -> findColon tail str
    | '\t' :: tail -> findColon tail str
    | ':' :: tail -> readValue tail str
    | [] -> (Invalid([ParsingError(sprintf "uncontinued expression, colon not found" )]), [], [])
    | a :: tail -> (Invalid([ParsingError(sprintf "unexpected sign %c" a)]), [], [])

and readKey list res  =
    match list with
    | '"' :: tail -> findColon tail res
    | '\\' :: ('"' :: tail2) -> readKey tail2 (res+"\"")
    | '\\' :: ('\\' :: tail2) -> readKey tail2 (res+"\\")
    | '\\' :: ('/' :: tail2) -> readKey tail2 (res+"/")
    | '\\' :: ('b' :: tail2) -> readKey tail2 (res+"\008")
    | '\\' :: ('f' :: tail2) -> readKey tail2 (res+"\012")
    | '\\' :: ('n' :: tail2) -> readKey tail2 (res+"\010")
    | '\\' :: ('r' :: tail2) -> readKey tail2 (res+"\013")
    | '\\' :: ('t' :: tail2) -> readKey tail2 (res+"\009")
    | '\\' :: [] -> (Invalid([ParsingError("uncompleted string")]), [], [])
    | [] -> (Invalid([ParsingError("uncompleted string")]), [], [])
    | a :: tail -> readKey tail (res + string(a))

and endOrBeginOfKey list   =
    match list with
    | '"' :: tail -> readKey tail ""
    | '}' :: tail -> (Valid, [], tail)
    | '\013' :: tail -> endOrBeginOfKey tail
    | '\010' :: tail -> endOrBeginOfKey tail
    | ' ' :: tail -> endOrBeginOfKey tail
    | '\t' :: tail -> endOrBeginOfKey tail
    | [] -> (Valid, [], [])
    | a :: tail -> (Invalid([ParsingError(sprintf "unexpected sign %c" a)]), [], [])
let rec startParsing list  =
    match list with
    | '{' :: tail -> endOrBeginOfKey tail
    | '\013' :: tail -> startParsing tail
    | '\010' :: tail -> startParsing tail
    | ' ' :: tail -> startParsing tail
    | '\t' :: tail -> startParsing tail
    | [] -> (Valid, [], [])
    | a :: tail -> (Invalid([ParsingError (sprintf "unexpected sign {a}, not found '{'")]), [], [])

let rec checkEnd list r =
        match list with
        | '\013' :: tail -> checkEnd tail r
        | '\010' :: tail -> checkEnd tail r
        | ' ' :: tail -> checkEnd tail r
        | '\t' :: tail -> checkEnd tail r
        | [] -> Ok(r)
        | a :: tail -> Error(ParsingError(sprintf "unexpected sign %c after the end of rules" a))

let toOneStr strList name =
    name(List.fold (fun acc elem -> (ConfunError.toString elem)+"\n"+acc) "" strList)

let parsingJson file=
        let readLines filePath =
            System.IO.File.ReadAllText(filePath) |> Seq.toList
        let info = readLines file
        let res = (startParsing info)
        // printfn "%A" ( info)
        // printfn "%A" ( res)
        match res with
        | (Valid, r, l) -> checkEnd l r
        | (Invalid(err), a, b) -> Error(toOneStr err ParsingError)

// printfn "%A" (parsingJson "C:\\start\\vs\\src\\examples\\ReadmeExample\\App\\Configs\\app.config.json")


// let correctNameOrGroup rules =
//     match rules with
//     | ("name", Str(name))::tail -> Some([name])
//     | ("group", Array(arr))::tail -> Some((arr |> Array.toList))
//     | _::tail -> correctNameOrGroup tail
//     | [] -> None
let rec getNames rules res was =
    match rules with
    | ("name", name)::tail when was = false -> getNames tail (Some([name])) true
    | ("group", Array(arr))::tail when was = false -> getNames tail (Some((arr |> Array.toList))) true
    | ("name", _)::tail -> None
    | ("group", _)::tail -> None
    | _::tail -> getNames tail res was
    | [] -> res

let rec correctListNames list res =
    match list with
    | (Str(str))::tail -> correctListNames tail (str::res)
    | [] -> Some(res |> List.fold (fun acc h -> h::acc) [])
    | _ -> None


let rec correctListGroups list res =
    match list with
    | (Group(gr))::tail -> correctListGroups tail (gr::res)
    | [] -> Some(res |> List.fold (fun acc h -> h::acc) [])
    | _ -> None

let rec pushDictWithName names group res =
    match names with
    | name :: tail ->
        if (List.exists (fun (str, _) -> str = name) res) then
            pushDictWithName tail group (List.map (fun (str, data) -> if str = name then (str, List.append data group) else (str, data)) res)
        else
            pushDictWithName tail group ((name, group)::res)
    | [] -> (Valid, res |> List.fold (fun acc h -> h::acc) [])

let pushDictInResult group acc =
    let (a, b) = acc
    let names = getNames group None false
    match names with
    | Some(list) ->
        let newNames = correctListNames list []
        match newNames with
        | Some(list1) ->
            match a with
            | (Valid, list2) -> (pushDictWithName list1 group list2, b+1)
            | (Invalid(err), _) -> ((Invalid(err), []), b+1)
        | None ->
            match a with
            | (Valid, _) -> ((Invalid([StructureError(sprintf "All value in 'name' or 'group' should be of type 'string' in %d object" b)]), []), b+1)
            | (Invalid(err), _) -> ((Invalid((StructureError(sprintf "All value in 'name' or 'group' should be of type 'string' in %d object" b)) :: err), []), b+1)
    | None ->
        match a with
        | (Valid, _) -> ((Invalid([StructureError(sprintf "Can not define 'name' or 'group' in %d object" b)]), []), b+1)
        | (Invalid(err), _) -> ((Invalid(StructureError(sprintf "Can not define 'name' or 'group' in %d object" b) :: err), []), b+1)

let sortingRules value =
    let groups = correctListGroups value []
    match groups with
    | Some(gr) ->
        let (a, b) = gr |> List.fold (fun acc h ->  (pushDictInResult h acc)) ((Valid, []), 1)
        a
    | None -> (Invalid([StructureError(sprintf "All elems in the first value should be objects")]), [])

let deleteGroupAndNameItem elem =
    List.filter (fun (key, value) -> (key <> "group" && key <> "name")) elem

let deleteGroupAndName value =
    match value with
    | (Valid, old) -> (Valid, (List.map (fun (a, x) -> (a, (deleteGroupAndNameItem x))) old))
    | (Invalid(err), a) -> (Invalid(err), a)

let addError new_err errorList =
        match new_err with
        | Valid ->  errorList
        | Invalid(err) ->
            match errorList with
                | Valid -> new_err
                | Invalid(strError) -> Invalid(List.append err strError)

let checkPath rulesForOneFile =
    let (name, rules) = rulesForOneFile
    let count = List.fold (fun acc (key, value) -> if key = "path" then acc + 1 else acc) 0 rules
    if count <> 1 then
        Invalid([StructureError(sprintf "Can not define path of file with name '%s'" name)])
    else
        let (key, value) = List.find (fun (key, value) -> key = "path") rules
        match value with
            | Str(path) ->
                if (File.Exists(path)) then
                    Valid
                else Invalid([StructureError(sprintf "Can not find file with name '%s' in '%s'" name path)])
            | _ -> Invalid([StructureError(sprintf "Unexpected path for file with name '%s'" name)])


let checkPathAll value =
    let (err, x) = value
    let res = List.fold (fun acc elem -> addError (checkPath elem) acc) err x
    match res with
    | Valid -> (Valid, x)
    | Invalid(err) -> (Invalid(err), x)

let checkFormat rulesForOneFile =
    let (name, rules) = rulesForOneFile
    let count = List.fold (fun acc (key, value) -> if key = "format" then acc + 1 else acc) 0 rules
    let countReq = List.fold (fun acc (key, value) -> if key = "required" then acc + 1 else acc) 0 rules
    let err1 =
        if count > 1 then
            Invalid([StructureError(sprintf "Can not define format of file with name '%s'" name)])
        elif count = 1 then
            let (key, value) = List.find (fun (key, value) -> key = "format") rules
            match value with
                | Str(path) ->
                    if (path = "json") then
                        Valid
                    elif (path = "xml") then
                        Valid
                    elif (path = "yaml") then
                        Valid
                    else Invalid([StructureError(sprintf "Wrong format of file with name '%s'" name)])
                | _ -> Invalid([StructureError(sprintf "Unexpected format for file with name '%s'" name)])
        elif countReq <> 0 then
            Invalid([StructureError(sprintf "Can not find the key 'format' in file with name '%s'" name)])
        else
            Valid
    err1


let checkFormatAll value =
    let (err, x) = value
    let res = List.fold (fun acc elem -> addError (checkFormat elem) acc) err x
    match res with
    | Valid -> (Valid, x)
    | Invalid(err) -> (Invalid(err), x)


let popResult value =
    let (a, x) = value
    a


let rec requiredCheck mainKey elem name path mess =
    let (key, value) = elem
    match value with
    | Group(gr) ->
        match mess with
        | Group(new_mess) ->
            let ex = List.exists (fun (k, v) ->  k = key) new_mess
            if (ex) then
                let (_, res1) = List.find (fun (key1, value1) -> key1 = key) new_mess
                List.fold (fun acc (k, v) -> addError (requiredCheck (mainKey+"."+k) (k, v) name path res1) acc) (Valid) gr
            else
                Invalid([StructureError(sprintf "In the file with name '%s' there is no key '%s'" name mainKey)])
        | _ -> Invalid([StructureError(sprintf "In the file with name '%s' there is uncorrect type of value with key '%s'" name mainKey)])


    | Array(arr) ->
        match mess with
        | Array(arr) ->
            let (new_mess, _) = List.fold (fun (acc, i) elem -> ((List.append acc [((sprintf "%i" i), elem)]), i+1)) ([], 0) (arr |> Array.toList)
            let ex = List.exists (fun (k, v) ->  k = key) new_mess
            if (ex) then
                let (_, res1) = List.find (fun (key1, value1) -> key1 = key) new_mess
                let (res, _) = List.fold (fun (acc, i) elem -> (addError (requiredCheck (mainKey+".["+(sprintf "%i" i)+"]") ((sprintf "%i" i), elem) name path res1) acc), i+1) (Valid, 0) (arr |> Array.toList)
                res
            else
                Invalid([StructureError(sprintf "In the file with name '%s' there is no key '%s'" name mainKey)])
        | _ -> Invalid([StructureError(sprintf "In the file with name '%s' there is uncorrect type of value with key '%s'" name mainKey)])
    | Str(str) ->
        if (key = "type") then
            match mess with
            | Null when str = "null" -> (Valid)
            | Int(_) when str = "int" -> (Valid)
            | Float(_) when str = "float" -> (Valid)
            | Bool(_) when str = "bool" -> (Valid)
            | Str(_) when str = "string" -> (Valid)
            | _ -> Invalid([StructureError(sprintf "In the file with name '%s' there is unexpected type of value with the key '%s', here should be the type '%s'" name mainKey str)])
        else
            (Valid)
    | _ -> (Valid)


let requiredCheckAll name rules path mess =
    let arrRequired = List.fold (
        fun acc (key, value) ->
            (if (key = "required") then
                match value with
                    | Group(gr) -> List.append acc gr
                    | _ -> acc
                else
                    acc)) [] rules
    List.fold (fun acc (k, v) -> addError (requiredCheck k (k, v) name path mess) acc) (Valid) arrRequired



let getContentwithCheck rulesForOneFile =
    let (name, rules) = rulesForOneFile
    let (key, value) = List.find (fun (key, value) -> key = "path") rules

    let path =
        match value with
            | Str(p) ->
                p
            | _ -> ""
    // printfn "%A" path
    let (key2, value2) = List.find (fun (key, value) -> key = "format") rules
    let format =
        match value2 with
            | Str(f) ->
                f
            | _ -> ""
    let err =
        // printfn "%A" format
        if (format = "json") then
            parsingJson path
        elif (format = "xml") then
            Ok([("w1", Null)])
        else
            Ok([("w2", Null)])
    match err with
    | Ok message ->
        let res2 = Ok(ValidatedConfunMap([]))//message |> MapValidator.validate
        match res2 with
        | Ok (ValidatedConfunMap mess)  ->
            requiredCheckAll name rules path (Group(mess))
        | Error errors ->
            (Invalid(ValidationError(sprintf "For file with name '%s' with path '%s':" name path) :: [(toOneStr errors ValidationError)]))
    | Error errors ->
        // printfn "%A" errors
        (Invalid(ParsingError(sprintf "For file with name '%s' with path '%s':\n" name path) :: [errors]))

let getContentwithCheckAll value =
    let (err, x) = value
    match err with
    | Valid -> List.fold (fun acc elem -> addError (getContentwithCheck elem) acc) (Valid) x
    | Invalid(err) -> Invalid(err)

let checkExcessType name key (key1, value) =
    if (key1 = "type") then
        match value with
        | Str(str) ->
            if (str = "null" || str = "int" || str = "float" || str = "bool" || str = "string") then
                (Valid)
            else
                Invalid([StructureError(sprintf "In the description of file with name '%s' there is unexpected type of value with the key '%s'" name key+"."+key1)])
        | _ -> Invalid([StructureError(sprintf "In the description of file with name '%s' the value of the key '%s' should have type 'string'" name key+"."+key1)])
    else
        Invalid([StructureError(sprintf "In the description of file with name '%s' there is unexpected value of the key '%s'" name key+"."+key1)])


let rec checkExcessArr name key gr =
    match gr with
    | Group(gr1) -> (checkExcessGroup name (key) gr1)
    | Array(arr) ->
        let (res, _) = List.fold (fun (acc, i) elem -> ((addError (checkExcessArr name (key+".["+(sprintf "%d" i)+"]") elem) acc), i+1)) (Valid, 0) (arr |> Array.toList)
        res
    | _ -> (Invalid([StructureError(sprintf "In the description of file with name '%s' the value of the key '%s' should have type 'object' or 'array'" name key)]))

and checkExcessGroup name key gr =
    List.fold (fun acc (key1, value) ->
        match value with
        | Group(gr1) -> addError (checkExcessGroup name (key+"."+key1) gr1) acc
        | Array(arr) ->
            let (res, _) = List.fold (fun (acc, i) elem -> ((addError (checkExcessArr name (key+"."+key1+".["+(sprintf "%d" i)+"]") elem) acc), i+1)) (Valid, 0) (arr |> Array.toList)
            res
        | _ -> addError (checkExcessType name key (key1, value)) acc
    ) (Valid) gr

let checkExcess rulesForOneFile =
    let (name, rules) = rulesForOneFile
    List.fold (fun acc (key, value) ->
        if (key = "required") then
            match value with
            | Group(gr) -> addError (checkExcessGroup name key gr) acc
            | _ -> addError (Invalid([StructureError(sprintf "In the description of file with name '%s' the value of the key 'requied' should have type 'object'" name)])) acc
        elif (key <> "format" && key <> "path") then
            addError (Invalid([StructureError(sprintf "In the description of file with name '%s' there is the unexpected key '%s'" name key)])) acc
        else
            acc
    ) (Valid) rules


let checkExcessAll value =
    let (err, x) = value
    let res = List.fold (fun acc elem -> addError (checkFormat elem) acc) err x
    match res with
    | Valid -> (Valid, x)
    | Invalid(err) -> (Invalid(err), x)


let verificationConfig (config : ConfigParam list) =
    match config with
    | (key, Array(value)) :: [] ->
        let res = (sortingRules (value |> Array.toList)) |> deleteGroupAndName
        match res with
        | (Valid, x) -> res |> checkPathAll |> checkFormatAll |> checkExcessAll |> getContentwithCheckAll // |> popResult
        | (Invalid(err), _) -> Invalid(err)
    | (key, _) :: [] -> Invalid([StructureError(sprintf "uncorrect type of value with key'%s'" key)])
    | _ -> Invalid([StructureError(sprintf "Wrong number of rules. There should be one, given %d" config.Length)])
