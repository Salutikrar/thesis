namespace Confun.Check.Syntax.Json

open System.Diagnostics.CodeAnalysis
open System.IO
open Confun.Core.Processing.Api
open Confun.Core.Types
open Confun.Core.Processing


module CheckConfigs =
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
                    else Invalid([StructureError(sprintf "Can not find the file with name '%s' in '%s'" name path)])
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


    let toOneStr strList name =
        name(List.fold (fun acc elem -> (acc + "\t" + ConfunError.toString elem + "\n")) "" strList)

    let regexValidation name path mainKey pattern text str =
        let fullPattern = sprintf "^%s$" pattern
        let str1 =
            if (str = "") then (sprintf "regex '%s'" fullPattern)
            else (sprintf "type '%s'" str)
        let regexResult = System.Text.RegularExpressions.Regex.Match(text, fullPattern)
        if not regexResult.Success then
            Invalid [ ValidationError (sprintf "In the file with name '%s' text '%s' (of value with key '%s') is not matched by %s" name text mainKey str1) ]
        else
            Valid

    let rec requiredCheck mainKey elem name path mess =
        // printfn "%A %A %A %A %A\n\n\n" mainKey elem name path mess
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
            | Group(gr) ->
                let ex = List.exists (fun (k, v) ->  k = key) gr
                if (ex) then
                    let (_, res1) = List.find (fun (key1, value1) -> key1 = key) gr
                    match res1 with
                    | Array(arr1) ->
                        let groupList1 = arr1 |> List.ofArray
                        let groupList = arr |> List.ofArray
                        if (groupList.Length <> groupList1.Length) then
                            // printfn "%A\n\n\n%A" (groupList) (groupList1)
                            Invalid([StructureError(sprintf "In the file with name '%s' there is missmatched in array length of value with key '%s'" name mainKey)])
                        else
                            let (result, _) = List.zip groupList groupList1 |> List.fold (fun (acc, i) (f, s) ->  ((addError (
                                match f with
                                | Group(grElem) -> List.fold (fun acc1 (k, v) -> addError (requiredCheck (mainKey+".["+(sprintf "%i" i)+"]."+k) (k, v) name path s) acc1) (Valid) grElem
                                | _ -> (Valid)) acc), i+1)) (Valid, 0)

                            result
                    | _ -> Invalid([StructureError(sprintf "In the file with name '%s' there is uncorrect type of value with key '%s'" name mainKey)])
                else
                    Invalid([StructureError(sprintf "In the file with name '%s' there is no key '%s'" name mainKey)])
            | _ -> Invalid([StructureError(sprintf "In the file with name '%s' there is uncorrect type of value with key '%s'" name mainKey)])
        | Str(str) ->
            // printfn "%s\n" str
            if (key = "type") then
                match mess with
                | Null when str = "null" -> (Valid)
                | Int(_) when str = "int" -> (Valid)
                | Float(_) when str = "float" -> (Valid)
                | Bool(_) when str = "bool" -> (Valid)
                | Str(_) when str = "string" -> (Valid)
                | Str(x) when str = "IPv4" -> regexValidation name path mainKey @"((25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(25[0-5]|2[0-4]\d|[01]?\d\d?)" x str
                | Str(x) when str = "IPv6" -> regexValidation name path mainKey @"((^|:)([0-9a-fA-F]{0,4})){1,8}$" x str
                | Str(x) when str = "data" -> regexValidation name path mainKey @"(0[1-9]|[12][0-9]|3[01])[- /.](0[1-9]|1[012])[- /.](19|20)\d\d" x str
                | Str(x) when str = "url" -> regexValidation name path mainKey @"(https?:\/\/)?([\w\.]+)\.([a-z]{2,6}\.?)(\/[\w\.]*)*\/?" x str
                | Str(x) when str = "E-mail" -> regexValidation name path mainKey @"^[-\w.]+@([A-z0-9][-A-z0-9]+\.)+[A-z]{2,4}$" x str
                | _ -> Invalid([StructureError(sprintf "In the file with name '%s' there is unexpected type of value with the key '%s', here should be the type '%s'" name mainKey str)])
            elif (key = "regex") then
                match mess with
                | Str(x) -> regexValidation name path mainKey str x ""
                | _ -> Invalid([StructureError(sprintf "In the file with name '%s' there is unexpected type of value with the key '%s' for checking regex, should be 'string'" name mainKey)])
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
                JsonValidation.parsingJson path
            elif (format = "xml") then
                XmlValidation.parsingXml path
            else
                YamlValidation.parsingYaml path
        match err with
        | Ok message ->
            let res2 = message |> MapValidator.validate
            match res2 with
            | Ok (ValidatedConfunMap mess) ->
                requiredCheckAll name rules path (Group(mess))
            | Error errors ->
                (Invalid(ValidationError(sprintf "For file with name '%s' with path '%s':" name path) :: [(toOneStr errors ValidationError)]))
        | Error errors ->
            // printfn "%A" errors
            (Invalid(ParsingError(sprintf "For file with name '%s' with path '%s':" name path) :: [errors]))

    let getContentwithCheckAll value =
        let (err, x) = value
        match err with
        | Valid -> List.fold (fun acc elem -> addError (getContentwithCheck elem) acc) (Valid) x
        | Invalid(err) -> Invalid(err)

    let checkExcessType name key (key1, value) =
        if (key1 = "type") then
            match value with
            | Str(str) ->
                if (str = "null" || str = "int" || str = "float" || str = "bool" || str = "string" || str = "IPv4" || str = "IPv6" || str = "data" || str = "url" || str = "E-mail") then
                    (Valid)
                else
                    Invalid([StructureError(sprintf "In the description of file with name '%s' there is unexpected type of value with the key '%s'" name key+"."+key1)])
            | _ -> Invalid([StructureError(sprintf "In the description of file with name '%s' the value of the key '%s' should have type 'string'" name key+"."+key1)])
        elif (key1 = "regex") then
            match value with
            | Str(str) -> (Valid)
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
        let res = List.fold (fun acc elem -> addError (checkExcess elem) acc) err x
        match res with
        | Valid -> (Valid, x)
        | Invalid(err) -> (Invalid(err), x)

    let popResult value =
        match value with
        | Valid -> Valid
        | Invalid strList ->
            Invalid(StructureError(List.fold (fun acc elem -> (acc + "\n" + ConfunError.toString elem)) "" strList))

    let verificationConfig (config : ConfigParam list) =
        match config with
        | (key, Array(value)) :: [] ->
            let res = (sortingRules (value |> Array.toList)) |> deleteGroupAndName
            match res with
            | (Valid, x) -> res |> checkPathAll |> checkFormatAll |> checkExcessAll |> getContentwithCheckAll |> popResult
            | (Invalid(err), _) -> popResult (Invalid(err))
        | (key, _) :: [] -> popResult (Invalid([StructureError(sprintf "uncorrect type of value with key'%s'" key)]))
        | _ -> popResult (Invalid([StructureError(sprintf "Wrong number of rules. There should be one, given %d" config.Length)]))
