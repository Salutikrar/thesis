namespace Confun.Check.Syntax.Json

open System.Diagnostics.CodeAnalysis

open Confun.Core.Processing.Api
open Confun.Core.Types

module JsonValidation =
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
