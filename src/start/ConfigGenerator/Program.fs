// Learn more about F# at http://fsharp.org

open System

open Confun.Check.Syntax.Json
open Confun.Core.Processing
open Confun.Core.Types
open Confun.Core.Processing.Api

[<EntryPoint>]
let main argv =
    let res = JsonValidation.parsingJson argv[0]
    match res with
    | Ok messages ->
        let res2 = messages |> MapValidator.validate
        match res2 with
        | Ok message ->
            let res3 = CheckConfigs.verificationConfig messages
            match res3 with
            | Valid ->
                printfn "All configs are valid"
                0
            | Invalid err ->
                printfn "%s\n" (ConfunError.toString err)
                1
        | Error error ->
            printfn "%s\n"  (ConfunError.toString (CheckConfigs.toOneStr error ValidationError))
            1
    | Error errors ->
        printfn "%s\n" (ConfunError.toString errors)
        1
