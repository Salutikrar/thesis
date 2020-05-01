﻿// Learn more about F# at http://fsharp.org

open Confun.Core.Types
open Confun.Core.Processing

let m:ConfunMap = [
            "SrcPort", Port 10us
            "", Port 8080us
            "DatabaseConnection", Group [
                "ConnectionString", Str "ms-sql.localhost:9090"
                "", Str "ms-sql.localhost:9090"
                "ConnectionString3", Str "ms-sql.localhost:9090"
            ]
        ]

[<EntryPoint>]
let main argv =
    let res = MapValidator.validate m
    match res with
    | Error error ->
                    printf "%s" (ConfigGenerator.printErrors error)
                    1
    | Ok validatedResult -> 
                    printf "%s" (ConfigGenerator.generateAFormat validatedResult)
                    0
