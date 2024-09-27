open System
open FParsec
open AST
open Parser
open Interpreter


let runSourceFile fpath =
    match runParserOnFile program () fpath System.Text.Encoding.UTF8 with
    | Success(result, _, _) ->
        interpret result |> ignore
    | Failure(errorMsg, _, _) ->
        printfn "Parsing failed with error: %s" errorMsg

[<EntryPoint>]
let main argv =
    match argv with
    | [|fpath|] -> runSourceFile fpath
    | _ -> printfn "Usage: kfalang <source file>"
    0