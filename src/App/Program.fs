open System

open Hopac
open Argu

open Chiron
open Chiron.Operators

module D = Json.Decode
module E = Json.Encode
module I = Inference.Json
module S = Serialization.Json

open Client

[<EntryPoint>]
let main argv =
    try
        let args = Args("fss", argv)
        let query = @"_sourceCategory=harbour/container-health"
        let client =  Client.FromArgs args
        match client.Query query args.FromTime args.ToTime with
            | Fail fails -> printfn "Query Fail %A" fails
            | Error error -> printfn "Query Error %A" error
            | Pass ticket ->
                printfn "Sleeping..."
                run <| job { do! timeOut (TimeSpan.FromSeconds 3.0)}
                match client.WaitOn ticket with
                    | Fail fails -> printfn "Status Fail %A" fails
                    | Error error -> printfn "Status Error %A" error
                    | Pass status -> printfn "Status %A" status
    with
        | Failure(msg) -> printfn "Error: %s" msg
        | :? ArguParseException as ex -> printfn "%s" ex.Message
    0
