open System

open Argu


open SumoFS.Core.Client

// let query = @"_sourceCategory=harbour/container-health"

[<EntryPoint>]
let main argv =
    try
        match ParseArgs argv with
            | Options.NoCommand -> printfn "%s" "Subcommand required!"
            | Options.Query o ->
                let client = SumoClient.New o.BaseUrl o.Id o.Key
                match client.Query o.Query o.From o.To with
                    | Fail fails -> printfn "Query Fail %A" fails
                    | Error error -> printfn "Query Error %A" error
                    | Pass ticket -> printfn "Query Pass %A" ticket
            // | Options.Status o ->
            //     let client = Client(o.BaseUrl, o.Id, o.Key)
            //     let ticket = Ticket.Create
            //     run <| job { do! timeOut (TimeSpan.FromSeconds 3.0)}
            //     match client.WaitOn ticket with
            //         | Fail fails -> printfn "Status Fail %A" fails
            //         | Error error -> printfn "Status Error %A" error
            //         | Pass status -> printfn "Status %A" status
    with
        | Failure(msg) -> printfn "Error: %s" msg
        | :? ArguParseException as ex -> printfn "%s" ex.Message
    0
