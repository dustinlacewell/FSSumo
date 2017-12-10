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
    let query = @"_sourceCategory=harbour/container-health"
    try
        Args("fss", argv)
        |> Client.query query |> function
            | QueryResponse.Ticket t -> printfn "Ticket: %A" t
            | QueryResponse.Error e -> printfn "Error %A" e
            | QueryResponse.Fail e -> printfn "Fail! %A" e
    with
        | Failure(msg) -> printfn "Error: %s" msg
        | :? ArguParseException as ex -> printfn "%s" ex.Message
    0
