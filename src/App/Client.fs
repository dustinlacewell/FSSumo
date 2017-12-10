module Client

open System
open HttpFs.Client

open Hopac

open Chiron
open Chiron.Operators

module D = Json.Decode
module E = Json.Encode
module I = Inference.Json

let timeFormat = "yyyy-MM-ddTHH:mm:ssZ"

let convert (f:(Json -> 'a)) v = JsonResult.pass (f(v))
let attr attr = E.required E.string attr

type Error =
    { Status: int
      Id: string
      Code: string
      Message: string }

    static member Create status id code message =
        { Status = status
          Id = id
          Code = code
          Message = message }

    static member Decode
        with get() = jsonDecoder {
            let! status = D.required D.int "status"
            let! id = D.required D.string "id"
            let! code = D.required D.string "code"
            let! message = D.required D.string "message"
            return Error.Create status id code message }
    static member FromJson(_:Error) = Error.Decode

type QueryParams =
    { Query: string
      From: DateTime
      To: DateTime }

    static member ToJson(x:QueryParams) =
        let query = E.required E.string "query" x.Query
        let from = E.required E.string "from" (x.From.ToString(timeFormat))
        let too = E.required E.string "to" (x.To.ToString(timeFormat))
        let timeZone = E.required E.string "timeZone" "UTC"
        let objEncoder = query >> from >> too >> timeZone >> JsonObject.toJson
        objEncoder JsonObject.empty
        // let t = fun jObj ->
        //     jObj
        //     |> E.required E.string "query" x.Query
        //     |> E.required E.string "from" (x.From.ToString(timeFormat))
        //     |> E.required E.string "to" (x.To.ToString(timeFormat))
        //     |> E.required E.string "from" "UTC"
        // E.jsonObjectWith
            // attr "query" x.Query >>
            // attr "from" (x.To.ToString(timeFormat)) >>
            // attr "to" (x.To.ToString(timeFormat)) >>
            // attr "timeZone" "UTC"
            // <| jObj
        // json { return {Query = null; From = DateTime.Now; To = DateTime.Now; }} 

type Link =
    { Rel: string; Href: string }

    static member Create rel href =
        { Rel = rel; Href = href; }

    static member ToJson(x:Link) =
        E.required E.string "rel" x.Rel
        >> E.required E.string "href" x.Href

    static member FromJson(_:Link) =
        jsonDecoder {
            let! rel = D.required D.string "rel"
            let! url = D.required D.string "href"
            return Link.Create rel url }

type Ticket =
    { Id: string; Link: Link }

    static member Create id link =
        { Id = id; Link = link; }

    static member Decode
        with get() = jsonDecoder {
            let! rel = D.required D.string "id"
            let! link = D.required I.decode "link"
            return Ticket.Create rel link }
    static member FromJson(_:Ticket) = Ticket.Decode
        // Ticket.Unit
        // <!> D.required D.string "id"
        // <*> D.required I.decode "url"

type QueryResponse =
    | Ticket of ticket: Ticket
    | Error of error: Error
    | Fail of fail: string list

    static member UnknownError
        with get() = Error (Error.Create 0 null null null)

    static member Failures (fs:JsonFailure list) =
        Fail [ for x in fs -> (x.ToString()) ]

    static member FromJson(_:QueryResponse) = fun json ->
        match Ticket.Decode json with
        | JPass x -> JPass <| QueryResponse.Ticket x
        | JFail f ->
            match Error.Decode json with
            | JPass x -> JPass <| QueryResponse.Error x
            | JFail f2 -> JPass <| QueryResponse.Failures [f; f2]

let send req = Request.responseAsString req |> run

let create m ep (args:Args) =
    let url = sprintf "%s/%s" args.BaseUrl ep
    let auth = Request.basicAuthentication args.AccessId args.AccessKey
    Request.createUrl m url |> auth

let get ep (args:Args) =
    create Get ep args

let post ep (args:Args) =
    create Post ep args

let jsonp body req =
    req
    |> Request.setHeader (
        Custom ("Content-Type", "application/json"))
    |> Request.setHeader (
        Custom ("Accept", "application/json"))
    |> Request.bodyString (body |> I.serialize |> fun x -> printfn "JSON BODY\n%A" x; x)

let query body (args:Args) : QueryResponse =
    let req = post "search/jobs" args
    let requ = jsonp { Query = body; From = args.FromTime; To = args.ToTime; } req
    send requ |> (fun x -> printfn "Response: %s" x; x) |> I.deserialize |> function
        | JPass x -> x
        | JFail f -> QueryResponse.Failures [f]


