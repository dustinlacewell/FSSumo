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

type QueryParams =
    { Query: string
      From: DateTime
      To: DateTime }

    static member Encode(x:QueryParams) =
        E.required E.string "query" x.Query >>
        E.required E.string "from" (x.From.ToString(timeFormat)) >>
        E.required E.string "to" (x.To.ToString(timeFormat)) >>
        E.required E.string "timeZone" "UTC"

    static member ToJson(x:QueryParams) =
        E.buildWith QueryParams.Encode x

type Link =
    { Rel: string; Href: string }

    static member Create rel href =
        { Rel = rel; Href = href; }

    static member ToJson(x:Link) =
        E.required E.string "rel" x.Rel >>
        E.required E.string "href" x.Href

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

type Status = NotStarted | GatheringResults | Paused | ForcePaused | DoneGatheringResults | Canceled

type HistogramBucket = {Start: int; Length: int; Count: int}

type JobStatus =
    { Status: Status
      MessageCount: int
      RecordCount: int
      PendingErrors: string list
      PendingWarnings: string list
      HistogramBuckets: HistogramBucket list }

type Client(baseURL, accessID, accessKey) =

    let _auth = Request.basicAuthentication accessID accessKey

    static member FromArgs(args:Args) = Client(args.BaseUrl, args.AccessId, args.AccessKey)

    member val AccessID = accessID
    member val BaseURL = baseURL with get

    member this.URLFor =
        sprintf "%s/%s" this.BaseURL

    member this.Send =
        Request.responseAsString >> run

    member this.Create httpMethod endpoint =
        let url =  this.URLFor endpoint
        Request.createUrl httpMethod url |> _auth

    member this.Get = this.Create Get
    member this.Post = this.Create Post

    member this.WithJSON body request =
        request
        |> Request.setHeader (Custom ("Content-Type", "application/json"))
        |> Request.setHeader (Custom ("Accept", "application/json"))
        |> Request.bodyString (body |> I.serialize)

    member this.Query query fromTime toTime : QueryResponse =
        this.Post "search/jobs"
        |> this.WithJSON { Query = query; From = fromTime; To = toTime; }
        |> this.Send
        |> I.deserialize |> function
            | JPass x -> x
            | JFail f -> QueryResponse.Failures [f]

    // member this.Status jobID : JobStatus =
    //     this.Get (sprintf "search/jobs/%s" jobID)
