module SumoFS.Core.Client

open System
open System.Net.Http
open System.Text

open Chiron
open Chiron.Operators

module D = Json.Decode
module E = Json.Encode
module I = Inference.Json

open HttpFs.Client
open Hopac

let timeFormat = "yyyy-MM-ddTHH:mm:ssZ"

let emit s v = printfn "%s: %A" s v
let convert (f:(Json -> 'a)) v = JsonResult.pass (f(v))
let attr attr = E.required E.string attr

type Query =
    { Query: string
      From: DateTime
      To: DateTime }

    static member Encode(x:Query) =
        E.required E.string "query" x.Query >>
        E.required E.string "from" (x.From.ToString(timeFormat)) >>
        E.required E.string "to" (x.To.ToString(timeFormat)) >>
        E.required E.string "timeZone" "UTC"

    static member ToJson(x:Query) =
        E.buildWith Query.Encode x

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

    static member Decode = jsonDecoder {
            let! status = D.required D.int "status"
            let! id = D.required D.string "id"
            let! code = D.required D.string "code"
            let! message = D.required D.string "message"
            return Error.Create status id code message }

    static member FromJson(_:Error) = Error.Decode

type Errors =
    | Submit of Error
    | Poll of Error
    | Fetch of Error

type Response<'a> =
    | Pass of 'a
    | Error of Errors
    | Fail of string list

type State =
    Initialized
    | NotStarted
    | GatheringResults
    | Paused
    | ForcePaused
    | DoneGatheringResults
    | Canceled
    | Unknown

    static member Decode (s:Json) =
        match D.string s with
            | JFail x -> State.Unknown
            | JPass x ->
                match x with
                    | "NOT STARTED" -> State.NotStarted
                    | "GATHERING RESULTS" -> State.GatheringResults
                    | "PAUSED" -> State.Paused
                    | "FORCE PAUSED" -> State.ForcePaused
                    | "DONE GATHERING RESULTS" -> State.DoneGatheringResults
                    | "CANCELED" -> State.Canceled
                    | _ -> State.Unknown
    static member FromJson(_: State) = State.Decode

type HistogramBucket =
    {Start: int; Length: int; Count: int}

    static member Decode = jsonDecoder {
        let! start = D.required D.int "start"
        let! length = D.required D.int "length"
        let! count = D.required D.int "count"
        return { Start = start; Length = length; Count = count; } }
    static member FromJson(_: HistogramBucket) = HistogramBucket.Decode


type Status =
    { State: State
      MessageCount: int
      RecordCount: int
      PendingErrors: string list
      PendingWarnings: string list
      HistogramBuckets: HistogramBucket list }

    static member Create () = { State = Initialized
                                MessageCount = 0
                                RecordCount = 0
                                PendingErrors = []
                                PendingWarnings = []
                                HistogramBuckets = []}


    static member Decode = jsonDecoder {
        let! state = D.required (State.Decode >> JsonResult.pass) "state"
        let! messages = D.required D.int "messageCount"
        let! records = D.required D.int "recordCount"
        let! errors = D.required I.decode "pendingErrors"
        let! warnings = D.required I.decode "pendingWarnings"
        let! buckets = D.required I.decode "histogramBuckets"
        return { State = state
                 MessageCount = messages
                 RecordCount = records
                 PendingErrors = errors
                 PendingWarnings = warnings
                 HistogramBuckets = buckets }
        }
    static member FromJson(_:Status) = Status.Decode

module Request =
    let withJSON body =
        Request.setHeader (ContentType <| ContentType.create("application", "json", charset = Encoding.UTF8))
        >> Request.bodyString (body |> I.serialize)

    let deserializeWith (decode:Decoder<Json, 'a>) (response:string) :Response<'a>  =
        Serialization.Json.deserializeWith decode response |> function
            | JPass ticket -> Pass ticket
            | JFail f ->
                Serialization.Json.deserializeWith Error.Decode response |> function
                    | JPass error -> Response.Error (Errors.Submit error)
                    | JFail fail -> Fail (JsonFailure.toStrings fail)

type SumoClient =
    { baseUrl: string
      username: string
      password: string
      client: HttpClient }

    static member New baseUrl username password =
        { baseUrl = baseUrl
          username = username
          password = password
          client = new HttpClient(new HttpClientHandler(UseCookies = true))}

    member this.URL endpoint = Uri <| sprintf "%s/%s" this.baseUrl endpoint

    member this.Request httpMethod endpoint =
        let url = this.URL endpoint
        { Request.create httpMethod url with httpClient = this.client }
        |> Request.basicAuthentication this.username this.password
        |> Request.setHeader (Accept "application/json")

    member this.Get = this.Request Get
    member this.Post = this.Request Post

    member this.Query query fromTime toTime :Response<Ticket> =
        this.Post "search/jobs"
        |> Request.withJSON { Query = query; From = fromTime; To = toTime; }
        |> Request.responseAsString |> run
        |> Request.deserializeWith Ticket.Decode
        // |> function
        //     | Error x -> Error x
        //     | Fail x -> Fail x
        //     | Pass ticket ->
        //         this.Get (sprintf "search/jobs/%s" ticket.Id)
        //         |> Request.setHeader (Accept "application/json")
        //         |> fun r -> printfn "Request\n%A" r; r
        //         |> Request.responseAsString |> run
        //         |> Request.deserializeWith Status.Decode

//     member this.WaitOn jobId request : Response<Status> =
//         let status = Status.Create()
//         let url = sprintf "search/jobs/%s" ticket.Id

//         let fetch () = job {
//             let request =
//                 this.Get url
//                 |> _auth
//                 |> this.AddCookies
//                 |> Request.setHeader (Accept "application/json")
//                 |> Request.setHeader (UserAgent "curl/7.56.0")

//             do printfn "Request %A" request
//             let! response =  Request.responseAsString request
//             do printfn "Response %A" response
//             return this.DeserializeResponseWith Status.Decode response }

//         let timeout seconds = job {
//             do! timeOut (TimeSpan.FromSeconds seconds)
//             return Fail ["Query timed out!"] }

//         let checkOn () = job {
//             let! w = Promise.start (fetch())
//             let! t = Promise.start (timeout 5.0)
//             return! Alt.choose [Promise.read t
//                                 Promise.read w] }

//         let rec waitOn (status:Status) =
//             match status.State with
//                 | State.Canceled -> Response.Fail ["Job was canceled"]
//                 | State.DoneGatheringResults -> Pass status
//                 | State.Initialized
//                 | _ ->
//                     match checkOn() |> run with
//                         | Pass x -> waitOn x
//                         | Fail x -> Fail x
//                         | Error x -> Error x

//         waitOn status


// type Client(baseURL, accessID, accessKey) =

//     let mutable _cookies = Map<string, string> []
//     let _client = 
//     let _auth = Request.basicAuthentication accessID accessKey

//     member val AccessID = accessID
//     member val BaseURL = baseURL with get

//     member this.URLFor =
//         sprintf "%s/%s" this.BaseURL

//     member this.AddCookies request =
//         let folder s k v =
//             printfn "Adding cookie: %s - %s" k v
//             s |> Request.cookie (Cookie.create(k, v, path = "/"))
//         printfn "Current cookies %A" _cookies
//         let request = Map.fold folder request _cookies
//         printfn "Request after cookies %A" request
//         request

//     member this.Send request = run <| job {
//         let request = this.AddCookies request
//         let! response = getResponse request
//         printfn "RESPONSE %A" response
//         do printfn "Server provided cookies %A" response.cookies
//         do _cookies <- response.cookies
//         do printfn "Saved cookies %A" _cookies
//         return! Request.responseAsString request }

//     member this.Create httpMethod endpoint =
//         let url =  this.URLFor endpoint
//         printfn "Created URL: %s" url
//         { Request.createUrl httpMethod url with httpClient = _client } |> _auth
//         |> Request.setHeader (Accept "application/json")

//     member this.Get = this.Create Get
//     member this.Post = this.Create Post

//     member this.WithJSON body request =
//         request
//         |> Request.setHeader (Custom ("Content-Type", "application/json"))
//         |> Request.bodyString (body |> I.serialize)

//     member this.DeserializeResponseWith (decode:Decoder<Json, 'a>) (response:string) :Response<'a>  =
//         Serialization.Json.deserializeWith decode response |> function
//             | JPass ticket -> Pass ticket
//             | JFail f ->
//                 Serialization.Json.deserializeWith Error.Decode response |> function
//                     | JPass error -> Response.Error (Errors.Submit error)
//                     | JFail fail -> Fail (JsonFailure.toStrings fail)

//     member this.Query query fromTime toTime : Response<Ticket> =
//         this.Post "search/jobs"
//         |> this.WithJSON { Query = query; From = fromTime; To = toTime; }
//         |> Request.autoFollowRedirectsDisabled
//         |> this.Send |> (fun x -> printfn "RESPONSE BODY\n%A" x; x)
//         |> this.DeserializeResponseWith Ticket.Decode

