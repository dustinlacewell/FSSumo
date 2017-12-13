[<AutoOpen>]
module Args

open System
open Argu

[<CliPrefix(CliPrefix.Dash)>]
type QueryArgs =
    | [<GatherUnrecognized>][<Last>][<ExactlyOnce>] Query of string
    | [<AltCommandLine("--from")>][<ExactlyOnce>] FromTime of string
    | [<AltCommandLine("--to")>] [<ExactlyOnce>] ToTime of string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Query _ -> "Sumo search query"
            | FromTime _ -> "query start time"
            | ToTime _ -> "query end time"
// and StatusArgs =
//     | [<Mandatory>] [<AltCommandLine("-j")>] JobId of string
//     | [<Mandatory>] ELBCookie of string
//     | [<Mandatory>] SessionCookie of string
// with
//     interface IArgParserTemplate with
//         member this.Usage =
//             match this with
//             | JobId _ -> "ID of job to poll"
//             | ELBCookie _ -> "Cookie for AWS ELB"
//             | SessionCookie _ -> "Cookie for session"
and [<RequireSubcommand>] GlobalArgs =
    | [<Inherit>] AccessId of string
    | [<Inherit>] AccessKey of string
    | [<Inherit>] BaseUrl of string
    | [<CliPrefix(CliPrefix.None)>] Query of ParseResults<QueryArgs>
//    | [<CliPrefix(CliPrefix.None)>] Status of ParseResults<StatusArgs>
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | AccessId _ -> "API access ID."
            | AccessKey _ -> "API access key."
            | BaseUrl _ -> "Base API URL"
            | Query _ -> "Submit a query."
//            | Status _ -> "Poll query status"

let baseUrl (args:ParseResults<GlobalArgs>) = args.GetResult (<@ BaseUrl @>, defaultValue = BASEURL)

let accessId (args:ParseResults<GlobalArgs>) = args.GetResult (<@ AccessId @>, defaultValue = ACCESSID)

let accessKey (args:ParseResults<GlobalArgs>) = args.GetResult (<@ AccessKey @>, defaultValue = ACCESSKEY)

let fromTime (args:ParseResults<'t>) =
    args.GetResult (<@ FromTime @>)
    |> DateTime.Parse
    |> fun x -> x.ToUniversalTime()

let toTime (args:ParseResults<'t>) =
    args.GetResult (<@ ToTime @>)
    |> DateTime.Parse
    |> fun x -> x.ToUniversalTime()

type QueryOptions =
    { Query: string
      From: DateTime
      To: DateTime
      BaseUrl: string
      Id: string
      Key: string }

    with
        static member Unit () =
            { Query = null
              From = DateTime.UtcNow
              To = DateTime.UtcNow
              BaseUrl = null
              Id = null
              Key = null }

        static member FromArgs (gargs:ParseResults<GlobalArgs>) =
            let (args:ParseResults<QueryArgs>) = gargs.GetResult (<@ Query @>)
            { Query = args.GetResult (<@ QueryArgs.Query @>)
              From = fromTime args
              To = toTime args
              BaseUrl = baseUrl gargs
              Id = accessId gargs
              Key = accessKey gargs }


// type StatusOptions =
//     { JobId: string
//       ELB: string
//       Session: string
//       BaseUrl: string
//       Id: string
//       Key: string }

//     with
//         static member Unit () =
//             { JobId = null
//               ELB = null
//               Session = null
//               BaseUrl = null
//               Id = null
//               Key = null }

//         static member FromArgs (gargs:ParseResults<GlobalArgs>) =
//             let (args:ParseResults<StatusArgs>) = gargs.GetResult (<@ Status @>)
//             { JobId = args.GetResult (<@ StatusArgs.JobId @>)
//               ELB = args.GetResult (<@ StatusArgs.ELBCookie @>)
//               Session = args.GetResult (<@ StatusArgs.SessionCookie @>)
//               BaseUrl = baseUrl gargs
//               Id = accessId gargs
//               Key = accessKey gargs }


type Options =
    | Query of QueryOptions
//    | Status of StatusOptions
    | NoCommand

let ParseArgs argv : Options =
    let parser = ArgumentParser.Create<GlobalArgs>(programName = "fss")
    let results = parser.ParseCommandLine argv
    let command = results.GetSubCommand()
    let now = (DateTime.UtcNow).ToString()

    match command with
        | GlobalArgs.Query x -> Query <| QueryOptions.FromArgs results
//        | GlobalArgs.Status x -> Status <| StatusOptions.FromArgs results
        | _ -> NoCommand

