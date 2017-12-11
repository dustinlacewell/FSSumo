[<AutoOpen>]
module Args

open System
open Argu

type private ArgDefs =
    | AccessId of string
    | AccessKey of string
    | BaseUrl of string
    | FromTime of string
    | ToTime of string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | AccessId _ -> "sumologic access ID."
            | AccessKey _ -> "sumologic access key."
            | BaseUrl _ -> "sumologic jobs api url"
            | FromTime _ -> "query start time"
            | ToTime _ -> "query end time"

type Args(programName, argv) =
    let _argv = argv
    let _parser = ArgumentParser.Create<ArgDefs>(programName = programName)
    let _results = _parser.ParseCommandLine argv

    member this.BaseUrl
      with get() = _results.GetResult (<@ BaseUrl @>, defaultValue = BASEURL)

    member this.AccessId
      with get() =
        let id = _results.GetResult (<@ AccessId @>, defaultValue = ACCESSID)
        if isNull id then
            failwith "Access ID is required."
        else
            id

    member this.AccessKey
      with get() =
        let key = _results.GetResult (<@ AccessKey @>, defaultValue = ACCESSKEY)
        if isNull key then
            failwith "Access Key is required."
        else
            key

    member this.ToTime
      with get() =
          let now = DateTime.UtcNow - TimeSpan(1, 0, 0, 0)

          _results.GetResult (<@ ToTime @>, defaultValue = now.ToString("o"))
          |> DateTime.Parse
          |> fun x -> x.ToUniversalTime()

    member this.FromTime
      with get() =
          let before = this.ToTime - TimeSpan(1, 0, 0)
          _results.GetResult (<@ FromTime @>, defaultValue = before.ToString("o"))
          |> DateTime.Parse
          |> fun x -> x.ToUniversalTime()
