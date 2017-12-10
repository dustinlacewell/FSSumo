[<AutoOpen>]
module Defaults

open System

let ACCESSID = env "SUMO_ACCESS_ID"
let ACCESSKEY = env "SUMO_ACCESS_KEY"
let BASEURL = pick (env "SUMO_URL") "https://api.us2.sumologic.com/api/v1"
