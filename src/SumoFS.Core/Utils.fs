[<AutoOpen>]
module Utils

open System

// if not a then b
let pick a b = if isNull a then b else a

let env s :string = Environment.GetEnvironmentVariable s
