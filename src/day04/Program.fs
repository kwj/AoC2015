module AoC2015.Day04

open System.Security.Cryptography
open System.Text

let partOne (secret: string) =
    let md5 = MD5.Create()
    Seq.initInfinite string
    |> Seq.find (fun s ->
        let hash = md5.ComputeHash(Encoding.ASCII.GetBytes(secret + s))
        hash[0] = 0uy && hash[1] = 0uy && hash[2] < 0x10uy)

let partTwo (secret: string) =
    let md5 = MD5.Create()
    Seq.initInfinite string
    |> Seq.find (fun s ->
        let hash = md5.ComputeHash(Encoding.ASCII.GetBytes(secret + s))
        hash[0] = 0uy && hash[1] = 0uy && hash[2] = 0uy)

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let secret = File.ReadAllText(args[0]).TrimEnd()
        partOne secret |> printfn "%s"
        partTwo secret |> printfn "%s"
        0
