module AoC2015.Day04

open System.Security.Cryptography
open System.Text

let makeHashSeq (secret: string) =
    let md5 = MD5.Create()

    Seq.initInfinite string
    |> Seq.map (fun s -> md5.ComputeHash(Encoding.ASCII.GetBytes(secret + s)))

let partOne (secret: string) =
    makeHashSeq secret
    |> Seq.findIndex (fun hash -> hash[0] = 0uy && hash[1] = 0uy && (hash[2] &&& 0xF0uy = 0uy))

let partTwo (secret: string) =
    makeHashSeq secret
    |> Seq.findIndex (fun hash -> hash[0] = 0uy && hash[1] = 0uy && hash[2] = 0uy)

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let secret = File.ReadAllText(args[0]).TrimEnd()
        partOne secret |> printfn "%d"
        partTwo secret |> printfn "%d"
        0
