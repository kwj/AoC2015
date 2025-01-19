module AoC2015.Day25

(*
[python3]
>>> import math
>>> math.log2(33554393 * 33554393)
49.999996646336356
>>> math.log2(33554393 * 20151125)
49.264355371374386

Okay, I'll use uint64.
*)

open System.Text.RegularExpressions

let parseFile (data: string) =
    let m = Regex.Matches(data.TrimEnd(), @"\d+")
    assert (m.Count = 2)
    int m[0].Value, int m[1].Value

// 0-based index
let getLinearIndex (r: int) (c: int) =
    let n = r + c - 2
    n * (n + 1) / 2 + c - 1

let partOne (pos: int * int) =
    let powmod (b: uint64) (e: int) (m: uint64) =
        let rec loop b e result =
            match e with
            | 0 -> result
            | _ when e % 2 <> 0 -> loop (b * b % m) (e >>> 1) (result * b % m)
            | _ -> loop (b * b % m) (e >>> 1) result

        loop b e 1UL

    let init = 20151125UL
    let b = 252533UL
    let e = pos ||> getLinearIndex
    let m = 33554393UL

    init * (powmod b e m) % m

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let input = File.ReadAllText(args[0]) |> parseFile
        partOne input |> printfn "%d"
        0
