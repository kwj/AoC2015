module AoC2015.Day20

open System
open System.Collections.Generic

let blockSize = 100_000

let parseFile (data: string) = data.TrimEnd() |> int

(*
https://en.wikipedia.org/wiki/Divisor_function#Growth_rate

sigma(n) < e ** gamma * n * log(log(n)) + 0.6483n / log(log(n))  where n >= 3
*)
let findStartPos (thr: int) (factor: int) =
    let upper (n: int) =
        let e = 2.7182818284 // Euler's number
        let gamma = 0.5772156649 // Euler's constant
        let x = float n
        let tmp = Math.Log(Math.Log(x))

        Math.Round(e ** gamma * x * tmp + 0.6483 * x / tmp, MidpointRounding.AwayFromZero)
        |> int

    Seq.unfold (fun state -> Some(state, state + blockSize)) 0
    |> Seq.map (fun x -> (x, upper (x + blockSize) * factor))
    |> Seq.find (fun tpl -> snd tpl >= thr)
    |> fst

let nextMultiple (n: int) (mul: int) =
    if n % mul = 0 then n + mul else n + (mul - (n % mul))

let makeBlock (pos: int) (factor: int) (updateFn: array<int> -> array<int>) =
    // block: blkStart + 1 .. blkStart + blockSize
    let block = Array.zeroCreate blockSize
    let blkStart = pos + 1
    let blkEnd = pos + blockSize

    if pos > 0 then
        // Elves (#blkStart .. #blkEnd)
        for i = 0 to blockSize - 1 do
            block[i] <- (blkStart + i) * factor

        // Elves (#((blkEnd / 2) + 1) .. #(blkStart - 1))
        // They can't visit this block, so ignore them

        // Elves (#blockSize .. #(blkEnd / 2))
        // They can visit this block at most once.
        for i = blockSize to blkEnd / 2 do
            let amount = i * factor
            let idx = nextMultiple pos i - blkStart

            if idx < blockSize then
                block[idx] <- block[idx] + amount

    updateFn block

let checkBlockP1 (thr: int) (pos: int) =
    let blkStart = pos + 1

    let updateFn (block: array<int>) =
        // Elves (#1 .. #blockSize - 1)
        for i = 1 to blockSize - 1 do
            let amount = i * 10
            let mutable idx = nextMultiple pos i - blkStart

            while idx < blockSize do
                block[idx] <- block[idx] + amount
                idx <- idx + i

        block

    try
        let idx = makeBlock pos 10 updateFn |> Array.findIndex ((<=) thr)
        Some(blkStart + idx)
    with :? KeyNotFoundException ->
        None

let checkBlockP2 (thr: int) (pos: int) =
    let blkStart = pos + 1

    let updateFn (block: array<int>) =
        // Elves (#1 .. #blockSize - 1)
        for i = 1 to blockSize - 1 do
            let amount = i * 11
            let mutable idx = nextMultiple pos i - blkStart
            let mutable cnt = (i * 50) - nextMultiple pos i + 1

            while cnt > 0 && idx < blockSize do
                block[idx] <- block[idx] + amount
                idx <- idx + i
                cnt <- cnt - 1

        block

    try
        let idx = makeBlock pos 11 updateFn |> Array.findIndex ((<=) thr)
        Some(blkStart + idx)
    with :? KeyNotFoundException ->
        None

let findAnswer (start: int) (thr: int) (checkFn: int -> int -> option<int>) =
    Seq.unfold (fun state -> Some(state, state + blockSize)) start
    |> Seq.map (checkFn thr)
    |> Seq.find _.IsSome
    |> _.Value

let partOne (thr: int) =
    findAnswer (findStartPos thr 10) thr checkBlockP1

let partTwo (thr: int) =
    findAnswer (findStartPos thr 11) thr checkBlockP2

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let input = File.ReadAllText(args[0]) |> parseFile
        partOne input |> printfn "%d"
        partTwo input |> printfn "%d"
        0
