module AoC2015.Day14

let parseFile (data: array<string>) =
    data
    |> Seq.map (fun line ->
        let words = line.Split(' ')
        int words[3], int words[6], int words[13])
    |> List.ofSeq

let distance (speed: int, duration: int, rest: int) (time: int) =
    let period = duration + rest
    let q, r = time / period, time % period

    // speed * duration * q + speed * min duration rest
    speed * (duration * q + min duration r)

let partOne (reindeers: list<int * int * int>) (time: int) =
    Seq.map (fun r -> distance r time) reindeers |> Seq.max

let distanceSeq (speed: int, duration: int, rest: int) (time: int) =
    seq {
        while true do
            yield! Seq.init duration (fun _ -> speed)
            yield! Seq.init rest (fun _ -> 0)
    }
    |> Seq.take time
    |> Seq.scan (+) 0
    |> Seq.tail

let maxIndexes (distArr: array<int>) =
    let maxValue = Array.max distArr

    seq {
        for n in 0 .. Array.length distArr - 1 do
            if distArr[n] = maxValue then
                n
    }

let partTwo (reindeers: list<int * int * int>) (time: int) =
    let n = Seq.length reindeers
    let matrix = Array2D.create n time 0
    let scores = Array.create n 0

    reindeers
    |> Seq.iteri (fun i r -> distanceSeq r time |> Seq.iteri (fun j distance -> matrix[i, j] <- distance))

    for i = 0 to time - 1 do
        maxIndexes matrix[*, i] |> Seq.iter (fun idx -> scores[idx] <- scores[idx] + 1)

    Array.max scores

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let reindeers = File.ReadAllLines(args[0]) |> parseFile
        partOne reindeers 2503 |> printfn "%d"
        partTwo reindeers 2503 |> printfn "%d"
        0
