module AoC2015.Day02

let parseFile (data: array<string>) =
    data |> Seq.map (fun line -> line.Split('x') |> Array.map int |> List.ofArray)

let partOne (input: seq<list<int>>) =
    input
    |> Seq.sumBy (fun lst ->
        let area1 = lst[0] * lst[1]
        let area2 = lst[1] * lst[2]
        let area3 = lst[2] * lst[0]
        let slack = area1 |> min area2 |> min area3
        (area1 + area2 + area3) * 2 + slack)

let partTwo (input: seq<list<int>>) =
    input
    |> Seq.sumBy (fun lst ->
        let sortedLst = List.sort lst
        let perimeter = (sortedLst[0] + sortedLst[1]) * 2
        let bow = List.reduce ( * ) lst
        perimeter + bow)

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let input = File.ReadAllLines(args[0]) |> parseFile
        partOne input |> printfn "%d"
        partTwo input |> printfn "%d"
        0
