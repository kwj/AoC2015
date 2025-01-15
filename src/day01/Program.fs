module AoC2015.Day01

let parseFile (data: string) =
    data.TrimEnd()
    |> Seq.scan
        (fun acc ->
            function
            | '(' -> acc + 1
            | ')' -> acc - 1
            | ch -> failwithf "invalid instruction: %c" ch)
        0

let partOne (input: seq<int>) = Seq.last input

// Note: The first instruction is the 2nd element of the sequence (0-based)
let partTwo (input: seq<int>) = Seq.findIndex ((>) 0) input

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let input = File.ReadAllText(args[0]) |> parseFile
        partOne input |> printfn "%d"
        partTwo input |> printfn "%d"
        0
