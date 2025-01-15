module AoC2015.Day16

let parseFile (data: array<string>) =
    let parseLine (line: string) =
        let words =
            line.Split([| ' '; ','; ':' |], System.StringSplitOptions.RemoveEmptyEntries)

        {| Id = int words[1]
           Props =
            List.ofArray words
            |> List.skip 2
            |> Seq.chunkBySize 2
            |> Seq.map (fun arr -> (arr[0], int arr[1])) |}

    Seq.map parseLine data |> List.ofSeq

let partOne (clue: Map<string, int>) (allAunts: list<{| Id: int; Props: seq<string * int> |}>) =
    allAunts
    |> Seq.find (fun aunt ->
        aunt.Props
        |> Seq.forall (fun (k, v) ->
            match clue.TryFind k with
            | Some x -> x = v
            | None -> false))
    |> _.Id

let partTwo (clue: Map<string, int>) (allAunts: list<{| Id: int; Props: seq<string * int> |}>) =
    allAunts
    |> Seq.find (fun aunt ->
        aunt.Props
        |> Seq.forall (fun (k, v) ->
            match clue.TryFind k with
            | Some x when k = "cats" || k = "trees" -> v > x
            | Some x when k = "pomeranians" || k = "goldfish" -> v < x
            | Some x -> x = v
            | None -> false))
    |> _.Id

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let allAunts = File.ReadAllLines(args[0]) |> parseFile

        let clue =
            Map
                [ ("children", 3)
                  ("cats", 7)
                  ("samoyeds", 2)
                  ("pomeranians", 3)
                  ("akitas", 0)
                  ("vizslas", 0)
                  ("goldfish", 5)
                  ("trees", 3)
                  ("cars", 2)
                  ("perfumes", 1) ]

        partOne clue allAunts |> printfn "%d"
        partTwo clue allAunts |> printfn "%d"
        0
