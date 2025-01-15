module AoC2015.Day03

let parseFile (data: string) =
    data.TrimEnd().ToCharArray() |> List.ofArray

let allLocations (instr: list<char>) =
    instr
    |> List.scan
        (fun (x, y) ->
            function
            | '^' -> (x, y + 1)
            | '>' -> (x + 1, y)
            | 'v' -> (x, y - 1)
            | '<' -> (x - 1, y)
            | _ -> failwith "invalid direction")
        (0, 0)

let partOne (input: list<char>) =
    allLocations input |> Set.ofList |> Set.count

let partTwo (input: list<char>) =
    let (santa, robo) = Util.fork input

    (allLocations santa |> Set.ofList, allLocations robo |> Set.ofList)
    ||> Set.union
    |> Set.count

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let input = File.ReadAllText(args[0]) |> parseFile
        partOne input |> printfn "%d"
        partTwo input |> printfn "%d"
        0
