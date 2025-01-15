module AoC2015.Day12

// I used the .NET API to handle JSON data and cut corners.
open System.Text.Json

let parseFile (data: string) = JsonDocument.Parse(data.TrimEnd())

let rec total (e: JsonElement) (isDeficit: JsonElement -> bool) =
    match e.ValueKind with
    | JsonValueKind.Number -> e.GetInt32()
    | JsonValueKind.Array -> e.EnumerateArray() |> Seq.sumBy (fun x -> total x isDeficit)
    | JsonValueKind.Object ->
        if isDeficit e then
            0
        else
            e.EnumerateObject() |> Seq.sumBy (fun prop -> total prop.Value isDeficit)
    | _ -> 0

let partOne (input: JsonDocument) =
    total input.RootElement (fun _ -> false)

let partTwo (input: JsonDocument) =
    // Note: `e` must be a JSON object (not array, value, number, string, etc.)
    let isDeficit (e: JsonElement) =
        assert (e.ValueKind = JsonValueKind.Object)

        e.EnumerateObject()
        |> Seq.exists (fun prop -> prop.Value.ValueKind = JsonValueKind.String && prop.Value.GetString() = "red")

    total input.RootElement isDeficit

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let input = File.ReadAllText(args[0]) |> parseFile
        partOne input |> printfn "%d"
        partTwo input |> printfn "%d"
        0
