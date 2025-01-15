module AoC2015.Day05

let rule1 (str: string) =
    Seq.filter (fun c -> List.contains c [ 'a'; 'e'; 'i'; 'o'; 'u' ]) str
    |> Seq.length
    |> (<=) 3

let rule2 (str: string) =
    Seq.pairwise str |> Seq.exists (fun (x, y) -> x = y)

let rule3 (str: string) =
    Seq.pairwise str
    |> Seq.exists (fun pair -> List.contains pair [ ('a', 'b'); ('c', 'd'); ('p', 'q'); ('x', 'y') ])
    |> not

let rule4 (str: string) =
    let twoCharSubStrings (str: string) =
        str.ToCharArray() |> Seq.ofArray |> Seq.windowed 2 |> Seq.map System.String

    let countSubstr (str: string) (substr: string) =
        let rec loop count (idx: int) =
            if String.length str <= idx then
                count
            else
                match str.IndexOf(substr, idx) with
                | -1 -> count
                | offset -> loop (count + 1) (offset + String.length substr)

        if String.length substr = 0 then 0 else loop 0 0

    twoCharSubStrings str |> Seq.map (countSubstr str) |> Seq.max |> (<) 1

let rule5 (str: string) =
    Seq.zip str (Seq.tail str |> Seq.tail) |> Seq.exists (fun (x, y) -> x = y)

let partOne (input: array<string>) =
    input
    |> Seq.filter (fun str -> rule1 str && rule2 str && rule3 str)
    |> Seq.length

let partTwo (input: array<string>) =
    input |> Seq.filter (fun str -> rule4 str && rule5 str) |> Seq.length

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let input = File.ReadAllLines(args[0])
        partOne input |> printfn "%d"
        partTwo input |> printfn "%d"
        0
