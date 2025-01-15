module AoC2015.Day06

let parseFile (data: array<string>) =
    data
    |> Seq.map (fun line ->
        let words = line.Split([| ' '; ',' |], System.StringSplitOptions.RemoveEmptyEntries)

        if words[0] = "toggle" then
            (words[0], (int words[1], int words[2]), (int words[4], int words[5]))
        else
            (words[1], (int words[2], int words[3]), (int words[5], int words[6])))

let sumArray2D (grid: int array2d) =
    let mutable acc = 0

    for x = 0 to Array2D.length1 grid - 1 do
        for y = 0 to Array2D.length2 grid - 1 do
            acc <- acc + grid[x, y]

    acc

let implement
    (turnOn: int -> int)
    (turnOff: int -> int)
    (toggle: int -> int)
    (input: seq<string * (int * int) * (int * int)>)
    =
    let grid = Array2D.create 1000 1000 0

    let updateGrid (x1: int, y1: int) (x2: int, y2: int) (op: int -> int) =
        for x = x1 to x2 do
            for y = y1 to y2 do
                grid[x, y] <- op grid[x, y]

    input
    |> Seq.iter (fun (op, start, stop) ->
        match op with
        | "on" -> updateGrid start stop turnOn
        | "off" -> updateGrid start stop turnOff
        | "toggle" -> updateGrid start stop toggle
        | _ -> failwith "invalid op")

    grid

let partOne (input: seq<string * (int * int) * (int * int)>) =
    let turnOn (_: int) = 1
    let turnOff (_: int) = 0
    let toggle (v: int) = if v = 0 then 1 else 0

    input |> implement turnOn turnOff toggle |> sumArray2D

let partTwo (input: seq<string * (int * int) * (int * int)>) =
    let turnOn (v: int) = v + 1
    let turnOff (v: int) = if v <= 0 then 0 else v - 1
    let toggle (v: int) = v + 2

    input |> implement turnOn turnOff toggle |> sumArray2D

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let input = File.ReadAllLines(args[0]) |> parseFile
        partOne input |> printfn "%d"
        partTwo input |> printfn "%d"
        0
