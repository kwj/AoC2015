module AoC2015.Day09

let parseFile (data: array<string>) =
    let parseLine (str: string) =
        match str.Split(' ') with
        | [| x; "to"; y; "="; dist |] -> (x, y, float dist)
        | _ -> failwith "invalid line"

    let distInfo, locs =
        data
        |> Seq.fold
            (fun (lst, set) line ->
                let (p1, p2, _) as tpl = parseLine line
                (tpl :: lst, set |> Set.add p1 |> Set.add p2))
            (List.empty<string * string * float>, Set.empty<string>)

    let nLocs = locs.Count
    let locIdMap = locs |> Set.toSeq |> Seq.mapi (fun i x -> (x, i)) |> Map
    let adjMatrix = Array2D.create nLocs nLocs nan

    distInfo
    |> List.iter (fun (p1, p2, dist) ->
        adjMatrix[locIdMap[p1], locIdMap[p2]] <- dist
        adjMatrix[locIdMap[p2], locIdMap[p1]] <- dist)

    nLocs, adjMatrix

let classifySbs (n: int) (bitSet: int) =
    seq { 0 .. n - 1 }
    |> Seq.fold
        (fun (visited: list<int>, unVisited: list<int>) x ->
            if bitSet &&& (1 <<< x) <> 0 then
                (x :: visited, unVisited)
            else
                (visited, x :: unVisited))
        (List.empty, List.empty)

let runDP (n: int) (adjMatrix: float array2d) (dpTbl: float array2d) (fn: float -> float -> float) =
    // start from all locations, so set initial distance of each location as 0
    seq { 0 .. n - 1 } |> Seq.iter (fun x -> dpTbl[1 <<< x, x] <- 0)

    for bitSet = 1 to (1 <<< n) - 1 do
        let visited, unVisited = classifySbs n bitSet

        unVisited
        |> List.iter (fun u ->
            let tgtRow = bitSet ||| (1 <<< u)

            dpTbl[tgtRow, u] <-
                visited
                |> List.fold (fun acc v -> fn acc (dpTbl[bitSet, v] + adjMatrix[v, u])) dpTbl[tgtRow, u])

    dpTbl[(1 <<< n) - 1, *]

let partOne (n: int) (adjMatrix: float array2d) =
    let dpTbl = Array2D.create (1 <<< n) n infinity

    adjMatrix
    |> Array2D.iteri (fun i j v ->
        if System.Double.IsNaN(v) then
            adjMatrix[i, j] <- infinity)

    runDP n adjMatrix dpTbl min |> Array.min |> int

let partTwo (n: int) (adjMatrix: float array2d) =
    let dpTbl = Array2D.create (1 <<< n) n -infinity

    adjMatrix
    |> Array2D.iteri (fun i j v ->
        if System.Double.IsNaN(v) then
            adjMatrix[i, j] <- -infinity)

    runDP n adjMatrix dpTbl max |> Array.max |> int

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let nLocs, adjMatrix = File.ReadAllLines(args[0]) |> parseFile
        partOne nLocs adjMatrix |> printfn "%d"
        partTwo nLocs adjMatrix |> printfn "%d"
        0
