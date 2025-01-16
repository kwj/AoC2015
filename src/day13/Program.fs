module AoC2015.Day13

let parseFile (data: array<string>) =
    let parseLine (str: string) =
        let lst = str.Split([| ' '; '.' |], System.StringSplitOptions.RemoveEmptyEntries)

        match lst[2] with
        | "gain" -> (lst[0], lst[10], float lst[3])
        | _ -> (lst[0], lst[10], -(float lst[3]))

    let happiness, persons =
        data
        |> Seq.fold
            (fun (lst, set) line ->
                let (p1, p2, v) as tpl = parseLine line
                (tpl :: lst, set |> Set.add p1 |> Set.add p2))
            (List.empty<string * string * float>, Set.empty<string>)

    let nPersons = persons.Count + 1 // Increase by 1 because make myself as the last invited guest.
    let personIdMap = persons |> Set.toSeq |> Seq.mapi (fun i x -> (x, i)) |> Map
    let adjMatrix = Array2D.create nPersons nPersons 0.0

    happiness
    |> List.iter (fun (p1, p2, v) ->
        let x, y = personIdMap[p1], personIdMap[p2]
        adjMatrix[x, y] <- adjMatrix[x, y] + v
        adjMatrix[y, x] <- adjMatrix[y, x] + v)

    nPersons, adjMatrix

let classifySbs (n: int) (bitSet: int) =
    seq { 0 .. n - 1 }
    |> Seq.fold
        (fun (visited: list<int>, unVisited: list<int>) x ->
            match bitSet &&& (1 <<< x) with
            | 0 -> (visited, x :: unVisited)
            | _ -> (x :: visited, unVisited))
        (List.empty, List.empty)

// This problem is a maximum traveling salesman problem.
let runDP (n: int) (adjMatrix: float array2d) =
    let dpTbl = Array2D.create (1 <<< n) n -infinity
    dpTbl[1, 0] <- 0

    for bitSet = 1 to (1 <<< n) - 1 do
        let visited, unVisited = classifySbs n bitSet

        unVisited
        |> List.iter (fun u ->
            let tgtRow = bitSet ||| (1 <<< u)

            dpTbl[tgtRow, u] <-
                visited
                |> List.fold (fun acc v -> max acc (dpTbl[bitSet, v] + adjMatrix[v, u])) dpTbl[tgtRow, u])

    dpTbl

let partOne (n: int) (adjMatrix: float array2d) (dpTbl: float array2d) =
    // Decrease `n` by 1 to exclude myself.
    let result = dpTbl[(1 <<< (n - 1)) - 1, *]

    // ditto
    seq { 0 .. (n - 1) - 1 }
    |> Seq.map (fun x -> result[x] + adjMatrix[x, 0])
    |> Seq.max
    |> int

let partTwo (n: int) (adjMatrix: float array2d) (dpTbl: float array2d) =
    let result = dpTbl[(1 <<< n) - 1, *]

    seq { 0 .. n - 1 }
    |> Seq.map (fun x -> result[x] + adjMatrix[x, 0])
    |> Seq.max
    |> int

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let nPersons, adjMatrix = File.ReadAllLines(args[0]) |> parseFile
        let dpTbl = runDP nPersons adjMatrix
        partOne nPersons adjMatrix dpTbl |> printfn "%d"
        partTwo nPersons adjMatrix dpTbl |> printfn "%d"
        0
