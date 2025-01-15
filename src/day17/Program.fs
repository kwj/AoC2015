module AoC2015.Day17

let parseFile (data: array<string>) = Seq.map int data

// Return the target indexes in ascending order.
let selectIndexes (tbl: (int * list<int>) array) (capacity: int) =
    Seq.mapi (fun idx (_, lst) -> List.length lst, idx) tbl
    |> Seq.filter (fun (len, idx) -> len > 0 && idx + capacity < tbl.Length)
    |> Seq.map (snd >> fun x -> (x, x + capacity))
    |> List.ofSeq

// [Importatnt]
// Update the table in order from the largest index to the smallest.
let updateTbl (tbl: (int * list<int>) array) (indexPairs: list<int * int>) =
    indexPairs
    |> List.rev
    |> List.iter (fun (src, dest) ->
        let (srcNCombs, srcNContsLst) = tbl[src]
        let (destNCombs, destNContsLst) = tbl[dest]
        tbl[dest] <- (srcNCombs + destNCombs, List.append destNContsLst (List.map ((+) 1) srcNContsLst)))

let runDP (totalCapacity: int) (containers: seq<int>) =
    // (number of combinations, list of number of containers)
    let dpTbl = Array.create (totalCapacity + 1) (0, List.empty<int>)
    dpTbl[0] <- (1, [ 0 ])

    containers |> Seq.iter (updateTbl dpTbl << selectIndexes dpTbl)

    Array.last dpTbl

let partOne (result: int * list<int>) = fst result

let partTwo (result: int * list<int>) =
    snd result |> List.sort |> Util.group |> List.head |> List.length

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let containers = File.ReadAllLines(args[0]) |> parseFile
        let result = runDP 150 containers
        partOne result |> printfn "%d"
        partTwo result |> printfn "%d"
        0
