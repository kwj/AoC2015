module AoC2015.Day24

let parseFile (data: array<string>) =
    // Warning: `packages` must be in descending order
    let packages = Seq.map int64 data |> Seq.sortDescending |> List.ofSeq

    packages, List.sum packages

let findSmallestGroups (packages: list<int64>) (weight: int64) =
    let mutable minGrpSize = FSharp.Core.int.MaxValue

    let rec loop (acc: list<int64>) (accLen: int) (capacity: int64) (unused: list<int64>) (rest: list<int64>) =
        seq {
            if capacity = 0 then
                minGrpSize <- min minGrpSize accLen
                yield (acc, List.rev unused @ rest)
            else if accLen < minGrpSize then
                match rest with
                | hd :: tl ->
                    if
                        (minGrpSize = FSharp.Core.int.MaxValue
                        || hd * int64 (minGrpSize - accLen) >= capacity)
                    then
                        if hd <= capacity then
                            yield! loop (hd :: acc) (accLen + 1) (capacity - hd) unused tl

                        yield! loop acc accLen capacity (hd :: unused) tl
                | _ -> ()
        }

    // Convert the search results from Seq to List and determine the value of minGrpSize for filtering
    let result = loop List.empty 0 weight List.empty (List.sortDescending packages) |> List.ofSeq
    result |> List.filter (fun (lst, _) -> List.length lst = minGrpSize)

let separatePackages (packages: list<int64>) (groupWeight: int64) (nGroups: int) =
    let rec loop
        (acc: list<int64>)
        (capacity: int64)
        (unused: list<int64>)
        (rest: list<int64>)
        (result: list<list<int64>>)
        (depth: int)
        =
        seq {
            if capacity = 0 then
                if depth > 1 then
                    yield! loop List.empty groupWeight List.empty (List.rev unused @ rest) (acc :: result) (depth - 1)
                else
                    yield List.rev (acc :: result)
            else
                match rest with
                | hd :: tl ->
                    if hd <= capacity then
                        yield! loop (hd :: acc) (capacity - hd) unused tl result depth

                    yield! loop acc capacity (hd :: unused) tl result depth
                | _ -> ()
        }

    loop List.empty groupWeight List.empty (List.sortDescending packages) List.empty nGroups

let findPackagesOfGroupOne (packages: list<int64>) (groupWeight: int64) (nGroups: int) =
    // Search for all candidate groups for Group 1, which have the fewest number of packages
    findSmallestGroups packages groupWeight

    // Sort groups in order of quantum entanglement
    |> Seq.map (fun (grp1, restPackages) -> List.reduce ( * ) grp1, grp1, restPackages)
    |> Seq.sortBy (fun (qe, _, _) -> qe)

    // Find the first package group which remaining packages can be organized into each remaining group
    |> Seq.find (fun (_, _, restPackages) ->
        separatePackages restPackages groupWeight (nGroups - 1) |> Seq.isEmpty |> not)

let partOne (packages: list<int64>) (totalWeight: int64) =
    assert (totalWeight % 3L = 0)

    findPackagesOfGroupOne packages (totalWeight / 3L) 3 |> (fun (qe, _, _) -> qe)

let partTwo (packages: list<int64>) (totalWeight: int64) =
    assert (totalWeight % 4L = 0)

    findPackagesOfGroupOne packages (totalWeight / 4L) 4 |> (fun (qe, _, _) -> qe)

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let packages, totalWeight = File.ReadAllLines(args[0]) |> parseFile
        partOne packages totalWeight |> printfn "%d"
        partTwo packages totalWeight |> printfn "%d"
        0
