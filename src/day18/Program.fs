module AoC2015.Day18

let nRows = 100
let nCols = 100

let parseFile (data: array<string>) =
    assert (data.Length = nRows)
    assert (Seq.map String.length data |> Seq.forall ((=) nCols))
    let grid = Array2D.zeroCreate nRows nCols

    data
    |> Seq.iteri (fun x str -> str |> String.iteri (fun y c -> grid[x, y] <- if c = '#' then 1 else 0))

    grid

let oneCycle (grid: int array2d) =
    let pickup (r: int) (c: int) =
        match (r, c) with
        | r, c when r >= 0 && r < nRows && c >= 0 && c < nCols -> grid[r, c]
        | _ -> 0

    let nNbrs (r: int) (c: int) =
        seq {
            for dr in { -1 .. 1 } do
                for dc in { -1 .. 1 } do
                    yield pickup (r + dr) (c + dc)
        }
        |> Seq.sum
        |> (+) -(pickup r c)

    let updateInfo =
        seq {
            for r = 0 to nRows - 1 do
                for c = 0 to nCols - 1 do
                    let nextLight =
                        if grid[r, c] = 1 then
                            match nNbrs r c with
                            | 2
                            | 3 -> 1
                            | _ -> 0
                        else
                            match nNbrs r c with
                            | 3 -> 1
                            | _ -> 0

                    yield (r, c, nextLight)
        }
        |> List.ofSeq // finalize the update information

    List.iter (fun (r, c, v) -> grid[r, c] <- v) updateInfo

    grid

let countupGrid (grid: int array2d) =
    let mutable acc = 0

    for r = 0 to nRows - 1 do
        for c = 0 to nCols - 1 do
            acc <- acc + grid[r, c]

    acc

let partOne (grid: int array2d) (nSteps: int) =
    { 1..nSteps } |> Seq.fold (fun g _ -> oneCycle g) grid |> countupGrid

let partTwo (grid: int array2d) (nSteps: int) =
    let corners (grid: int array2d) =
        grid[0, 0] <- 1
        grid[0, nCols - 1] <- 1
        grid[nRows - 1, 0] <- 1
        grid[nRows - 1, nCols - 1] <- 1
        grid

    { 1..nSteps }
    |> Seq.fold (fun g _ -> oneCycle g |> corners) (corners grid)
    |> countupGrid

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let grid = File.ReadAllLines(args[0]) |> parseFile
        partOne (Array2D.copy grid) 100 |> printfn "%d"
        partTwo grid 100 |> printfn "%d"
        0
