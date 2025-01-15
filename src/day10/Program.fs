module AoC2015.Day10

let parseFile (data: string) =
    data.TrimEnd() |> Seq.map (fun x -> int x - int '0') |> List.ofSeq

let digits n =
    let rec loop lst =
        function
        | 0 -> List.rev lst
        | x -> loop ((x % 10) :: lst) (x / 10)

    loop List.empty<int> n

let oneCycle (lst: list<int>) =
    seq {
        let mutable prev = List.head lst
        let mutable cnt = 1

        for x in List.tail lst do
            if x <> prev then
                if cnt > 9 then
                    for x in List.rev (digits cnt) do
                        yield x
                else
                    yield cnt

                yield prev

                cnt <- 1
                prev <- x
            else
                cnt <- cnt + 1

        if cnt > 9 then
            for x in List.rev (digits cnt) do
                yield x
        else
            yield cnt

        yield prev
    }
    |> List.ofSeq

let doCycles (seed: list<int>) (cnt: int) =
    let lst = seq { 1..cnt } |> Seq.fold (fun lst _ -> oneCycle lst) seed
    List.length lst, lst

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let input = File.ReadAllText(args[0]) |> parseFile
        let p1Ans, p1Lst = doCycles input 40
        p1Ans |> printfn "%d"
        let p2Ans, _ = doCycles p1Lst 10 // 40 + 10 = 50
        p2Ans |> printfn "%d"
        0
