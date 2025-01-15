module AoC2015.Day08

let partOne (data: array<string>) =
    let decodedLength (str: string) =
        let charLst = str.ToCharArray() |> List.ofArray

        let rec countUp (lst: list<char>) (cnt: int) =
            match lst with
            | [ '"' ] -> cnt
            | '\\' :: '"' :: cs -> countUp cs (cnt + 1)
            | '\\' :: '\\' :: cs -> countUp cs (cnt + 1)
            | '\\' :: 'x' :: _ :: _ :: cs -> countUp cs (cnt + 1)
            | _ :: cs -> countUp cs (cnt + 1)
            | _ -> failwith "not reached"

        // Drop the first character of the string, '"'.
        countUp (List.tail charLst) 0

    // If I write `Array.sumBy _.Length data`, F# 9.0 can't infer the type of
    // the array elements. I don't know the reason why.
    (data |> Array.sumBy _.Length) - (data |> Array.sumBy decodedLength)

let partTwo (data: array<string>) =
    let encodedLength (str: string) =
        let encodedSize =
            function
            | '\\' -> 2
            | '"' -> 2
            | _ -> 1

        // Add 2 for new double quotation marks around the encoded string.
        (str.ToCharArray() |> Array.sumBy encodedSize) + 2

    (data |> Array.sumBy encodedLength) - (data |> Array.sumBy _.Length)

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let input = File.ReadAllLines(args[0])
        partOne input |> printfn "%d"
        partTwo input |> printfn "%d"
        0
