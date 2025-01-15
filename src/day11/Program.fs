module AoC2015.Day11

let parseFile (data: string) =
    let charI = int 'i' - int 'a'
    let charL = int 'l' - int 'a'
    let charO = int 'o' - int 'a'

    let rawPasswd =
        data.TrimEnd() |> Seq.map (fun x -> int x - int 'a') |> Seq.rev |> List.ofSeq

    // rule 2
    try
        let idx =
            List.findIndexBack (fun x -> x = charI || x = charL || x = charO) rawPasswd

        let arr = Array.ofList rawPasswd
        arr[idx] <- arr[idx] + 1

        for i in seq { 0 .. idx - 1 } do
            arr[i] <- 0

        List.ofArray arr
    with :? System.Collections.Generic.KeyNotFoundException ->
        rawPasswd

let nextCandidate (passwd: list<int>) =
    let charH = int 'h' - int 'a' // the letter before `i`
    let charK = int 'k' - int 'a' // the letter before `l`
    let charN = int 'n' - int 'a' // the letter before `o`
    let charZ = int 'z' - int 'a'

    let addCarry (x: int) (carry: int) =
        // rule 2
        if carry = 0 then
            (x, 0)
        else
            let delta n = if n = charH || n = charK || n = charN then 2 else 1
            let nextChar = x + delta x

            if nextChar > charZ then
                ((nextChar % (charZ + 1)), 1)  // actually, (0, 1) is fine
            else
                (nextChar, 0)

    let rec aux (lst: list<int>) (carry: int) (result: list<int>) =
        match lst with
        | [] -> List.rev result
        | c :: cs when carry = 1 ->
            let nextChar, nextCarry = addCarry c carry
            aux cs nextCarry (nextChar :: result)
        | c :: cs -> List.rev result @ lst

    aux passwd 1 List.empty

let rec rule1 (lst: list<int>) =
    match lst with
    | x1 :: (x2 :: x3 :: _ as tl) when x1 = x2 + 1 && x2 = x3 + 1 -> true
    | _ :: (_ :: _ :: _ as tl) -> rule1 tl
    | _ -> false

let rule3 (lst: list<int>) =
    // The problem statements says "at least two different, non-overlapping pairs of letters".
    // So, I understood that "aaa" didn't meet the condition and "aaaa" met it.

    // Seq.windowed is difficult to use here because it creates a sequence of arrays.
    Seq.pairwise lst
    |> Seq.filter (fun (x, y) -> x = y)
    |> List.ofSeq
    |> Util.group
    |> List.sumBy (fun x -> (List.length x + 1) / 2)
    |> (<) 1

let passwdStr (passwd: list<int>) =
    List.rev passwd
    |> List.map (fun x -> x + int 'a' |> char)
    |> Array.ofList
    |> System.String

let nextPasswd (input: list<int>) =
    let rec aux (passwd: list<int>) =
        if rule1 passwd && rule3 passwd then
            passwd
        else
            aux (nextCandidate passwd)

    aux (nextCandidate input)

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let input = File.ReadAllText(args[0]) |> parseFile
        let p1ans = nextPasswd input
        passwdStr p1ans |> printfn "%s"
        let p2ans = nextPasswd p1ans
        passwdStr p2ans |> printfn "%s"
        0
