namespace AoC2015

[<RequireQualifiedAccess>]
module Util =
    // Note: To avoid stack overflow, it should be a tail recursive function.
    let group<'T when 'T: equality> (lst: list<'T>) =
        let rec loop (lst: list<'T>) (q: list<'T>) (result: list<list<'T>>) =
            match lst with
            | x1 :: (x2 :: _ as tl) when x1 = x2 -> loop tl (x1 :: q) result
            | x :: (_ :: _ as tl) -> loop tl List.empty<'T> ((x :: q) :: result)
            | [ x ] -> (x :: q) :: result
            | [] -> []

        loop lst List.empty List.empty |> List.rev

    let rec fork =
        function
        | x :: xs ->
            let (ys, zs) = fork xs
            (x :: zs, ys)
        | [] -> ([], [])
