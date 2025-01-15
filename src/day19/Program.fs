module AoC2015.Day19

open System.Collections.Generic
open System.Text.RegularExpressions

let parseFile (data: array<string>) =
    let sepIdx = Array.findIndex ((=) "") data
    let pairs = data[0 .. sepIdx - 1] |> Seq.map (fun s -> s.Split(" => "))
    let fwdLst = pairs |> Seq.map (fun arr -> arr[0], arr[1]) |> List.ofSeq
    let revLst = pairs |> Seq.map (fun arr -> arr[1], arr[0]) |> List.ofSeq

    fwdLst, revLst, data[sepIdx + 1]

let partOne (fwdLst: list<string * string>) (mol: string) =
    seq {
        for (src, dest) in fwdLst do
            let re = Regex(src, RegexOptions.Compiled)

            for m in re.Matches(mol) do
                yield re.Replace(mol, dest, 1, m.Index)
    }
    |> Set.ofSeq
    |> Set.count

(*
Check the given replacement rules.

There are three representation forms for molecules.

  - A small letter (only `e`)
  - A capital letter (e.g. B, F, etc.)
  - A capital letter with following a small letter (e.g. Ca, Mg, Si, etc.)

If we need to know number of molecules which not including `e`, we should count capital letters.

Molecules C, Rn, Y and Ar have no destinations for replacement.
Also, Rn, Y and Ar are in a certain order. (Rn -> Y* -> Ar).
When Y appears, it is limited to between Rn and Ar.

C is located before Rn, but C is not the only one that comes before Rn.

It looks like that one replacement step converts one molecule into the following either:
  * Two molecules
  * One molecule and a group of molecules which the first molecule is Rn

Molecular groups beginning with Rn is represented as follows:

It looks like that Y is a separator.
  - [.] <Rn> [F|Mg|Al] <Ar>
  - [.] <Rn>  [F|Mg]   <Y>  [F|Mg]  <Ar>
  - [.] <Rn>    F      <Y>    F     <Y>   F  <Ar>

----------------

For simplify, consider the number of reverse replacements required to
back to the numerator `e`.

First, T molecules without Rn molecule groups:

  ######## -> e

It is clear that `T - 1` steps are needed.

Next, consider Rn molecule groups.

  case #1:
    [.] <Rn> [F|Mg|Al] <Ar>  -->  [.]

    This procedure reduces the number of molecules by 3 instead of 1.

  case #2:
    [.] <Rn> [F|Mg] <Y> [F|Mg] <Ar>  -->  [.]

    This procedure reduces the number of molecules by 5 instead of 1.

  case #3:
    [.] <Rn> F <Y> F <Y> F <Ar>  --> [.]

    This procedure reduces the number of molecules by 7 instead of 1.

In short:

    3 - 1 = 2 = number of Rn + number of Ar + 2 * number of Y(0)
    5 - 1 = 4 = number of Rn + number of Ar + 2 * number of Y(1)
    7 - 1 = 6 = number of Rn + number of Ar + 2 * number of Y(2)

Assume that S is the total number of procedures to restore the medicine, T-molecules, to `e`:

  S = (T - 1) - ((number of Rn) + (number of Ar) + 2 * (number of Y))
    = T - 1 - (number of Rn) - (number of Ar) - 2 * (number of Y)

It is clear that replacement procedures were done `S` times to make
the medicine from the molecule `e`.
*)

let partTwo (mol: string) =
    let reCapitalLetter = Regex(@"[A-Z]", RegexOptions.Compiled)
    let reRn = Regex(@"Rn", RegexOptions.Compiled)
    let reAr = Regex(@"Ar", RegexOptions.Compiled)
    let reY = Regex(@"Y", RegexOptions.Compiled)

    reCapitalLetter.Matches(mol).Count
    - 1
    - reRn.Matches(mol).Count
    - reAr.Matches(mol).Count
    - 2 * reY.Matches(mol).Count

(*
Tentative algorithm for restoring to the original molecule `e`.
The following is a priority order of substitution.

1. Two molecules become one of the original molecules
  e.g.
    BaK => K
    DxDx => Dx
    QCi => Q

2. Two molecules which convert to F, Mg or Al
   In my given data, these three molecules occur frequently in Rn molecule group
  e.g.
    AiH => F
    BaDi => Al

3. The conversion source contains a group of molecules starting with Rn
  e.g.
    TRnFAr => D

4. Other two molecules
  e.g.
    BiE => Ha
    UWx => R

If this doesn't work well, how about searching from the back instead of the front?
But there is no basis for this idea.

I feel that a practical option is to use a randomized algorithm (no priority rules).
I know the name of the CYK (Cocke-Youngger-Kasami) algorithm, but not what is in it.
*)
let reorderByPriority (revLst: list<string * string>) =
    let reCLetter = Regex(@"[A-Z]", RegexOptions.Compiled)
    let reSplit = Regex(@"(.)([A-Z])", RegexOptions.Compiled)

    let allTwoMolsLst, pri3Lst =
        revLst |> List.partition (fun (src, _) -> reCLetter.Matches(src).Count = 2)

    let pri1Lst, restTwoMolsLst =
        allTwoMolsLst
        |> List.partition (fun (src, dest) ->
            let arr = reSplit.Replace(src, "$1 $2").Split(' ')
            arr[0] = dest || arr[1] = dest)

    let pri2Lst, pri4Lst =
        restTwoMolsLst
        |> List.partition (fun (_, dest) -> dest = "F" || dest = "Mg" || dest = "Al")

    pri1Lst @ (pri2Lst @ (pri3Lst @ pri4Lst))

let reverseReplacement (revLst: list<string * string>) (mol: string) (answer: int) (verbose: bool) =
    let checkLst =
        reorderByPriority revLst
        |> List.map (fun (src, dest) -> Regex(src, RegexOptions.Compiled), dest, (dest, src))

    let rec restoreMolecule (s: string, cnt: int, trace: list<int * (string * string)>) (limit: int) =
        if s = "e" then
            if cnt = answer then
                Some(cnt, trace)
            else
                printfn "Expected %d steps, but the result was %d steps.\n" answer cnt
                None
        else if limit <= 0 then
            // A more smarter algorithm is needed
            printfn "It could not be restored in %d replication steps." answer
            printfn "My algorithm is inappropriate for this input data."
            None
        else
            try
                let (re, dest, pair) = checkLst |> List.find (fun (re, _, _) -> re.Match(s).Success)
                let m = re.Match(s)
                restoreMolecule (re.Replace(s, dest, 1), cnt + 1, (m.Index, pair) :: trace) (limit - 1)
            with :? KeyNotFoundException ->
                // A more smarter algorithm is needed
                printfn "Oops! Restoring process is wrong."
                printfn "My algorithm is inappropriate for this input data."
                None

    printfn ("----")

    match restoreMolecule (mol, 0, List.empty<int * (string * string)>) answer with
    | Some(n, lst) ->
        printfn "Trying reverse replacement on Part 2: OK (%d steps)" n

        if verbose then
            printfn "\nReplacement process: Step# (Position, (From, To))"
            List.iteri (fun i x -> printfn "#%d  %A" (i + 1) x) lst
    | None -> printfn "Trying reverse replacement on Part 2: Failed"

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let fwdLst, revLst, molecules = File.ReadAllLines(args[0]) |> parseFile
        partOne fwdLst molecules |> printfn "%d"
        let p2ans = partTwo molecules
        printfn "%d" p2ans
        reverseReplacement revLst molecules p2ans true
        0
