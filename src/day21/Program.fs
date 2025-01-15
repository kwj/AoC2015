module AoC2015.Day21

// C: Cost, D: Damage, A: Armor
type Equipment =
    { C: int
      D: int
      A: int }

    static member (+)(L: Equipment, R: Equipment) =
        { C = L.C + R.C
          D = L.D + R.D
          A = L.A + R.A }

let weapons =
    [ { C = 8; D = 4; A = 0 }
      { C = 10; D = 5; A = 0 }
      { C = 25; D = 6; A = 0 }
      { C = 40; D = 7; A = 0 }
      { C = 74; D = 8; A = 0 } ]

let armors =
    [ { C = 13; D = 0; A = 1 }
      { C = 31; D = 0; A = 2 }
      { C = 53; D = 0; A = 3 }
      { C = 75; D = 0; A = 4 }
      { C = 102; D = 0; A = 5 }
      { C = 0; D = 0; A = 0 } ]

let rings =
    let ringData =
        [| { C = 25; D = 1; A = 0 }
           { C = 50; D = 2; A = 0 }
           { C = 100; D = 3; A = 0 }
           { C = 20; D = 0; A = 1 }
           { C = 40; D = 0; A = 2 }
           { C = 80; D = 0; A = 3 }
           { C = 0; D = 0; A = 0 } |]

    seq {
        let len = ringData.Length

        for i = 0 to len - 2 do
            for j = i + 1 to len - 1 do
                yield ringData[i] + ringData[j]

        yield { C = 0; D = 0; A = 0 }
    }
    |> List.ofSeq

let equip (xs: list<Equipment>) (ys: list<Equipment>) =
    xs |> List.collect (fun x -> ys |> List.map (fun y -> x + y))

let makeJudgeFn (bossHP: int) (bossEqpt: Equipment) (playerHP: int) =
    let numberOfAtks (hp: int) (damage: int) =
        if hp % damage = 0 then hp / damage else hp / damage + 1

    // true: player win, false: boss win
    let judge (playerEqpt: Equipment) =
        numberOfAtks playerHP (max (bossEqpt.D - playerEqpt.A) 1)
        >= numberOfAtks bossHP (max (playerEqpt.D - bossEqpt.A) 1)

    judge

let parseFile (data: array<string>) =
    let mutable hp = 0
    let mutable damage = 0
    let mutable armor = 0

    data
    |> Array.map _.Split(": ")
    |> Array.iter (fun tokens ->
        match tokens[0] with
        | "Hit Points" -> hp <- int tokens[1]
        | "Damage" -> damage <- int tokens[1]
        | "Armor" -> armor <- int tokens[1]
        | _ -> failwith "invalid format")

    hp, { C = 0; D = damage; A = armor }

let partOne (allEqpts: list<Equipment>) (judge: Equipment -> bool) =
    allEqpts
    |> List.sortBy _.C
    |> List.find judge
    |> _.C

let partTwo (allEqpts: list<Equipment>) (judge: Equipment -> bool) =
    allEqpts
    |> List.sortByDescending _.C
    |> List.find (not << judge)
    |> _.C

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let bossHP, bossEqpt = File.ReadAllLines(args[0]) |> parseFile
        let playerHP = 100
        let judgeFn = makeJudgeFn bossHP bossEqpt playerHP
        let allEqpts = weapons |> equip armors |> equip rings
        partOne allEqpts judgeFn |> printfn "%d"
        partTwo allEqpts judgeFn |> printfn "%d"
        0
