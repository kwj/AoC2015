module AoC2015.Day22

open System.Collections.Generic

// M: mana, S: shield, P: poison, R: recharge
type Status =
    { BossHP: int
      PlayerHP: int
      M: int
      S: int
      P: int
      R: int }

    member x.IsBossDead = x.BossHP <= 0
    member x.IsPlayerDead = x.PlayerHP <= 0

let parseFile (data: array<string>) =
    let mutable hp = 0
    let mutable damage = 0

    data
    |> Array.map _.Split(": ")
    |> Array.iter (fun tokens ->
        match tokens[0] with
        | "Hit Points" -> hp <- int tokens[1]
        | "Damage" -> damage <- int tokens[1]
        | _ -> failwith "invalid format")

    damage,
    { BossHP = hp
      PlayerHP = 50
      M = 500
      S = 0
      P = 0
      R = 0 }

let spellMissile (st: Status) =
    let cost = 53

    if st.M < cost then
        None
    else
        Some(
            cost,
            { st with
                BossHP = st.BossHP - 4
                M = st.M - cost }
        )

let spellDrain (st: Status) =
    let cost = 73

    if st.M < cost then
        None
    else
        Some(
            cost,
            { st with
                BossHP = st.BossHP - 2
                PlayerHP = st.PlayerHP + 2
                M = st.M - cost }
        )

let spellShield (st: Status) =
    let cost = 113

    if st.M < cost || st.S > 0 then
        None
    else
        Some(cost, { st with M = st.M - cost; S = 6 })

let spellPoison (st: Status) =
    let cost = 173

    if st.M < cost || st.P > 0 then
        None
    else
        Some(cost, { st with M = st.M - cost; P = 6 })

let spellRecharge (st: Status) =
    let cost = 229

    if st.M < cost || st.R > 0 then
        None
    else
        Some(cost, { st with M = st.M - cost; R = 5 })

let evalEffect (st: Status) =
    { st with
        BossHP = if st.P > 0 then st.BossHP - 3 else st.BossHP
        M = if st.R > 0 then st.M + 101 else st.M
        S = max 0 (st.S - 1)
        P = max 0 (st.P - 1)
        R = max 0 (st.R - 1) }

let makeBossAction (damage: int) =
    let bossAction (crntCnsmp: int, beforeSt: Status) =
        let st = evalEffect beforeSt

        if st.IsBossDead then
            Some(crntCnsmp, st)
        else
            let playerHP = st.PlayerHP - max 1 (damage - if st.S > 0 then 7 else 0)

            if playerHP > 0 then
                Some(crntCnsmp, { st with PlayerHP = playerHP })
            else
                None

    bossAction

let playerAction (preSt: Status) (crntCnsmp: int) =
    if preSt.IsPlayerDead then
        []
    else
        let st = evalEffect preSt

        if st.IsBossDead then
            [ (crntCnsmp, st) ]
        else
            [ spellMissile; spellDrain; spellShield; spellPoison; spellRecharge ]
            |> List.map (fun fn -> fn st)
            |> List.filter _.IsSome
            |> List.map _.Value
            |> List.map (fun (cost, newSt) -> (crntCnsmp + cost, newSt))

let oncCycle (st: Status) (crntCnsmp: int) (bossAction: int * Status -> option<int * Status>) =
    let endLst1, contLst1 =
        playerAction st crntCnsmp |> List.partition (fun tpl -> (snd tpl).IsBossDead)

    let endLst2, contLst2 =
        List.map bossAction contLst1
        |> List.filter _.IsSome
        |> List.map _.Value
        |> List.partition (fun tpl -> (snd tpl).IsBossDead)

    let endLst = endLst1 @ endLst2

    (contLst2,
     if List.isEmpty endLst then
         FSharp.Core.int.MaxValue
     else
         (List.map fst endLst |> List.min))

let playGame (st: Status) (bossAction: int * Status -> option<int * Status>) (hardMode: bool) =
    let seen = HashSet<Status>() // for pruning
    let pq = PriorityQueue<(int * Status), int>()

    pq.Enqueue((0, st), 0)

    let rec loop (minCnsmp: int) =
        if pq.Count = 0 then
            minCnsmp
        else
            match pq.Dequeue() with
            | cnsmp, _ when cnsmp > minCnsmp -> minCnsmp
            | _, st when seen.Contains(st) ->
                // If this status has already been registered, there is no need to check further from here.
                // Because there is a precedent that have been reached this status using less mana.
                // Note: This pruning reduces the computation volume by a factor of about 100.
                loop minCnsmp
            | cnsmp, st ->
                // The minimum mana consumption required to reach the status `st` is finalized here.
                seen.Add(st) |> ignore

                let contStatus, endCnsmp =
                    oncCycle
                        (if hardMode then
                             { st with PlayerHP = st.PlayerHP - 1 }
                         else
                             st)
                        cnsmp
                        bossAction

                contStatus |> List.iter (fun (v, x) -> pq.Enqueue((v, x), v))
                loop (min endCnsmp minCnsmp)

    loop FSharp.Core.int.MaxValue

let partOne (st: Status) (bossAction: int * Status -> option<int * Status>) = playGame st bossAction false

let partTwo (st: Status) (bossAction: int * Status -> option<int * Status>) = playGame st bossAction true

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let damage, initStatus = File.ReadAllLines(args[0]) |> parseFile
        let bossAction = makeBossAction damage
        partOne initStatus bossAction |> printfn "%d"
        partTwo initStatus bossAction |> printfn "%d"
        0
