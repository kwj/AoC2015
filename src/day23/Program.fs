module AoC2015.Day23

type Registers = { A: int; B: int }

let opHlf (tgt: string) =
    fun (reg: Registers) ->
        match tgt with
        | "a" -> (1, { reg with A = reg.A >>> 1 })
        | "b" -> (1, { reg with B = reg.B >>> 1 })
        | x -> failwith $"invalid register: hlf {x}"

let opTpl (tgt: string) =
    fun (reg: Registers) ->
        match tgt with
        | "a" -> (1, { reg with A = reg.A * 3 })
        | "b" -> (1, { reg with B = reg.B * 3 })
        | x -> failwith $"invalid register: tpl {x}"

let opInc (tgt: string) =
    fun (reg: Registers) ->
        match tgt with
        | "a" -> (1, { reg with A = reg.A + 1 })
        | "b" -> (1, { reg with B = reg.B + 1 })
        | x -> failwith $"invalid register: inc {x}"

let opJmp (offset: int) = fun (reg: Registers) -> (offset, reg)

let opJie (tgt: string) (offset: int) =
    let isEven n = n % 2 = 0

    fun (reg: Registers) ->
        match tgt with
        | "a" when isEven reg.A -> (offset, reg)
        | "a" -> (1, reg)
        | "b" when isEven reg.B -> (offset, reg)
        | "b" -> (1, reg)
        | x -> failwith $"invalid register: jle {x}, {offset}"

let opJio (tgt: string) (offset: int) =
    let isOne n = n = 1

    fun (reg: Registers) ->
        match tgt with
        | "a" when isOne reg.A -> (offset, reg)
        | "a" -> (1, reg)
        | "b" when isOne reg.B -> (offset, reg)
        | "b" -> (1, reg)
        | x -> failwith $"invalid register: jio {x}, {offset}"

let parseFile (data: array<string>) =
    data
    |> Seq.map (fun line ->
        let words = line.Split([| ' '; ',' |], System.StringSplitOptions.RemoveEmptyEntries)

        match words[0] with
        | "hlf" -> opHlf words[1]
        | "tpl" -> opTpl words[1]
        | "inc" -> opInc words[1]
        | "jmp" -> opJmp (int words[1])
        | "jie" -> opJie words[1] (int words[2])
        | "jio" -> opJio words[1] (int words[2])
        | _ -> failwith "invalid instracution")
    |> Array.ofSeq

let simulate (opCode: array<Registers -> int * Registers>) (reg: Registers) =
    let rec loop (ip: int) (reg: Registers) =
        if ip >= opCode.Length then
            reg.B
        else
            let offset, nextReg = opCode[ip]reg
            loop (ip + offset) nextReg

    loop 0 reg

let partOne (opCode: array<Registers -> int * Registers>) = simulate opCode { A = 0; B = 0 }

let partTwo (opCode: array<Registers -> int * Registers>) = simulate opCode { A = 1; B = 0 }

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let opCode = File.ReadAllLines(args[0]) |> parseFile
        partOne opCode |> printfn "%d"
        partTwo opCode |> printfn "%d"
        0
