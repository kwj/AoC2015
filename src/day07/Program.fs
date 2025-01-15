module AoC2015.Day07

// Logic Gate: (input wires, evaluation function, output wire)
//
// Note:
// Before calling the evaluation function, it must be ensured that
// all input wires have been finalized.
type Gate = list<string> * (Map<string, uint16> -> uint16) * string

let lgAND (inp: list<string>) (out: string) : Gate =
    let fn (wires: Map<string, uint16>) =
        List.map (fun x -> wires[x]) inp |> List.reduce (&&&)

    (inp, fn, out)

let lgOR (inp: list<string>) (out: string) : Gate =
    let fn (wires: Map<string, uint16>) =
        List.map (fun x -> wires[x]) inp |> List.reduce (|||)

    (inp, fn, out)

let lgLSHIFT (inp: string) (out: string) (n: string) : Gate =
    let fn (wires: Map<string, uint16>) = wires[inp] <<< (int32 wires[n])
    ([ inp ], fn, out)

let lgRSHIFT (inp: string) (out: string) (n: string) : Gate =
    let fn (wires: Map<string, uint16>) = wires[inp] >>> (int32 wires[n])
    ([ inp ], fn, out)

let lgNOT (inp: string) (out: string) : Gate =
    let fn (wires: Map<string, uint16>) = ~~~wires[inp]
    ([ inp ], fn, out)

let lgWIRE (inp: string) (out: string) : Gate =
    let fn (wires: Map<string, uint16>) = wires[inp]
    ([ inp ], fn, out)

let (|UInt16|_|) (s: string) =
    match System.UInt16.TryParse(s) with
    | (true, x) -> Some(x)
    | _ -> None

let rec updateWireMap (m: Map<string, uint16>) (lst: list<string>) =
    match lst with
    | [] -> m
    | x :: xs ->
        match x with
        | UInt16 v -> updateWireMap (Map.add x v m) xs
        | _ -> updateWireMap m xs

let parseFile (data: array<string>) =
    let parseLine (gates: list<Gate>, wires: Map<string, uint16>) (str: string) =
        match str.Split(' ') with
        | ws when ws[1] = "AND" -> (lgAND [ ws[0]; ws[2] ] ws[4] :: gates, updateWireMap wires [ ws[0]; ws[2] ])
        | ws when ws[1] = "OR" -> (lgOR [ ws[0]; ws[2] ] ws[4] :: gates, updateWireMap wires [ ws[0]; ws[2] ])
        | ws when ws[1] = "LSHIFT" -> (lgLSHIFT ws[0] ws[4] ws[2] :: gates, updateWireMap wires [ ws[2] ])
        | ws when ws[1] = "RSHIFT" -> (lgRSHIFT ws[0] ws[4] ws[2] :: gates, updateWireMap wires [ ws[2] ])
        | ws when ws[0] = "NOT" -> (lgNOT ws[1] ws[3] :: gates, wires)
        | ws when ws[1] = "->" -> (lgWIRE ws[0] ws[2] :: gates, updateWireMap wires [ ws[0] ])
        | _ -> failwith "invalid gate"

    Array.fold parseLine (List.empty<Gate>, Map.empty<string, uint16>) data

let rec simulateAux (wires: Map<string, uint16>) (gates: list<Gate>) (unEvalGates: list<Gate>) =
    match gates with
    | [] -> wires, unEvalGates
    | (inp, fn, out) as g :: gs ->
        match List.forall (fun x -> Map.containsKey x wires) inp with
        | true -> simulateAux (Map.add out (fn wires) wires) gs unEvalGates
        | _ -> simulateAux wires gs (g :: unEvalGates)

let rec simulate (wires: Map<string, uint16>) (gates: list<Gate>) =
    match gates with
    | []-> wires
    | _ -> simulateAux wires gates List.empty<Gate> ||> simulate

let partOne (gates: list<Gate>) (wires: Map<string, uint16>) = simulate wires gates |> Map.find "a"

let partTwo (gates: list<Gate>) (wires: Map<string, uint16>) (b: uint16) =
    let newGates =
        lgWIRE (string b) "b" :: List.filter (fun (_, _, out) -> out <> "b") gates

    let newWires = Map.add (string b) b wires

    simulate newWires newGates |> Map.find "a"

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let gates, wires = File.ReadAllLines(args[0]) |> parseFile
        let p1Ans = partOne gates wires
        p1Ans |> printfn "%d"
        partTwo gates wires p1Ans |> printfn "%d"
        0
