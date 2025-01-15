module AoC2015.Day15

type Ingredient =
    { Cap: int
      Du: int
      Fla: int
      Txr: int
      C: int }

    static member (+)(L: Ingredient, R: Ingredient) =
        { Cap = L.Cap + R.Cap
          Du = L.Du + R.Du
          Fla = L.Fla + R.Fla
          Txr = L.Txr + R.Txr
          C = L.C + R.C }

    static member ( * )(this: Ingredient, mul: int) =
        { Cap = this.Cap * mul
          Du = this.Du * mul
          Fla = this.Fla * mul
          Txr = this.Txr * mul
          C = this.C * mul }

    // Return total score and calorie
    member self.Score() =
        (max self.Cap 0) * (max self.Du 0) * (max self.Fla 0) * (max self.Txr 0), self.C

let parseFile (data: array<string>) =
    data
    |> Seq.map (fun line ->
        let words = line.Split([| ' '; ',' |], System.StringSplitOptions.RemoveEmptyEntries)

        { Cap = int words[2]
          Du = int words[4]
          Fla = int words[6]
          Txr = int words[8]
          C = int words[10] })
    |> Array.ofSeq

// All cookie recipes
let rec allRecipes (nIngredients: int) (amount: int) =
    match (nIngredients, amount) with
    | (1, amt) -> [ [ amt ] ]
    | (n, 0) -> [ List.init n (fun _ -> 0) ]
    | (n, amt) ->
        seq {
            for x = 0 to amt do
                yield! allRecipes (n - 1) (amt - x) |> List.map (fun lst -> x :: lst)
        }
        |> List.ofSeq

let allScores (recipes: list<list<int>>) (ingrs: array<Ingredient>) =
    let score (recipe: list<int>) =
        recipe
        |> List.mapi (fun i amount -> ingrs[i] * amount)
        |> List.reduce (+)
        |> _.Score()

    Seq.map score recipes

let partOne (recipes: list<list<int>>) (ingrs: array<Ingredient>) =
    allScores recipes ingrs |> Seq.map fst |> Seq.max

let partTwo (recipes: list<list<int>>) (ingrs: array<Ingredient>) (calorie: int) =
    allScores recipes ingrs
    |> Seq.filter ((=) calorie << snd)
    |> Seq.map fst
    |> Seq.max

module Main =
    open System.IO

    [<EntryPoint>]
    let main args =
        assert (Array.length args = 1)
        let ingrs = File.ReadAllLines(args[0]) |> parseFile
        let recipes = allRecipes ingrs.Length 100
        partOne recipes ingrs |> printfn "%d"
        partTwo recipes ingrs 500 |> printfn "%d"
        0
