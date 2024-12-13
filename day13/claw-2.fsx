open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

#load "../global.fsx"

type Point = int64*int64

type ClawMachine = {
    ButtonA: Point
    ButtonB: Point
    Prize: Point
}

let tokensA = 3.0M 
let tokensB = 1.0M

let prizeOffset = 10000000000000L

let parse path = 
    let str2tuple str =
        Regex.Matches(str, "\\d+")
        |> Seq.map (fun m -> int64 m.Value)
        |> Array.ofSeq
        |> fun arr -> (arr.[0],arr.[1])

    path
    |> File.ReadAllText
    |> fun x -> Regex.Split(x, "\r\n\r\n")
    |> Array.map (fun str -> 
        let parts  =
            str.Split('\n') 
        {
            ButtonA = str2tuple parts.[0]
            ButtonB = str2tuple parts.[1]
            Prize = 
                str2tuple parts.[2] 
                |> fun (x,y) -> (x + prizeOffset, y + prizeOffset)
        })

let tuple2float (x,y) = decimal x, decimal y

let solveMachine (mac: ClawMachine) = 
    let (xa ,ya) = mac.ButtonA |> tuple2float
    let (xb ,yb) = mac.ButtonB |> tuple2float
    let (xp ,yp) = mac.Prize |> tuple2float
    
    let D = xa*yb - ya*xb
    if D = 0M then None
    else
        let Da = xp*yb - yp*xb
        let Db = xa*yp - ya*xp
        let A = Da/D
        let B = Db/D

        Some (A,B)

    (*
    xp = Axa + Bxb
    yp = Αya + Byb

    A = x
    B = y
    xa = a1 
    ya = a2 
    xb = b1 
    yb = b2
    k1 = xp
    k2 = yp

    *)

let areFactorsIntegers (a: decimal, b: decimal) =
     a = Math.Floor(a) && b = Math.Floor(b)

let sumTokens (results : (decimal*decimal) array) =
    if results = [||]
    then 0M
    else results |> Array.fold (fun state (a,b) -> state + a * tokensA + b * tokensB) 0M

"./input.example"
|> parse
|> Array.map (fun x -> 
    { x with Prize = (fst x.Prize - prizeOffset), (snd x.Prize - 10000000000000L)})
|> Array.choose solveMachine
|> Array.filter areFactorsIntegers
|> sumTokens
|> Global.shouldBe 480.0M

"./input.example"
|> parse
|> Array.map (solveMachine>>Option.map(areFactorsIntegers))
|> Array.choose id
|> Global.shouldBe [|false;true;false;true|]

"./input.actual"
|> parse
|> Array.choose solveMachine
|> Array.filter areFactorsIntegers
|> sumTokens
|> printfn "Minimum tokens required to reach all available prizes %f" 

