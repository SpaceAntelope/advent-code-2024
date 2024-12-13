open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

#load "../global.fsx"

type Point = int*int

type ClawMachine = {
    ButtonA: Point
    ButtonB: Point
    Prize: Point
}

let tokensA = 3
let tokensB = 1

let parse path = 
    let str2tuple str =
        Regex.Matches(str, "\\d+")
        |> Seq.map (fun m -> int m.Value)
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
            Prize = str2tuple parts.[2]
        })



let search (mac: ClawMachine) =
    let visited = HashSet<char*int*int>()
    let rec search' tokens pushedA pushedB (point: Point) = 
        [
            yield!
                if pushedA = 100 || pushedB = 100 then [0]
                else
                    match point with
                    | current when current = mac.Prize -> [tokens]
                    | x,y -> 
                        let (xA,yA) = mac.ButtonA
                        let (xB,yB) = mac.ButtonB
                        let (nextAx,nextAy) = x+xA,y+yA
                        let (nextBx,nextBy) = x+xB,y+yB
                        
                        [
                            if visited.Add ('A',nextAx,nextAy)
                            then yield! search' (tokens+tokensA) (pushedA+1) pushedB (nextAx, nextAy)
                            else 0
                            if visited.Add ('B',nextBx,nextBy)
                            then yield! search' (tokens+tokensB) pushedA (pushedB+1) (nextBx,nextBy)
                            else 0
                        ]
        ]
    
    search' 0 0 0 (0,0)
    |> List.filter (fun x -> x > 0)
    |> function 
    | [] -> 0 
    | result -> result |> List.min

"./input.example"
|> parse
|> Seq.map search
|> Seq.sum
|> Global.shouldBe 480

"./input.actual"
|> parse
|> Seq.map search
|> Seq.sum
|> printfn "Minimum tokens required to win what's winnable is %d"