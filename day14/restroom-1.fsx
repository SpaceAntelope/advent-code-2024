open System
open System.IO
open System.Text.RegularExpressions

#load "../global.fsx"

type Point = { X: int; Y: int}

type Robot = {
    Position: Point
    Velocity: Point
} with 
    member r.next(rowCount,colCount) = 
        { 
            r with Position = {
                X = ((r.Position.X + r.Velocity.X) % colCount) |> fun x -> if x < 0 then colCount + x else x
                Y = ((r.Position.Y + r.Velocity.Y) % rowCount) |> fun y -> if y < 0 then rowCount + y else y  } 
        }
    override r.ToString() =
        sprintf "p = %d,%d v = %d %d" r.Position.Y r.Position.X r.Velocity.Y r.Velocity.X


let printRobots (size: int*int) (robots: Robot list) = 
    let (rows,cols) = size
    let middleRow = rows/2
    let middleCol = cols/2
    let idx = robots |> List.countBy _.Position |> Map.ofList
    Array2D.init rows cols (fun row col -> 
        idx 
        |> Map.tryFind ({ X = col; Y = row})
        |> function
        | Some count -> '0' + (char count)
        | None when col = middleCol || row = middleRow -> ' '
        | None -> '.')
    |> Global.print


let parse path = 
    path
    |> File.ReadAllLines
    |> Array.map(fun line -> 
        let m = Regex.Match(line, @"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)")
        {
            Position = { X = int m.Groups[1].Value; Y = int m.Groups[2].Value }
            Velocity = { X = int m.Groups[3].Value; Y=  int m.Groups[4].Value }
        })
    |> List.ofArray


let rec advance (seconds: int) (areaSize: int*int) (robots: Robot list) =
    if seconds = 0
    then robots
    else 
        let (rowCount,colCount) = areaSize
        robots
        |> List.map _.next(rowCount,colCount)
        |> advance (seconds-1) areaSize

let quadrants (areaSize: int*int) (robots: Robot list) =
    let (rowCount,colCount) = areaSize
    let middleRow = rowCount / 2
    let middleCol = colCount / 2
    let q (p: Point) = 
        match p with 
        | { X =x; Y=y} when x < middleCol && y < middleRow -> 1
        | { X =x; Y=y} when x < middleCol && y > middleRow -> 2
        | { X =x; Y=y} when x > middleCol && y < middleRow -> 3
        | { X =x; Y=y} when x > middleCol && y > middleRow -> 4
        | _ -> -1

    robots
    |> List.map _.Position
    |> List.countBy q
    |> Global.tee "q: "
    |> List.filter (fun (qdrant,_) -> qdrant > 0)
    |> List.map snd
    |> List.reduce (*)


let exampleArea = 7,11
"./input.example"        
|> parse
|> fun r -> printRobots exampleArea r; r
|> advance 100 exampleArea 
|> fun r -> printRobots exampleArea r; r
|> quadrants exampleArea
|> Global.shouldBe 12

let actualArea = 103,101
"./input.actual"        
|> parse
|> advance 100 actualArea 
|> quadrants actualArea
|> printfn "The product of the numer of robots in each quadrant is %d"

"./input.actual"        
|> parse
|> advance 100 actualArea 
|> quadrants actualArea
|> printfn "The product of the numer of robots in each quadrant is %d"