open System
open System.IO
open System.Text.RegularExpressions

#load "../global.fsx"

type Point = { X: int; Y: int}

type Robot = {
    Id: int
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
        sprintf "id: %d p = %d,%d v = %d %d" r.Id r.Position.Y r.Position.X r.Velocity.Y r.Velocity.X


let printRobots (size: int*int) (robots: Robot list) = 
    let (rows,cols) = size
    let middleRow = rows/2
    let middleCol = cols/2
    let idx = robots |> List.countBy _.Position |> Map.ofList
    let matrix = Array2D.init rows cols (fun row col -> 
        idx 
        |> Map.tryFind ({ X = col; Y = row})
        |> function
        | Some count -> '0' + (char count)
        | None when col = middleCol || row = middleRow -> ' '
        | None -> ' ')
    
    for row in 0..rows-1 do
        for col in 0..cols-1 do
            printf " %c" matrix.[row,col]
        printfn ""
    printfn "%s" <| "\n".PadLeft(cols * 2, '_')

let parse path = 
    path
    |> File.ReadAllLines
    |> Array.mapi(fun index line -> 
        let m = Regex.Match(line, @"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)")
        {   Id = index
            Position = { X = int m.Groups[1].Value; Y = int m.Groups[2].Value }
            Velocity = { X = int m.Groups[3].Value; Y=  int m.Groups[4].Value }
        })
    |> List.ofArray


// before realizing that the patterns start recycling early-ish
let rec advanceUntil (guard: int -> int*int -> Robot list -> bool) (seconds: int) (areaSize: int*int) (robots: Robot list) =
    if guard seconds areaSize robots
    then
        printfn "Seconds: %d" seconds
        printRobots areaSize robots
        robots
    else 
        let (rowCount,colCount) = areaSize
        robots
        |> List.map _.next(rowCount,colCount)
        |> advanceUntil guard (seconds+1) areaSize

// after realizing there's only ~10.500 position patterns
let allPositions()  = 
    let original = "./input.actual"  |> parse
    let state = "./input.actual" |> parse
    let rows,cols = 103, 101
    
    List.unfold (
        fun ((state: Robot list,seconds: int)) -> 
            let next =
                 state 
                 |> List.map (fun (r: Robot) -> r.next(rows,cols))
                 |> List.sortBy _.Id

            if seconds % 10000 = 0 then printfn "Seconds: %d" seconds

            if next = original 
            then None
            else Some ((next, seconds + 1), (next, seconds + 1))
        ) (state, 0)

let getQuadrant (areaSize: int*int) =
    let (rowCount,colCount) = areaSize
    let middleRow = rowCount / 2
    let middleCol = colCount / 2
    fun (p: Point) -> 
        match p with 
        | { X =x; Y=y} when x < middleCol && y < middleRow -> 1
        | { X =x; Y=y} when x < middleCol && y > middleRow -> 2
        | { X =x; Y=y} when x > middleCol && y < middleRow -> 3
        | { X =x; Y=y} when x > middleCol && y > middleRow -> 4
        | _ -> -1
   
let actualArea = 103,101

let q = getQuadrant actualArea

let sortByQ robots =
    robots 
        |> List.map _.Position 
        |> List.countBy q 
        |> List.map snd 
        |> List.reduce (*)

let sortByColumnDistribution (robots : Robot list) = 
    robots
    |> List.filter (fun robot -> robot.Position.Y > 51 && robot.Position.X < 51)
    |> List.length

let sortBySafetyFactor (robots: Robot list) =
    robots |> List.map _.Position |> List.countBy q |> List.map snd |> List.reduce (*)

let sortByBotsInCenter (robots: Robot list) =
     robots
    |> List.filter (fun robot -> 
        robot.Position.Y > 30 && robot.Position.X > 30 
        && robot.Position.Y < 70 && robot.Position.X < 70 )
    |> List.length

allPositions()
// |> List.sortByDescending (fun (matrix, seconds) -> sortByColumnDistribution matrix)
// |> List.sortBy (fun (matrix, seconds) -> sortBySafetyFactor matrix)
|> List.sortByDescending (fun (matrix, seconds) -> sortByBotsInCenter matrix)
|> List.head
|> fun (r,secs) -> 
    printfn "Seconds: %d" secs
    printRobots actualArea r