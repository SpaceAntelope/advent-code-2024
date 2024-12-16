open System
open System.IO
open System.Collections.Generic

#load "../global.fsx"
open System.Diagnostics

let parse path =
    path
    |> File.ReadAllLines
    |> Array.map _.ToCharArray()
    |> array2D

type FacingDirection = N | E | S | W
type MovingDirection = Fwd | Left | Right

let move (position: int*int) facingDirection movingDirection =
    let row,  col= position

    match facingDirection,movingDirection with
    | N, Fwd -> (row-1,col),N,1
    | N, Left -> (row,col-1),W,1001
    | N, Right -> (row,col+1),E,1001
    | E, Fwd -> (row,col+1),E,1
    | E, Left -> (row-1,col),N,1001
    | E, Right -> (row+1,col),S,1001
    | S, Fwd -> (row+1,col),S,1
    | S, Left -> (row,col+1),E,1001
    | S, Right -> (row,col-1),W,1001
    | W, Fwd -> (row,col-1),W,1
    | W, Left -> (row+1,col),S,1001
    | W, Right -> (row-1,col),N,1001

let printMultitPath  (paths: Map<int*int,FacingDirection> list)  (matrix: char array2d) =
    let rows,cols = Global.matrixSize matrix

    let mergedPath = 
        paths 
        |> List.collect Map.toList
        |> List.map fst
        |> Set.ofList

    for row in 0..rows-1 do
        for col in 0..cols-1 do
            let value = matrix.[row,col]             
            if mergedPath |> Set.contains (row,col) && value <> 'S' && value <> 'E'
            then '▉'
            else matrix.[row,col]
            |> printf " %c"
        printfn ""
    printfn ""

let travelAllPaths (matrix: char array2d) =
    let (rows,cols) =Global.matrixSize matrix
    
    let smallestScoreSoFar = 
        let dic = Dictionary<int*int*FacingDirection,int>()
        matrix 
        |> Global.matrixIndices
        |> Seq.iter(fun (row,col) -> 
            [N;S;E;W] |> List.iter (fun dir -> dic.Add((row,col,dir), Int32.MaxValue)))
        dic

    let initPosition =
        matrix        
        |> Global.matrixIndices
        |> Seq.find (fun (row,col) -> matrix.[row,col] = 'S')
    
    let mutable smallestTotalScore = Int32.MaxValue
    smallestScoreSoFar[(fst initPosition, snd initPosition, E)] <- 0

    let sw = Stopwatch()
    sw.Start()

    let rec travel' (position: int*int) (dir: FacingDirection) (totalScore: int) depth (path: Map<int*int,FacingDirection>) =
        let row,col = position
        let updatedPath = path |> Map.add(row,col) dir

        if matrix.[row,col] = 'E'
        then 
            if smallestTotalScore > totalScore then smallestTotalScore <- totalScore
            printfn "Score: %d Path length: %d Depth: %d %A" totalScore (updatedPath.Count) depth (sw.Elapsed)
            [totalScore,updatedPath]
        else
            [Fwd;Right;Left]             
            |> List.map (fun moveDir -> move (row,col) dir moveDir)
            |> List.filter(fun ((row,col: int), dir, score ) -> 
                (totalScore + score) <= smallestScoreSoFar.[row,col,dir] && (totalScore + score) <= smallestTotalScore
                && matrix.[row,col] <> '#' && not (path |> Map.containsKey(row,col)))
            |> List.collect(fun (pos,faceDir, score) -> 
                let r,c = pos
                smallestScoreSoFar.[(r,c,dir)] <- totalScore + score
                travel' pos faceDir (totalScore + score) (depth+1) updatedPath)

    travel' initPosition E 0 0 (Map.empty)

let collectPositionsFromBestPaths (matrix: char array2d) =
    matrix
    |> travelAllPaths
    |> List.groupBy fst
    |> List.minBy fst
    |> snd
    |> List.map snd
    |> fun paths -> printMultitPath paths matrix; paths
    |> List.collect Map.toList
    |> List.map fst
    |> List.distinct

"./input.example"
|> parse
|> collectPositionsFromBestPaths
|> List.length
|> Global.shouldBe 45

"./input.example2"
|> parse
|> collectPositionsFromBestPaths
|> List.length
|> Global.shouldBe 64

"./input.actual"
|> parse
|> collectPositionsFromBestPaths
|> List.length
|> printfn "%d tiles are part of at least one of the best paths through the maze"