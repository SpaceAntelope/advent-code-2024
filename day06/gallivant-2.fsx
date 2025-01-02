open System
open System.IO
open System.Text.RegularExpressions

#load "../global.fsx"
#load "./common.fsx"

open Common
open Global

(*
    Turns out most of the problem was that I didn't get that the puzzle required the guard to be starting from the
    initial position every time, instead of assuming they'll always get to the new obstacle from the correct direction. 
    That the example cases worked either way certainly didn't help.
*)

type Dir = Up | Dn | Lt | Rt

let rotateRight currentDir = 
    match currentDir with
    | Up -> Rt
    | Rt -> Dn
    | Dn -> Lt
    | Lt -> Up

let next current dir =
    let row,col = current
    
    match dir with
    | Up -> row-1,col
    | Dn -> row+1,col
    | Rt -> row,col+1
    | Lt -> row,col-1

let reverseDir dir =
    match dir with
    | Up -> Dn
    | Dn -> Up
    | Rt -> Lt
    | Lt -> Rt

let getObstacles (matrix : char array2d) =
    matrix
    |> Global.matrixIndices
    |> Seq.filter (fun (row,col) -> matrix.[row,col] = '#' )

let printMaze (matrix : char array2d) (path: (Point*Dir) seq) (init: (Point*Dir) seq) (addedObstacles: Point seq)=  

    let obstacles = getObstacles matrix |> Set, '░'
    let directedPath = 
        path 
        |> Seq.groupBy snd 
        |> Seq.map (fun (dir,grp)->
            grp |> Seq.map fst |> Set,
            match dir with 
            | Up -> '▲'
            | Dn -> '▼'
            | Lt -> '◄'
            | Rt -> '►')
    let initPositions =
        init
        |> Seq.groupBy snd
        |> Seq.map (fun (dir,grp)->
            grp |> Seq.map fst |> Set,
            match dir with 
            | Up -> '↑'
            | Dn -> '↓'
            | Lt -> '←'
            | Rt -> '→')
        
    let newObstacles = addedObstacles |> Set, 'O'

    Global.printMatrixBase matrix [newObstacles; yield! initPositions; obstacles; yield! directedPath; ]


let traverse (matrix: char array2d) (pos: Point) (dir: Dir) =
    let rows,cols = matrixSize matrix
    let mutable visited = Set.empty<Point*Dir>
    let isNotVisited pos dir = visited.Contains (pos,dir) |> not
    // let isNotObstacle pos = matrix.[fst pos, snd pos] <> '#'
    let isInBounds (row,col) = row <= rows-1 && col <= cols-1 && col >= 0 && row >= 0
    let isObstacle pos = isInBounds pos && matrix.[fst pos, snd pos] = '#'
    
    let mutable currentPos = pos
    let mutable currentDir = dir
    let mutable outOfBounds = isInBounds currentPos |> not
    let mutable cyclical = isNotVisited currentPos currentDir |> not

    [   while not outOfBounds && not cyclical do
            yield currentPos, currentDir

            visited <- visited |> Set.add (currentPos,currentDir)
            
            let nextPosition = next currentPos currentDir
            if isObstacle nextPosition
            then 
                currentDir <- rotateRight currentDir
            else 
                currentPos <- nextPosition
            
            outOfBounds <- isInBounds currentPos |> not
            cyclical <- isNotVisited currentPos currentDir |> not
    ], outOfBounds, cyclical
        
let bfCountCycles matrix =
    let initPos = 
        matrix
        |> matrixIndices
        |> Seq.find (fun (row,col) -> matrix.[row,col] = '^' )
    
    let rows,cols = matrixSize matrix
    let isInBounds (row,col) = row <= rows-1 && col <= cols-1 && col >= 0 && row >= 0
    let isObstacle pos = isInBounds pos && matrix.[fst pos, snd pos] = '#'
    let mutable obstructionsThatLeadToCycle = Set.empty<Point>

    let originalPath,_,_ = traverse matrix initPos Up

    for (pos, dir) in originalPath do
        let nextPos = next pos dir
        if not (isObstacle nextPos) && isInBounds nextPos && nextPos <> initPos
        then 
            let row,col = nextPos
            matrix.[row,col] <- '#'
            let path,_,isCycle = traverse matrix initPos Up
            matrix.[row,col] <- '.'
            
            if isCycle 
            then 
                obstructionsThatLeadToCycle <- obstructionsThatLeadToCycle |> Set.add nextPos // cycleCount <- cycleCount + 1
                // printMaze matrix path [pos,dir] [nextPos]
                
    obstructionsThatLeadToCycle.Count

let sanityCheck filepath expected =
    let matrix= 
        filepath
        |> parseData

    let initialPosition = 
        matrix
        |> matrixIndices
        |> Seq.find (fun (row,col) -> matrix.[row,col] = '^' )


    let path, bounds, cycle= 
        traverse matrix initialPosition Up 

    printMaze matrix path [initialPosition,Up] []
    printfn "Is cyclical: %b Finished out of bounds: %b" cycle bounds

    path
    |> List.map fst
    |> List.distinct
    |> List.length
    |> Global.shouldBe expected

    printfn "%s pt1 test successful" filepath

sanityCheck "./input.example" 41
sanityCheck "./input.actual" 4758


"./input.example"
|> parseData
|> bfCountCycles
|> shouldBe 6


"./input.actual"
|> parseData
|> bfCountCycles
|> printfn "%d different positions could be chosed for this obstruction."

