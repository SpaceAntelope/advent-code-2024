open System
open System.IO
open System.Collections.Generic

#load "../global.fsx"
#load "./common.fsx"

open Common

let parse path = 
    path
    |> File.ReadAllLines
    |> Array.map _.Split(',')
    |> Array.map (Array.map int)
    |> Array.map (fun arr -> arr[1],arr[0])

let printMaze (size: int*int) (obstacles: Coord Set ) (path: Coord list) (newObstacles : Coord list) =  
    let (rows,cols) = size
    for row in 0..rows-1 do
        for col in 0..cols-1 do
            if obstacles.Contains(row,col) then '░'
            else if (row,col) = (newObstacles |> List.tryLast |> function Some x -> x | None -> (-1,-1))
            then 'O'
            else if newObstacles |> List.contains (row,col)
            then '╬'
            else if path |> List.contains (row,col)
            then '▇'
            else '.'
            |> printf "%c "
        printfn ""
    printfn ""

let findFirstByteThatBlocksExit(size: int*int) (startingByteCount: int) (path:string) = 

    let obstacles = 
        path
        |> parse

    let mutable byteCount = startingByteCount

    let mutable obstacleSet = 
        obstacles 
        |> Array.take byteCount
        |> Set.ofArray

    let newObstacles = ResizeArray<Coord>()
    let originalObstacleSet = obstacles |> Array.take startingByteCount |> Set.ofArray
    let mutable shortestPath = [obstacles.[byteCount]]
    
    while shortestPath <> [] do
        let newByte = obstacles.[byteCount]
        obstacleSet <- obstacleSet |> Set.add newByte
        // printfn "%4d New byte added: %A" byteCount newByte
        if shortestPath |> List.contains newByte
        then 
            shortestPath <- AStar size obstacleSet
            printMaze size originalObstacleSet shortestPath ((newObstacles |> List.ofSeq)@[newByte])
        
        byteCount <- byteCount + 1
        newObstacles.Add(newByte)

    obstacles.[byteCount-1]
    

findFirstByteThatBlocksExit (7,7) 12 "./input.example"    
|> Global.shouldBe (1,6)

findFirstByteThatBlocksExit (71,71) 1024 "./input.actual"    
|> printfn "The coordinates of the first byte that will prevent the exit from being reachable from your starting position are %A (row,col i.e. Y,X)" 
