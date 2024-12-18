open System
open System.IO
open System.Collections.Generic

#load "../global.fsx"
#load "./common.fsx"

open Common

let parse (count: int) path = 
    path
    |> File.ReadAllLines
    |> Array.take count
    |> Array.map _.Split(',')
    |> Array.map (Array.map int)
    |> Array.map (fun arr -> arr[1],arr[0])
    |> Set.ofArray


let printMaze (size: int*int) (obstacles: (int*int) Set ) (path: (int*int) list) =  
    let (rows,cols) = size
    for row in 0..rows-1 do
        for col in 0..cols-1 do
            if obstacles.Contains(row,col) then '░'
            else if path |> List.contains (row,col)
            then '▇'
            else '.'
            |> printf "%c "
        printfn ""
    printfn ""

type Coord = int*int
let obstacles = 
    "./input.example"    
    |> parse 12

obstacles
|> AStar (7,7)
|> fun path ->
    printMaze (7,7) obstacles path
    path.Length |> Global.shouldBe 22


"./input.actual"    
|> parse 1024
|> AStar (71,71)
|> List.length
|> printfn "The minimum number of steps needed to reach the exit is %d"

