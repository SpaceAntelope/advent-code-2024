open System
open System.IO

#load "../global.fsx"
open System.Diagnostics
open System.Collections.Generic

let parse path = 
    path
    |> File.ReadAllLines
    |> Array.map _.ToCharArray()
    |> array2D

type Point = int*int
type Neighbor = Up of Point | Dn of Point | Lt of Point | Rt of Point
type Cheat = Point*Point
type CheatingStatus = UseCheats of Cheat option | NoCheats

let printMaze (maze: char array2d) (path: (int*int) seq) (cheats: Cheat list)=  
    let (rows,cols) = Global.matrixSize maze
    for row in 0..rows-1 do
        for col in 0..cols-1 do
            if maze.[row,col] = '#' then '░'
            else if path |> Seq.contains (row,col)
            then '▇'
            else maze.[row,col]
            |> printf "%c "
        printfn ""
    printfn ""



let neighbors' (current: Point) =
    let row,col = current
    [   Up (row+1,col)
        Dn (row-1,col)
        Rt (row,col+1)
        Lt (row,col-1) ]

let neighbors (current: Point) =
    let row,col = current
    [  (row+1,col)
       (row-1,col)
       (row,col+1)
       (row,col-1) ]
// type X = A of int | B of int | C of int

// let x = B 2

// match x with
// | A y
// | B y 
// | C y when y = 1 -> "l"

let availableCheats' (neighbors: Neighbor list) (matrix: char array2d) : Cheat list =
    neighbors 
    |> List.filter (fun neighbor -> 
        match neighbor with
        | Up (r,c)
        | Dn (r,c)
        | Lt (r,c)
        | Rt (r,c) when matrix.[r,c] = '#' -> true
        | _ -> false )
    |> List.choose (fun neighbor -> 
        match neighbor with
        | Up (row,col) when matrix.[row-1,col] <> '#'-> Some ((row,col),(row-1,col))
        | Dn (row,col) when matrix.[row+1,col] <> '#'-> Some ((row,col),(row+1,col))
        | Lt (row,col) when matrix.[row,col-1] <> '#'-> Some ((row,col),(row,col-1))
        | Rt (row,col) when matrix.[row,col+1] <> '#'-> Some ((row,col),(row,col+1))
        | x -> None        
    )

let availableCheats (current: Point) (matrix: char array2d) : Cheat list = 
    let row,col= current
    [   (row-1,col),(row-2,col)
        (row+1,col),(row+2,col)
        (row,col-1),(row,col-2)
        (row,col+1),(row,col+2)    ]
    |> List.filter (fun ((r1,c1),(r2,c2)) -> matrix.[r1,c1] = '#' && matrix.[r2,c2] <> '#')

let travel (matrix: char array2d) =
    let (rows,cols) =Global.matrixSize matrix
    let shortestDistanceSoFar = Array2D.create rows cols (Int32.MaxValue)
    
    let initPosition =
        matrix        
        |> Global.matrixIndices
        |> Seq.find (fun (row,col) -> matrix.[row,col] = 'S')
        
    shortestDistanceSoFar.[fst initPosition, snd initPosition] <- 0

    let cheatsUsed = HashSet<Cheat>()
    // let mutable minScore = 1000000000//204824
    let sw = Stopwatch()
    sw.Start()

    let rec travel' (position: int*int) (picoseconds: int) (path: Point list) =
        let row,col = position
        let updatedPath = path |> List.append [(row,col)]

        if matrix.[row,col] = 'E'
        then 
            // printPath path matrix
            // printMaze matrix updatedPath
            printfn "Score: %d Path length: %d %A" picoseconds (updatedPath.Length) (sw.Elapsed)
            //if picoseconds < minScore then minScore <- picoseconds
            [updatedPath]
        else
            let neighborhood = neighbors position
            //let cheats = availableCheats position matrix

            neighborhood
            |> List.filter(fun (row,col) -> 
                 (picoseconds + 1) < shortestDistanceSoFar.[row,col]
                // && (picoseconds + 1) < minScore 
                && matrix.[row,col] <> '#' 
                && not (path |> List.contains(row,col)))
            |> List.collect(fun (pos) -> 
                let newScore = picoseconds + 1
                shortestDistanceSoFar.[fst pos, snd pos] <- newScore
                travel' pos newScore updatedPath)

    travel' initPosition 0 []

"./input.example"    
|> parse
|> travel