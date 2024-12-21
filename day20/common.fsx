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
type CheatResult = { Saved: int; CheatCount: int } with override x.ToString() = $"There are {x.CheatCount} cheats that save {x.Saved} picoseconds."

let printMaze (maze: char array2d) (path: (int*int) seq) (cheats: Cheat list) (position: Point)=  
    let (rows,cols) = Global.matrixSize maze
    let cheatSet = cheats |> List.map fst |> Set.ofList

    let printColIndex () = 
        [|0..cols-1|] 
        |> Array.map (sprintf "%03d") 
        |> Array.map _.ToCharArray() 
        |> Array.transpose 
        |> Array.iter (fun digits-> 
            printf "   "
            digits |> Array.iter(printf " %c")
            printfn ""
        )

    // let path' = path |> Array.ofSeq

    printColIndex()
    for row in 0..rows-1 do
        printf "%03d " row
        for col in 0..cols-1 do        
            if cheatSet.Contains (row,col)
            then '▚'
            else if (row,col) = position
            then '⦾' //⦿
            else if maze.[row,col] = '#' then '░'
            else if maze.[row,col] = 'E' || maze.[row,col] = 'S'
            then maze.[row,col]
            else if path |> Seq.contains (row,col)
            then '▇'
            // then path' |> Array.findIndex (fun (r,c) -> r = row && c = col) |> fun c -> char ((c + int '0')% (int (char 10))) // '▇'
            else maze.[row,col]
            |> printf "%c "
        printf "%03d" row
        if row < rows then printfn ""
    printColIndex()
    printfn ""

let neighbors (current: Point) =
    let row,col = current
    [  (row+1,col)
       (row-1,col)
       (row,col+1)
       (row,col-1) ]

let isNotOverflowing (size: int*int) (point: Point) =
    let rows,cols = size
    let row,col = point
    row >= 0 && col >= 0 && row < rows && col < cols


let findFullPath (matrix: char array2d) =
    let (rows,cols) =Global.matrixSize matrix
    let shortestDistanceSoFar = Array2D.create rows cols (Int32.MaxValue)
    
    let initPosition =
        matrix        
        |> Global.matrixIndices
        |> Seq.find (fun (row,col) -> matrix.[row,col] = 'S')
        
    shortestDistanceSoFar.[fst initPosition, snd initPosition] <- 0

    // let cheatsUsed = HashSet<Cheat>()
    // let mutable minScore = 1000000000//204824
    let sw = Stopwatch()
    sw.Start()

    let rec travel' (position: int*int) (score: int) (path: Point list) =
        let row,col = position
        let updatedPath = path@[(row,col)]

        if matrix.[row,col] = 'E'
        then 
            // printMaze matrix updatedPath []
            printfn "Score: %d Path length: %d %A" score (updatedPath.Length) (sw.Elapsed)
            [updatedPath]
        else
            let neighborhood = neighbors position

            neighborhood
            |> List.filter(fun (row,col) -> 
                (score + 1) < shortestDistanceSoFar.[row,col]
                && matrix.[row,col] <> '#' 
                && not (path |> List.contains(row,col)))
            |> List.collect(fun (pos) -> 
                let newScore = score + 1
                shortestDistanceSoFar.[fst pos, snd pos] <- newScore                
                travel' pos newScore updatedPath)
    

      

    let fullPath = 
        travel' initPosition 0 []
        |> List.head

    printfn "Uncheated path length is %d" fullPath.Length
    // printfn "Uncheated path length is %s" (fullPath |> List.fold (sprintf "%s %A") "")

    initPosition |> Global.shouldBe fullPath.[0]

    fullPath