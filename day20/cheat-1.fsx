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
    let cheatSet = cheats |> List.map fst |> Set.ofList
    for row in 0..rows-1 do
        for col in 0..cols-1 do
            if cheatSet.Contains (row,col)
            then '▚'
            else if maze.[row,col] = '#' then '░'
            else if maze.[row,col] = 'E' || maze.[row,col] = 'S'
            then maze.[row,col]
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

let isNotOverflowing (size: int*int) (point: Point) =
    let rows,cols = size
    let row,col = point
    row >= 0 && col >= 0 && row < rows && col < cols

let availableCheats (matrix: char array2d) (current: Point) : Cheat list = 
    let row,col= current
    let rows,cols = matrix |> Global.matrixSize
    let noOverflow point = isNotOverflowing (rows,cols) point

    [   (row-1,col),(row-2,col)
        (row+1,col),(row+2,col)
        (row,col-1),(row,col-2)
        (row,col+1),(row,col+2)    ]    
    |> List.filter (fun ((r1,c1),(r2,c2)) -> 
        noOverflow (r1,c1) && noOverflow (r2,c2)
        && matrix.[r1,c1] = '#' && matrix.[r2,c2] <> '#')

let travel (matrix: char array2d) =
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
        let updatedPath = path |> List.append [(row,col)]

        if matrix.[row,col] = 'E'
        then 
            printMaze matrix updatedPath []
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
    
    let rec countPicoseconds (position: int*int) (score: int) (activeCheat: Point) (path : Point list) (remainingScoreIndex: IReadOnlyDictionary<Point,int> ) (shortestDistanceSoFarIndex: int array2d) (matrix : char array2d) =
        let row,col = position

        if matrix.[row,col] = 'E'
        then [path]
            //printMaze matrix updatedPath []
            //printfn "Score: %d Path length: %d %A" score (updatedPath.Length) (sw.Elapsed)
            //[updatedPath]
        else
            let neighborhood = 
                let n = neighbors position
                if n |> List.contains activeCheat && shortestDistanceSoFarIndex.[fst activeCheat, snd activeCheat] = Int32.MaxValue
                then [activeCheat]
                else n

            neighborhood
            |> List.filter(fun (row,col) -> 
                (score + 1) < shortestDistanceSoFarIndex.[row,col]
                && matrix.[row,col] <> '#')
            |> List.collect(fun (nextRow,nextCol) -> 
                    let key = nextRow,nextCol
                // if remainingScoreIndex.ContainsKey key && shortestDistanceSoFarIndex.[fst activeCheat, snd activeCheat] < Int32.MaxValue
                // then remainingScoreIndex.[key]
                // else 
                    let newScore = score + 1
                    shortestDistanceSoFarIndex.[nextRow,nextCol] <- newScore
                    countPicoseconds key newScore activeCheat (path@[key]) remainingScoreIndex shortestDistanceSoFarIndex matrix)
            
            

    let fullPath = 
        travel' initPosition 0 []
        |> List.head

    printfn "Uncheated path length is %d" fullPath.Length
    // printfn "Uncheated path length is %s" (fullPath |> List.fold (sprintf "%s %A") "")

    let cheats = 
        let index = HashSet<Point>()
        
        fullPath 
        |> List.collect (availableCheats matrix)         
        |> List.filter (fun (pt1, _) ->  index.Add(pt1)) // prevent inverted direction cheats
    
    printMaze matrix fullPath cheats
    printfn "Cheats found: %d" cheats.Length

    let distanceToEndIndex = 
        let cheatIndex = 
            cheats 
            |> List.map (fun cheat -> fst cheat, -1) // so walk-through wall always gets chosen when in neighbors

        fullPath 
        |> List.rev
        |> List.mapi (fun index point -> point, fullPath.Length - index) 
        |> List.append cheatIndex
        |> readOnlyDict

    cheats 
    |> List.mapi(fun index cheat -> 
            let (deletedWallRow, deletedWallCol),_ = cheat
            let matrix' = Array2D.copy matrix
            matrix'.[deletedWallRow,deletedWallCol] <- '.'
            let shortestDistanceSoFar = Array2D.create rows cols (Int32.MaxValue)
            shortestDistanceSoFar.[fst initPosition, snd initPosition] <- 0
            // shortestDistanceSoFar.[deletedWallRow, deletedWallCol] <- -1
            let pathWithCheat = 
                countPicoseconds initPosition 0 (fst cheat) [] distanceToEndIndex shortestDistanceSoFar matrix'
                |> List.last

            printfn "CHEAT #%d %A -> psecs save %d" index cheat (fullPath.Length - 1 - pathWithCheat.Length)
            // pathWithCheat
            // |> List.iter (fun path -> 
            //     printfn "Path saved %d psecs" (fullPath.Length - path.Length - 1)
            
            //printMaze matrix' pathWithCheat [cheat]
            
            //pathWithCheat |> List.map(fun path -> fullPath.Length - 1 - path.Length)
            fullPath.Length - 1 - pathWithCheat.Length
    ) 
    |> List.countBy id
    |> List.sortBy fst
    |> List.iter (fun (score, freq) -> printfn $"There are {freq} cheats that save {score} picoseconds.")


"./input.example"    
|> parse
|> travel

"./input.actual"
|> parse
|> travel

