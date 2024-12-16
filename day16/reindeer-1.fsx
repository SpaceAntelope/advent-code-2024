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


let printPath  (path: Map<int*int,FacingDirection>)  (matrix: char array2d) =
    let rows,cols = Global.matrixSize matrix
    for row in 0..rows-1 do
        for col in 0..cols-1 do
            let value = matrix.[row,col]             
            if path |> Map.containsKey (row,col) && value <> 'S' && value <> 'E'
            then match path.[row,col] with N -> '^' | W -> '<' | S -> 'v' | E -> '>'
            else matrix.[row,col]
            |> printf " %c"
        printfn ""
    printfn ""

let reconstructPath (cameFrom : Dictionary<int*int,int*int>) current = 
    let totalPath = ResizeArray()
    let mutable _current = current
    while cameFrom.ContainsKey current do
        _current <- cameFrom.[current]
        totalPath.Add _current

    totalPath |> Seq.rev

let neihgbors (node: int*int) = 
    let row,col = node
    [row-1,col;row,col+1;row+1,col;row,col-1]


// let AStar (start:int*int) (stop:int*int) (h: (int*int)->int) (matrix: char array2d)=
//     let openSet = PriorityQueue<int*int,int>()
   
//     let cameFrom = Dictionary<int*int,int*int>()
    
//     let gScore = Dictionary<int*int,int>()
//     gScore.Add( start, 0)

//     let fScore = Dictionary<int*int,int>()
//     fScore.Add( start, (h start))
//     openSet.Add(start, fScore.[start])

//     while openSet.Count > 0 do
//         let current = openSet.Dequeue() // |> Seq.minBy (fun point -> fScore.[point])
//         if current = stop
//         then reconstructPath cameFrom current
//         else 
//             openSet.Remove(current)
//             let neighbors = 
//                 neihgbors current 
//                 |> List.filter (fun (row,col) -> matrix.[row,col] <> '#' )

//             for n in (neihgbors current) do
//                 let tentativeGScore = gScore[current] 
//                 ()


                //cameFrom.Add(n, current)


let travel (matrix: char array2d) =
    let (rows,cols) =Global.matrixSize matrix
    let shortestDistanceSoFar = Array2D.create rows cols (Int32.MaxValue)
    
    let initPosition =
        matrix        
        |> Global.matrixIndices
        |> Seq.find (fun (row,col) -> matrix.[row,col] = 'S')
    
    shortestDistanceSoFar.[fst initPosition, snd initPosition] <- 0

    let mutable minScore = 1000000000//204824
    let sw = Stopwatch()
    sw.Start()
    let rec travel' (position: int*int) (dir: FacingDirection) (totalScore: int) depth (path: Map<int*int,FacingDirection>) =
        let row,col = position
        let updatedMap = path |> Map.add(row,col) dir

        if matrix.[row,col] = 'E'
        then 
            // printPath path matrix
            printfn "Score: %d Path length: %d Depth: %d %A" totalScore (updatedMap.Count) depth (sw.Elapsed)
            if totalScore < minScore then minScore <- totalScore
            [totalScore]
        else
            // match dir with
            // | N -> [Fwd;Right;Left] 
            // | S -> [Right;Left;Fwd] 
            // | E -> [Left;Fwd;Right] 
            // | W -> [Right;Left;Fwd] 
            [Fwd;Right;Left]             
            |> List.map (fun moveDir -> move (row,col) dir moveDir)
            |> List.filter(fun ((row,col), _, score ) -> 
                 (totalScore + score) < shortestDistanceSoFar.[row,col]
                && (totalScore + score) < minScore 
                && matrix.[row,col] <> '#' 
                && not (path |> Map.containsKey(row,col)))
            |> List.collect(fun (pos,faceDir, score) -> 
                shortestDistanceSoFar.[fst pos, snd pos] <- totalScore + score
                travel' pos faceDir (totalScore + score) (depth+1) updatedMap)
        
    

    travel' initPosition E 0 0 (Map.empty)

"./input.example"
|> parse
|> travel
|> Global.tee "Path scores: "
|> List.min
|> Global.shouldBe 7036

"./input.actual"
|> parse
|> travel
|> Global.tee "Path scores: "
|> List.min
|> printfn "Shortest path score: %d"

