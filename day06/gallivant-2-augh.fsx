open System
open System.IO
open System.Text.RegularExpressions

#load "../global.fsx"
#load "./common.fsx"

open Common
open Global
open System.Collections.Generic

type Dir = Up | Dn | Lt | Rt


// let printMatrix (matrix: char array2d) (obstacles : Path) (customObstacles: Path) (patrol: Point*Dir) 


//     (position: int*int) (direction: int) (size: int*int) = 
//     let (l1,l2) = size
//     Array2D.init l1 l2 (fun row col -> 
//         if obstacles |> List.contains (row,col) then '#'
//         else if (row,col)=position then 
//             match direction with 4 -> '<' | 8 -> '^' | 6 -> '>' | 2 -> 'v' | _ -> '_'
//         else if customObstacles |> List.contains (row,col) then 'O'
//         else if patrol |> List.contains (row,col) then 'X'
//         else '.')
//     |> sprintf "%A"
//     |> fun str -> Regex.Replace(str, @"['\[\];]", "")
//     |> printfn " %s\n"

// let parseData path =
//     path
//     |> File.ReadAllLines
//     |> Array.map _.ToCharArray()
//     |> array2D

// type Path = (Point) list

// let obstaclePath matrix =
//     let mutable currentDirection = 8

//     let rowCount= matrix |> Array2D.length1
//     let colCount= matrix |> Array2D.length2

//     let obstacles =
//         matrix
//         |> Global.matrixIndices
//         |> List.filter (fun (row,col) -> matrix.[row,col] = '#' )

//     let initialPosition = 
//         matrix
//         |> Global.matrixIndices
//         |> List.find (fun (row,col) -> matrix.[row,col] = '^' )

//     let mutable (currentRow, currentCol) = initialPosition      

//     let rec next (currentRow, currentCol)  (direction) =
//         let nextObstacle = 
//             match direction with 
//             | 8 -> obstacles |> List.filter (fun (row,col) -> col = currentCol && row < currentRow) |> Common.tryMaxBy fst
//             | 2 -> obstacles |> List.filter (fun (row,col) -> col = currentCol && row > currentRow) |> Common.tryMinBy fst
//             | 4 -> obstacles |> List.filter (fun (row,col) -> col < currentCol && row = currentRow) |> Common.tryMaxBy fst
//             | 6 -> obstacles |> List.filter (fun (row,col) -> col > currentCol && row = currentRow) |> Common.tryMinBy fst
    
//         let nextPosition =
//             match nextObstacle, direction with
//             | Some obs, dir -> positionFromObstacle obd dir
//             | None, 8 -> 0, col
//             | None, 6 -> rowCount-1, col
//             | None, 4 -> row, 0
//             | None, 6 -> row, colCount-1
        
//         let positionFromObstacle (row,col) (dir:int) =
//             match dir with
//             | 8 -> row+1,col
//             | 2 -> row-1,col
//             | 4 -> row,col+1
//             | 6 -> row,col-1

//         let nextDirection = 
//             match direction with
//             | 8 -> 6
//             | 6 -> 2
//             | 2 -> 4
//             | 4 -> 8

//         [ 

//         ]



let getObstacles (matrix : char array2d) =
    matrix
    |> Global.matrixIndices
    |> Seq.filter (fun (row,col) -> matrix.[row,col] = '#' )

// let matrix =
//     "./input.exaple"
//     |> Common.parseData

// let rowCount= matrix |> Array2D.length1
// let colCount= matrix |> Array2D.length2





let rotateRight currentDir = 
    match currentDir with
    | Up -> Rt
    | Rt -> Dn
    | Dn -> Lt
    | Lt -> Up
    
let rotateLeft currentDir = 
    match currentDir with
    | Up -> Lt
    | Rt -> Up
    | Dn -> Rt
    | Lt -> Dn
    

// let obstacles = getObstacles matrix

let positionWhenFacingObstacle (obsRow,obsCol) (dir:Dir) =
    match dir with
    | Up -> obsRow+1,obsCol
    | Dn -> obsRow-1,obsCol
    | Lt -> obsRow,obsCol+1
    | Rt -> obsRow,obsCol-1

let inline isObstacle x = x = '#'

// for (row,col) in obstacles do
//     for lookingDir in [8;6;2;4] do
//         let mutable generatedObstacleCounter = 0
//         let mutable (currentRow,currentCol) = positionFromObstacle (row,col) lookingDir
//         let headingDirs = 
//             Seq.unfold (fun dir -> 
//                 match nextDirection dir with
//                 | nextDir when nextDir = lookingDir -> None
//                 | nextDir -> Some (nextDir,nextDir)) lookingDir
//         for headingDir in headingDirs do
//             match headingDir with
//             | 8 -> matrix.[0.. currentRow-1, currentCol] |> Array.tryFindIndexBack isObstacle |> Option.map (fun rowIndex -> rowCount - rowIndex - 1, currentCol)
//             | 6 -> matrix.[currentRow+1..rowCount-1, currentCol] |> Array.tryFindIndex isObstacle |> Option.map (fun rowIndex -> rowIndex, currentCol)
//             | 4 -> matrix.[currentRow, 0..currentCol] |> Array.tryFindIndexBack isObstacle |> Option.map (fun colIndex -> currentRow, colCount - colIndex - 1)
//             | 6 -> matrix.[currentRow, currentCol.. colCount-1] |> Array.tryFindIndex isObstacle |> Option.map (fun colIndex -> currentRow, colIndex)
//             |> function 
//             | Some o -> 
//                 let (nextRow,nextCol) = positionFromObstacle o headingDir
//                 currentRow <- nextRow
//                 currentCol <- nextCol
//             | None -> 

// let findDir (node1: Point) (node2: Point) =
//     match (node1, node2) with
//     | (r1,c1),(r2,c2) when c1 = c2 && r1 > r2 -> 8
//     | (r1,c1),(r2,c2) when c1 = c2 && r1 < r2 -> 2
//     | (r1,c1),(r2,c2) when c1 > c2 && r1 = r2 -> 4
//     | (r1,c1),(r2,c2) when c1 < c2 && r1 = r2 -> 6
//     | _ -> failwithf "%A -> %A: We're not doing diagonals at this time." node1 node2

// let findDirObs (o1 :Point) (o2: Point) =
//     match o1,o2 with
//     | (r1,c1),(r2,c2) when c2-c1 = 1 -> 2
//     | (r1,c1),(r2,c2) when c1-c2 = 1 -> 8
//     | (r1,c1),(r2,c2) when r2-r1 = 1 -> 6
//     | (r1,c1),(r2,c2) when r1-r2 = 1 -> 4



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

let traceStraightPathUntilObstacleOrBoundary pos dir matrix : Path =    
    let size = Global.matrixSize matrix

    let rec trace current =
        let row ,col = current
        if not (Global.isOutOfBounds size current) && not (isObstacle matrix.[row,col])
        then [
            yield current
            yield! trace (next current dir)
        ]
        else []

    trace pos

let obstacleNeighborhood matrix obs=
    [   for dir in [Up;Rt;Dn;Lt] do
            let row,col = positionWhenFacingObstacle obs dir
            yield!                    
                traceStraightPathUntilObstacleOrBoundary (row,col) (reverseDir dir) matrix
                |> List.map (fun pathPoint -> (pathPoint,dir), obs   )   ]
let mutable indexCache = Map.empty<char[,], Map<Point*Dir,Point>>
let pos2obsIndex (matrix: char array2d)=
    match indexCache |> Map.tryFind matrix with
    | None -> 
        let idx =
            [   for obs in getObstacles matrix do
                    for dir in [Up;Rt;Dn;Lt] do
                        let row,col = positionWhenFacingObstacle obs dir
                        yield!                    
                            traceStraightPathUntilObstacleOrBoundary (row,col) (reverseDir dir) matrix
                            |> List.map (fun pathPoint -> (pathPoint,dir),obs   )   ]
            |> Map
        indexCache <- indexCache |> Map.add matrix idx
        idx
    | Some idx -> idx

let pathBetweenPoints ptA ptB = 
    let rowA,colA = ptA
    let rowB, colB = ptB

    match rowA - rowB, colA - colB with
    | rowDiff, 0 when rowDiff > 0 -> [rowA.. -1 ..rowB] |> List.map (fun row -> row, colA) 
    | rowDiff, 0 when rowDiff < 0 -> [rowA..rowB] |> List.map (fun row -> row, colA)
    | 0, colDiff when colDiff > 0 -> [colA.. -1 ..colB] |> List.map (fun col -> rowA, col)
    | 0, colDiff when colDiff < 0 -> [colA..colB] |> List.map (fun col -> rowA, col)
    | 0,0 -> []
    //| 0, 0 -> failwithf "Pointdiff %A indicates no movement which shouldn't ever happen." (0,0)
    | rowDiff,colDiff -> failwithf "Pointdiff %A indicates diagonal movement which is not allowed." (rowDiff,colDiff)
    // |> Global.tee $"%A{ptA} %A{ptB} {rowA - rowB} {colA - colB}"

let pathBetweenPointsExcludeLast ptA ptB =
    let path = pathBetweenPoints ptA ptB
    path.[..path.Length-2]


let tracePath matrix =
   
    let nextObstacleIndex = pos2obsIndex matrix
    
    let rec trace (currentPosition) (currentDirection) =
        match nextObstacleIndex |> Map.tryFind (currentPosition, currentDirection) with
        | None -> seq{ yield! traceStraightPathUntilObstacleOrBoundary currentPosition currentDirection matrix } |> Seq.map (fun x -> x,currentDirection)
        | Some obstacleAhead -> 
            let nextPosition = positionWhenFacingObstacle obstacleAhead currentDirection
            let nextDirection = rotateRight currentDirection
            let path = pathBetweenPointsExcludeLast currentPosition nextPosition |> Seq.map (fun x -> x, currentDirection)
            seq {   
                yield! path 
                yield! trace nextPosition nextDirection }
   
    fun pos dir -> trace pos dir

        // match cache |> Map.tryFind (initPos,initDir) with
        // | Some itIs -> itIs
        // | None ->
        //     let result = 
        //     cache <- cache |> Map.add (initPos,initDir) result
        //     result

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
// let printMaze (maze: char array2d) (path: Point seq) (obstacles: Point seq) (init: (Point*Dir) seq) =  
//     let (rows,cols) = Global.matrixSize maze
//     let obsSet = obstacles |>  Set.ofSeq
//     let pathSet = path |> Set.ofSeq
//     let initSet = init |> readOnlyDict
//     let printColIndex () = 
//         [|0..cols-1|] 
//         |> Array.map (sprintf "%03d") 
//         |> Array.map _.ToCharArray() 
//         |> Array.transpose 
//         |> Array.iter (fun digits-> 
//             printf "   "
//             digits |> Array.iter(printf " %c")
//             printfn ""
//         )

//     // let path' = path |> Array.ofSeq

    

//     printColIndex()
//     for row in 0..rows-1 do
//         printf "%03d " row
//         for col in 0..cols-1 do                    
//             if initSet.ContainsKey (row,col) 
//             then //'⦾' //⦿
//                 match initSet[row,col] with 
//                 | Up -> '▲'
//                 | Dn -> '▼'
//                 | Lt -> '◄'
//                 | Rt -> '►'
//             else if obsSet.Contains (row,col)
//             then '░'
//             else if pathSet.Contains(row,col)
//             then '▇'
//             else maze.[row,col]
//             |> printf "%c "
//         printf "%03d" row
//         if row < rows then printfn ""
//     printColIndex()
//     printfn ""
   
    

let isCycle matrix =
   
    let nextObstacleIndex = pos2obsIndex matrix
    let rows,cols = matrix |> Global.matrixSize
    // let mutable cache = Map.empty<Point*Dir,bool>

    fun initPos initDir -> 
        let mutable pathCounter = 0
        let newObstacle = next initPos (rotateLeft initDir)

        let updatedObstacleIndex = 
            obstacleNeighborhood matrix newObstacle 
            |> List.fold (fun state (cell,obs) -> state |> Map.add cell obs ) nextObstacleIndex

        let visited = HashSet<Point*Dir>()

        let x d s = 
            pathCounter <- pathCounter + (s |> List.length)
            s |> Seq.iter (fun pt -> visited.Add(pt,d) |> ignore)
            s
        // let mutable visited = Set.empty<Point*Dir>
        // visited.Add(initPos,initDir) |> ignore

        let rec trace (currentPosition) (currentDirection) =
            // printf "IsCycle %A %A " currentPosition currentDirection


            if pathCounter > rows*cols
            then
                printfn "Problem at cycle starting with %A %A" initPos initDir
                // printfn "visited: %d" visited.Count 
                // printMaze matrix visited [initPos,initDir] [newObstacle]
                //false 
                true
            else
                match updatedObstacleIndex |> Map.tryFind (currentPosition, currentDirection) with
                | None -> 
                    // printfn "None"
                    visited.Add(currentPosition,currentDirection) |> ignore
                    traceStraightPathUntilObstacleOrBoundary currentPosition currentDirection matrix
                    //not (visited.Add(currentPosition, rotateRight currentDirection)                    )
                    //&& traceStraightPathUntilObstacleOrBoundary currentPosition currentDirection matrix
                    |> x currentDirection
                    |> List.contains newObstacle
                    |> ignore
                    
                    false
                    // |> List.contains initPos
                    //|> List.exists (fun point -> not <| visited.Add(point, currentDirection))
                    
                | Some obstacleAhead -> 
                    // printf "Some "
                    let nextPosition = positionWhenFacingObstacle obstacleAhead currentDirection
                    let nextDirection = rotateRight currentDirection
                    // printfn "np: %A nd: %A" nextPosition nextDirection
                    let result =
                        if nextPosition = initPos && nextDirection = initDir
                        then true 
                        else 
                            pathBetweenPointsExcludeLast currentPosition nextPosition
                            |> x currentDirection
                            |> List.contains initPos
                            && (currentPosition <> initPos)
                            // |> List.exists (fun point -> not <| visited.Add(point, nextDirection))
                        // |> List.contains initPos 
                        // && currentDirection = (rotateLeft initDir)
                    
                    result || trace nextPosition nextDirection 
        
        let result = trace initPos initDir    
        // printfn "visited: %d -- %b" visited.Count result
        // if result 
        // then printMaze matrix visited [initPos,initDir] [newObstacle]

        result

let sanityTest filepath expected =
    let matrix= 
        filepath
        |> parseData

    let initialPosition = 
        matrix
        |> Global.matrixIndices
        |> Seq.find (fun (row,col) -> matrix.[row,col] = '^' )

    let pathTracer = tracePath matrix

    let path = pathTracer initialPosition Up
    
    printMaze matrix path [initialPosition,Up] []

    path
    |> List.ofSeq
    |> List.map fst
    |> List.distinct
    |> List.length
    |> Global.shouldBe expected

    printfn "%s pt1 test successful" filepath

sanityTest "./input.example" 41
sanityTest "./input.actual" 4758

let testSpecialCyclicalPaths() =
    let matrix= 
        "./input.actual"
        |> parseData

    let pathTracer = tracePath matrix
    let initPos = (49,56)
    let initDir = Up

    let path = 
        pathTracer initPos Up |> Seq.take 200
        |> Seq.append (pathTracer (34,111) Lt |> Seq.take 500)
        |> Seq.append (pathTracer (26,109) Dn |> Seq.take 500)
        |> Seq.append (pathTracer (56,90) Lt |> Seq.take 500)
        |> Seq.append (pathTracer (21,127) Dn |> Seq.take 500)

    [   (34,111), Lt
        (26,109), Dn
        (56,90), Lt
        (21,127), Dn ]
    |> printMaze matrix path 


let countPossibeCustomObstructions matrix =
    let mSize = Global.matrixSize matrix
    let nextObstacleIndex = pos2obsIndex matrix
    
    let hasObstacleToTheRight dir pos =         
        nextObstacleIndex |> Map.tryFind (pos, (rotateRight dir))
        // |> Option.map (fun _ -> pos)
        // |> Global.tee $"Obs> {dir} %A{pos}"

    let customObstacleWouldBeInPlaceOfExistingObstacle dir pos=
        let row,col = next pos dir 
        not (Global.isOutOfBounds mSize (row,col))
        && matrix.[row,col] |> isObstacle

    let cycleTracer = isCycle matrix    

    let countCycles dir (path: Path) =
        path
        |> List.filter (not<<(customObstacleWouldBeInPlaceOfExistingObstacle dir))
        |> List.filter ((hasObstacleToTheRight dir)>>Option.isSome) 
        |> List.filter (fun pos -> cycleTracer pos (rotateRight dir))
        |> List.map (fun pos -> next pos dir)

    let rec trace (currentPosition) (currentDirection) =
        // printfn  "%A -/- %A " currentPosition currentDirection
        match nextObstacleIndex |> Map.tryFind (currentPosition, currentDirection) with
        | None -> 
            // printfn "None"
            traceStraightPathUntilObstacleOrBoundary currentPosition currentDirection matrix
            |> countCycles currentDirection
        | Some obstacleAhead -> 
            let nextPosition = positionWhenFacingObstacle obstacleAhead currentDirection
            let nextDirection = rotateRight currentDirection
            // printf "Some %A %A " nextPosition nextDirection
            let currentPathCycles = 
                pathBetweenPointsExcludeLast currentPosition nextPosition
                |> countCycles currentDirection
            // printfn "%d" currentPathCycles
            currentPathCycles @ trace nextPosition nextDirection
   
    fun pos dir -> trace pos dir

// let x = pathTracer initialPosition Up

"./input.example"
|> Common.parseData
|> fun matrix -> 
    let initialPosition = 
        matrix
        |> Global.matrixIndices
        |> Seq.find (fun (row,col) -> matrix.[row,col] = '^' )
    
    // Common.printMatrix (getObstacles matrix |> List.ofSeq) [] [] initialPosition 8 (Global.matrixSize matrix)
    printMaze matrix [] [initialPosition, Up] []
    
    countPossibeCustomObstructions matrix initialPosition Up
    |> List.distinct
    |> List.length
|> Global.shouldBe 6

// exit 666

"./input.actual"
|> Common.parseData
|> fun matrix -> 
    let initialPosition = 
        matrix
        |> Global.matrixIndices
        |> Seq.find (fun (row,col) -> matrix.[row,col] = '^' )
    
    let obs = countPossibeCustomObstructions matrix initialPosition Up
    
    printMaze matrix [] [initialPosition, Up] obs
    
    obs
    |> List.distinct
    |> List.length
|> printfn "Found %d different positions you could choose for the obstruction."

// |> patrolLength
// |> Global.shouldBe 41

// "./input.actual"
// |> Common.parseData
// |> patrolLength
// |> printfn "The total length of the patrol is %A" 

// let findAlmostCyclicalPaths (matrix : char array2d) (obstacles: Path) =
//     let rowCount= matrix |> Array2D.length1
//     let colCount= matrix |> Array2D.length2
//     obstacles 
//     |> List.pairwise
//     |> List.filter (fun ((r1,c1), (r2,c2)) -> 
//         let dir = findDir (r1,c1) (r2,c2)
//         match dir with
//         | 8 -> 
//             let node3 = 
//                 matrix[r2+1, c2..colCount-1] 
//                 |> Array.tryFindIndex isObstacle 
//                 |> Option.map (fun colIndex -> r2+1, colIndex)
        
//             let node4 = 
//                 matrix[r1, c1..colCount-1] 
//                 |> Array.tryFindIndex isObstacle 
//                 |> Option.map (fun colIndex -> r1, colIndex)

//             node3, node4
//         | 2 -> 
//             let node3 = 
//                 matrix[r2, 0..c2-1] 
//                 |> Array.tryFindIndexBack isObstacle 
//                 |> Option.map (fun colIndex -> r2, colIndex)
        
//             let node4 = 
//                 matrix[r1, c1..colCount-1] 
//                 |> Array.tryFindIndex isObstacle 
//                 |> Option.map (fun colIndex -> r1, colIndex)
//         | 4 -> 
//             let node3 = 
//                 matrix[r2, 0..c2-1] 
//                 |> Array.tryFindIndexBack isObstacle 
//                 |> Option.map (fun colIndex -> r2, colIndex)
        
//             let node4 = 
//                 matrix[r1, c1..colCount-1] 
//                 |> Array.tryFindIndex isObstacle 
//                 |> Option.map (fun colIndex -> r1, colIndex)    
//             node3, node4
//         |> function
//         | Some _, _ -> true
//         | _, Some _ -> true
//         | _ -> false)
    // seq {
    //     while (not obstacleNotFound) do
    //         // printfn "row: %d col: %d dir: %d" currentRow currentCol currentDirection
    //         let patrol = 
    //             match currentDirection with
    //             | 8 -> 
    //                 currentDirection <- 6
    //                 obstacles 
    //                 |> List.filter (fun (row,col) -> row < currentRow && col = currentCol ) 
    //                 |> Common.tryMaxBy fst
    //                 |> function
    //                 | None ->   
    //                     obstacleNotFound <- true
    //                     [currentRow .. -1 .. 0]
    //                 | Some (row,col) ->                         
    //                     [currentRow.. -1 ..row+1] 
    //                 |> List.map (fun row -> row,currentCol)                

    //             | 2 -> 
    //                 currentDirection <- 4
    //                 obstacles 
    //                 |> List.filter (fun (row,col) -> row > currentRow && col = currentCol ) 
    //                 |> Common.tryMinBy fst
    //                 |> function
    //                 | None -> 
    //                     obstacleNotFound <- true
    //                     [currentRow .. rowCount-1]
    //                 | Some (row,col) -> 
    //                     [currentRow..row-1] 
    //                 |> List.map (fun row -> row,currentCol)

    //             | 4 ->
    //                 currentDirection <- 8
    //                 obstacles 
    //                 |> List.filter (fun (row,col) -> row = currentRow && col < currentCol ) 
    //                 |> Common.tryMaxBy snd
    //                 |> function
    //                 | None -> 
    //                     obstacleNotFound <- true
    //                     [currentCol .. -1 .. 0 ]
    //                 | Some (row,col) -> 
    //                     [currentCol.. -1 ..col+1] 
    //                 |> List.map (fun col -> currentRow,col)

    //             | 6 ->
    //                 currentDirection <- 2
    //                 obstacles 
    //                 |> List.filter (fun (row,col) -> row = currentRow && col > currentCol ) 
    //                 |> Common.tryMinBy snd
    //                 |> function 
    //                 | None ->
    //                     obstacleNotFound <- true
    //                     [currentCol .. colCount - 1]
    //                 | Some (row,col) ->
    //                     [currentCol..col-1] 
    //                 |> List.map (fun col -> currentRow,col)

    //             | _ -> failwithf "Unexplained direction %d" currentDirection
        
    //         let currentPosition = (patrol |> List.last)
    //         currentRow <- currentPosition |> fst
    //         currentCol <- currentPosition |> snd

    //         // printMatrix obstacles patrol (currentRow,currentCol) currentDirection (rowCount, colCount)

    //         yield! patrol
        
    // }
    // |> Seq.distinct
    // |> Seq.length

// let initialPosition = 
//     matrix
//     |> Global.matrixIndices
//     |> List.find (fun (row,col) -> matrix.[row,col] = '^' )

// if existing obstacle on the right, place obstacle ahead and follow path to see if it cycles back
// probably set a max path length

// let findCustomObstructionPositions (matrix : char array2d) =
//     let rowCount, colCount = Global.matrixSize matrix


// let main = 
//     let matrix = 
//         "./input.example"
//         |> parseData
    
//     let rowCount, colCount = Global.matrixSize matrix

//     matrix
//     |> obstaclePath
//     |> fun obstacles ->
//         let obstructions = 
//             let i2d i = [6;2;4;8].[i % 4]
//             [ for i in 2..obstacles.Length-1 do
//                 let direction = i2d i
//                 let o1 = obstacles.[i-2]
//                 let o2 = obstacles.[i-1]
//                 let o3 = obstacles.[i]
//                 let o4 = 
//                         if i < obstacles.Length - 1
//                         then obstacles.[i+1]
//                         else 
//                             match direction with
//                             | 8 -> -1, snd o3 + 1
//                             | 6 -> fst o3 + 1, colCount
//                             | 2 -> rowCount, snd o3 - 1
//                             | 4 -> fst o3 - 1, -1
//                 //let lastDir = findDirObs o2 o3
//                 //let nextDir = nextDirection lastDir

//                 printfn "%A - %d -> %A - %d -> %A - %d -> %A" o1 (i2d (i-2)) o2 (i2d (i-1)) o3 direction o4
//                 Common.printMatrix obstacles.[i-2..i+1] [] [] (positionWhenFacingObstacle o3 (i2d (i-1))) direction (10,10) 
                
//                 match direction with
//                 | 8 -> 
//                     let row = fst o1 - 1
//                     let col = snd o3 + 1
//                     if row > (fst o4)
//                     then Some (row,col)
//                     else None
//                 | 2 -> 
//                     let row = fst o1 + 1
//                     let col = snd o3 - 1
//                     if row < (fst o4)
//                     then Some (row,col)
//                     else None
//                 | 4 -> 
//                     let row = fst o3 - 1
//                     let col = snd o1 - 1
//                     printfn "%d %A %A"  4 (row,col) o4
//                     if col > (snd o4)
//                     then Some (row,col)
//                     else None
//                 | 6 -> 
//                     let row = fst o3 + 1
//                     let col = snd o1 + 1
//                     if col < (snd o4)
//                     then Some (row,col)
//                     else None
//                 |> Option.map (fun (row,col) -> 
//                     Common.printMatrix obstacles.[i-2..i+1] [row,col] [] (positionWhenFacingObstacle (row,col) direction) (i2d (i+1)) (10,10) 
//                     (row,col)//let edges = obstacles.[i-2..i] 
//                 )
//             ] |> List.choose id
        
//         Common.printMatrix obstacles obstructions [] (1,4) 8 (10,10) 

//         obstructions
        
//     // |> 
//     // |> List.length 
//     // |> Global.shouldBe 6