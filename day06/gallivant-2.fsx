open System
open System.IO
open System.Text.RegularExpressions

#load "../global.fsx"
#load "./common.fsx"

module Main =
    let parseData path =
        path
        |> File.ReadAllLines
        |> Array.map _.ToCharArray()
        |> array2D

    type Path = (int*int) list

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
        |> List.filter (fun (row,col) -> matrix.[row,col] = '#' )

    // let matrix =
    //     "./input.exaple"
    //     |> Common.parseData
    
    // let rowCount= matrix |> Array2D.length1
    // let colCount= matrix |> Array2D.length2

    let nextDirection currentDir = 
        match currentDir with
        | 8 -> 6
        | 6 -> 2
        | 2 -> 4
        | 4 -> 8

    // let obstacles = getObstacles matrix

    let positionFromObstacle (row,col) (dir:int) =
        match dir with
        | 8 -> row+1,col
        | 2 -> row-1,col
        | 4 -> row,col+1
        | 6 -> row,col-1

    let isObstacle x = x = '#'

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

    let findDir (node1: int*int) (node2: int*int) =
        match (node1, node2) with
        | (r1,c1),(r2,c2) when c1 = c2 && r1 > r2 -> 8
        | (r1,c1),(r2,c2) when c1 = c2 && r1 < r2 -> 2
        | (r1,c1),(r2,c2) when c1 > c2 && r1 = r2 -> 4
        | (r1,c1),(r2,c2) when c1 < c2 && r1 = r2 -> 6
        | _ -> failwithf "%A -> %A: We're not doing diagonals at this time." node1 node2

    let findDirObs (o1 :int*int) (o2: int*int) =
        match o1,o2 with
        | (r1,c1),(r2,c2) when c2-c1 = 1 -> 2
        | (r1,c1),(r2,c2) when c1-c2 = 1 -> 8
        | (r1,c1),(r2,c2) when r2-r1 = 1 -> 6
        | (r1,c1),(r2,c2) when r1-r2 = 1 -> 4

    let obstaclePath matrix =
        let rowCount= matrix |> Array2D.length1
        let colCount= matrix |> Array2D.length2
    
        let mutable currentDirection = 8

        let obstacles =
            matrix
            |> Global.matrixIndices
            |> List.filter (fun (row,col) -> matrix.[row,col] = '#' )

        let initialPosition = 
            matrix
            |> Global.matrixIndices
            |> List.find (fun (row,col) -> matrix.[row,col] = '^' )

        List.unfold (fun ((currentRow, currentColumn), currentDirection) -> 
            match currentDirection with
            | 8 -> 
                obstacles 
                |> List.filter (fun (row,col) -> row < currentRow && col = currentColumn ) 
                |> Common.tryMaxBy fst
            | 2 -> 
                obstacles 
                |> List.filter (fun (row,col) -> row > currentRow && col = currentColumn ) 
                |> Common.tryMinBy fst            
            | 4 ->
                obstacles 
                |> List.filter (fun (row,col) -> row = currentRow && col < currentColumn ) 
                |> Common.tryMaxBy snd            
            | 6 ->
                obstacles 
                |> List.filter (fun (row,col) -> row = currentRow && col > currentColumn ) 
                |> Common.tryMinBy snd
            |> Option.map (fun obstacle -> 
                let nextPosition = positionFromObstacle obstacle currentDirection
                let nextDir = nextDirection currentDirection
                obstacle, (nextPosition,nextDir))                    
        ) (initialPosition, currentDirection)

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
    let main = 
        let matrix = 
            @"C:\Users\cernu\source\Repos\advent-code-2024\day06\input.example"
            |> Common.parseData
        
        let rowCount= matrix |> Array2D.length1
        let colCount= matrix |> Array2D.length2

        matrix
        |> obstaclePath
        |> fun obstacles ->
            let obstructions = 
                let i2d i = [6;2;4;8].[i % 4]
                [ for i in 2..obstacles.Length-1 do
                    let direction = i2d i
                    let o1 = obstacles.[i-2]
                    let o2 = obstacles.[i-1]
                    let o3 = obstacles.[i]
                    let o4 = 
                            if i < obstacles.Length - 1
                            then obstacles.[i+1]
                            else 
                                match direction with
                                | 8 -> -1, snd o3 + 1
                                | 6 -> fst o3 + 1, colCount
                                | 2 -> rowCount, snd o3 - 1
                                | 4 -> fst o3 - 1, -1
                    //let lastDir = findDirObs o2 o3
                    //let nextDir = nextDirection lastDir

                    printfn "%A - %d -> %A - %d -> %A - %d -> %A" o1 (i2d (i-2)) o2 (i2d (i-1)) o3 direction o4
                    Common.printMatrix obstacles.[i-2..i+1] [] [] (positionFromObstacle o3 (i2d (i-1))) direction (10,10) 
                    
                    match direction with
                    | 8 -> 
                        let row = fst o1 - 1
                        let col = snd o3 + 1
                        if row > (fst o4)
                        then Some (row,col)
                        else None
                    | 2 -> 
                        let row = fst o1 + 1
                        let col = snd o3 - 1
                        if row < (fst o4)
                        then Some (row,col)
                        else None
                    | 4 -> 
                        let row = fst o3 - 1
                        let col = snd o1 - 1
                        printfn "%d %A %A"  4 (row,col) o4
                        if col > (snd o4)
                        then Some (row,col)
                        else None
                    | 6 -> 
                        let row = fst o3 + 1
                        let col = snd o1 + 1
                        if col < (snd o4)
                        then Some (row,col)
                        else None
                    |> Option.map (fun (row,col) -> 
                        Common.printMatrix obstacles.[i-2..i+1] [row,col] [] (positionFromObstacle (row,col) direction) (i2d (i+1)) (10,10) 
                        (row,col)//let edges = obstacles.[i-2..i] 
                    )
                ] |> List.choose id
            
            Common.printMatrix obstacles obstructions [] (1,4) 8 (10,10) 

            obstructions
            
        // |> 
        // |> List.length 
        // |> Global.shouldBe 6