open System
open System.IO

#load "../global.fsx"
#load "./common.fsx"

let patrolLength matrix =
    let mutable currentDirection = 8
    
    let rowCount= matrix |> Array2D.length1
    let colCount= matrix |> Array2D.length2

    let obstacles =
        matrix
        |> Global.matrixIndices
        |> Seq.filter (fun (row,col) -> matrix.[row,col] = '#' )

    let initialPosition = 
        matrix
        |> Global.matrixIndices
        |> Seq.find (fun (row,col) -> matrix.[row,col] = '^' )

    let mutable (currentRow, currentCol) = initialPosition        

    let mutable obstacleNotFound = false

    seq {
        while (not obstacleNotFound) do
            // printfn "row: %d col: %d dir: %d" currentRow currentCol currentDirection
            let patrol = 
                match currentDirection with
                | 8 -> 
                    currentDirection <- 6
                    obstacles 
                    |> Seq.filter (fun (row,col) -> row < currentRow && col = currentCol ) 
                    |> Common.tryMaxBy fst
                    |> function
                    | None ->   
                        obstacleNotFound <- true
                        [currentRow .. -1 .. 0]
                    | Some (row,col) ->                         
                        [currentRow.. -1 ..row+1] 
                    |> List.map (fun row -> row,currentCol)                

                | 2 -> 
                    currentDirection <- 4
                    obstacles 
                    |> Seq.filter (fun (row,col) -> row > currentRow && col = currentCol ) 
                    |> Common.tryMinBy fst
                    |> function
                    | None -> 
                        obstacleNotFound <- true
                        [currentRow .. rowCount-1]
                    | Some (row,col) -> 
                        [currentRow..row-1] 
                    |> List.map (fun row -> row,currentCol)

                | 4 ->
                    currentDirection <- 8
                    obstacles 
                    |> Seq.filter (fun (row,col) -> row = currentRow && col < currentCol ) 
                    |> Common.tryMaxBy snd
                    |> function
                    | None -> 
                        obstacleNotFound <- true
                        [currentCol .. -1 .. 0 ]
                    | Some (row,col) -> 
                        [currentCol.. -1 ..col+1] 
                    |> List.map (fun col -> currentRow,col)

                | 6 ->
                    currentDirection <- 2
                    obstacles 
                    |> Seq.filter (fun (row,col) -> row = currentRow && col > currentCol ) 
                    |> Common.tryMinBy snd
                    |> function 
                    | None ->
                        obstacleNotFound <- true
                        [currentCol .. colCount - 1]
                    | Some (row,col) ->
                        [currentCol..col-1] 
                    |> List.map (fun col -> currentRow,col)

                | _ -> failwithf "Unexplained direction %d" currentDirection
            
            let currentPosition = (patrol |> List.last)
            currentRow <- currentPosition |> fst
            currentCol <- currentPosition |> snd

            // printMatrix obstacles patrol (currentRow,currentCol) currentDirection (rowCount, colCount)

            yield! patrol
            
    }
    |> Seq.distinct
    |> Seq.length

"./input.example"
|> Common.parseData
|> patrolLength
|> Global.shouldBe 41

"./input.actual"
|> Common.parseData
|> patrolLength
|> printfn "The total length of the patrol is %A" 