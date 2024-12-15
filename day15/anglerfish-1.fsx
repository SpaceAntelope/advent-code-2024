open System
open System.IO

#load "../global.fsx"


let parse path = 
    let lines = File.ReadAllLines path
    let matrix = 
        lines 
        |> Array.takeWhile (not<<String.IsNullOrEmpty)
        |> Array.map _.ToCharArray()
        |> array2D

    let instructions = 
        lines 
        |> Array.skipWhile (not<<String.IsNullOrEmpty)
        |> Array.skip 1
        |> Array.collect _.ToCharArray()
    
    matrix, instructions

let nextPosition (instruction: char) (pos: int*int) =
    let row,col = pos
    match instruction with
    | '^' -> row-1, col
    | '>' -> row, col+1
    | 'v' -> row+1, col
    | '<' -> row, col-1
    | _ -> failwithf "unknown instruction %c" instruction

let direction (from: int*int) (towards: int*int) = 
    let row1,col1 = from
    let row2, col2 = towards

    match row2-row1, col2-col1 with
    | r,c when r = 0 && c > 0 -> '>'
    | r,c when r = 0 && c < 0 -> '<'
    | r,c when r > 0 && c = 0 -> 'v'
    | r,c when r < 0 && c = 0 -> '^'
    | x -> failwithf "unforseen move from %A to %A (%A)" from towards x

let moveBoxes (position: int*int) (boxPos: int*int) (matrix : char array2d) =
    let rowCount, colCount = Global.matrixSize matrix
    let row,col = position
    let boxRow, boxCol= boxPos
    let dir = direction position boxPos
    let boxCount =
        match dir with 
        | '^' -> matrix[0..boxRow,boxCol] |> Array.rev
        | '>' -> matrix[boxRow,boxCol..colCount-1]
        | 'v' -> matrix[boxRow..rowCount-1,boxCol]
        | '<' -> matrix[boxRow,0..boxCol] |> Array.rev
        | x -> failwithf "unknown direction %c" x
        |> Array.takeWhile (fun x -> x = 'O')
        |> Array.length
    
    let afterRow,afterCol = 
        match dir with 
        | '^' -> boxRow - boxCount, boxCol
        | '>' -> boxRow, boxCol + boxCount
        | 'v' -> boxRow + boxCount, boxCol
        | '<' -> boxRow, boxCol - boxCount
        | x -> failwithf "unknown direction %c" x

    match matrix.[afterRow,afterCol] with
    | '.' -> 
        matrix.[afterRow,afterCol] <- 'O'
        matrix.[boxRow,boxCol] <- '@'
        matrix.[row,col] <- '.'
        boxRow,boxCol
    | _ -> row,col



let applyInstructions (matrix: char array2d) (instructions: char array) =
    let state = Array2D.copy matrix
    let initPosition = matrix |> Global.matrixIndices |> Seq.find (fun (row,col)-> matrix.[row,col] = '@')
    
    let rec apply (index: int) (position: int*int) =

        let row,col = position
        if index = instructions.Length
        then state
        else
            let nextRow, nextCol = nextPosition  instructions.[index] position

            match state.[nextRow,nextCol] with
            | '.' -> 
                state.[row,col] <- '.'
                state.[nextRow,nextCol] <- '@'
                apply (index+1) (nextRow,nextCol)
            | 'O' -> 
                let nRow,nCol = moveBoxes position (nextRow, nextCol) state
                apply (index+1) (nRow,nCol)
            | _ -> apply (index+1) (row,col)

    apply 0 initPosition

let gps (matrix: char array2d) = 
    matrix
    |> Global.matrixIndices
    |> Seq.filter(fun (row,col) -> matrix.[row,col] = 'O')
    |> Seq.fold(fun state (row,col) -> state + row * 100 + col) 0


"input.example1"
|> parse
||> applyInstructions 
|> gps
|> Global.shouldBe 2028

"input.example2"
|> parse
||> applyInstructions
|> gps
|> Global.shouldBe 10092

"input.actual"
|> parse
||> applyInstructions
|> gps
|> printfn "GPS sum is %d"