open System
open System.IO

#load "../global.fsx"

type Direction = Up | Rt | Dn | Lt

let parse path = 
    let lines = File.ReadAllLines path
    let matrix = 
        lines 
        |> Array.takeWhile (not<<String.IsNullOrEmpty)
        |> Array.map _.ToCharArray()
        |> Array.map (Array.collect (fun c ->
            match c with
            | '#' -> [|'#';'#'|]
            | 'O' -> [|'[';']'|]
            | '.' -> [|'.';'.'|]
            | '@' -> [|'@';'.'|]
            | x -> failwithf "unknown element %c" x
            ))
        |> array2D

    let instructions = 
        lines 
        |> Array.skipWhile (not<<String.IsNullOrEmpty)
        |> Array.skip 1
        |> Array.collect _.ToCharArray()
    
    matrix, instructions

// parse "input.example2"
// |> fst
// |> Global.printMatrix

let nextPosition (instruction: Direction) (pos: int*int) =
    let row,col = pos
    match instruction with
    | Up -> row-1, col
    | Rt -> row, col+1
    | Dn -> row+1, col
    | Lt -> row, col-1

let direction (from: int*int) (towards: int*int) = 
    let row1,col1 = from
    let row2, col2 = towards

    match row2-row1, col2-col1 with
    | r,c when r = 0 && c > 0 -> Rt
    | r,c when r = 0 && c < 0 -> Lt
    | r,c when r > 0 && c = 0 -> Dn
    | r,c when r < 0 && c = 0 -> Up
    | x -> failwithf "unforseen move from %A to %A (%A)" from towards x

type BoxResult = 
    | EncounteredWall
    | NoAdjacentBoxes
    | Movable of (int*int) list

[<RequireQualifiedAccess>]
module BoxResult = 
    let merge (left: BoxResult) (right: BoxResult) =
            match left,right with
            | EncounteredWall, _ -> EncounteredWall
            | _, EncounteredWall -> EncounteredWall
            | Movable boxes, Movable moreBoxes -> Movable (boxes@moreBoxes)
            | NoAdjacentBoxes, result -> result
            | result, NoAdjacentBoxes -> result

let rec connectedBoxes (boxPos: int*int) (direction: Direction) (boxesFound: (int*int) list) (matrix: char array2d) = 
    let boxRow, boxCol= boxPos 
    let adjacentCells = 
        match matrix.[boxRow,boxCol], direction with 
        | ']', Up -> 
            let r1, c1 = boxRow-1,boxCol-1
            let r2, c2 = boxRow-1,boxCol
            let additional = 
                [   if matrix.[r1,c1] = ']' then r1,c1-1
                    if matrix.[r2,c2] = '[' then r2,c2+1 ]
            [r1,c1;r2,c2; yield! additional]
        | '[', Up -> 
            let r1, c1 = boxRow-1,boxCol
            let r2, c2 = boxRow-1,boxCol+1
            let additional = 
                [   if matrix.[r1,c1] = ']' then r1,c1-1
                    if matrix.[r2,c2] = '[' then r2,c2+1 ]
            [r1,c1;r2,c2; yield! additional]
        | ']', Rt -> failwithf "World is in error, found broken box"
        | '[', Rt -> [boxRow,boxCol+1;boxRow,boxCol+2]
        | ']', Dn -> 
            let r1, c1 = boxRow+1,boxCol-1
            let r2, c2 = boxRow+1,boxCol
            let additional = 
                [   if matrix.[r1,c1] = ']' then r1,c1-1
                    if matrix.[r2,c2] = '[' then r2,c2+1 ]
            [r1,c1;r2,c2; yield! additional]
        | '[', Dn -> 
            let r1, c1 = boxRow+1,boxCol
            let r2, c2 = boxRow+1,boxCol+1
            let additional = 
                [   if matrix.[r1,c1] = ']' then r1,c1-1
                    if matrix.[r2,c2] = '[' then r2,c2+1 ]
            [r1,c1;r2,c2; yield! additional]
        | ']', Lt -> [boxRow,boxCol-2; boxRow,boxCol-1]
        | '[', Lt -> failwithf "Found broken box"
        |> List.map (fun (row,col) -> (row,col),matrix.[row,col])

    if adjacentCells |> List.exists (fun ((_,_),value) -> value = '#')
    then EncounteredWall
    else 
        let adjacentBoxes = 
            adjacentCells 
            |> List.filter (fun (_,value) -> value = '[' || value = ']')
    
        if adjacentBoxes.IsEmpty
        then
            if boxesFound.Length = 0
            then NoAdjacentBoxes
            else Movable boxesFound
        else 
            let updatedBoxesFound = 
                boxesFound 
                |> List.append (adjacentBoxes |> List.map fst)

            adjacentBoxes
            |> List.filter (fun (_,value) -> value = '[')
            |> List.fold (fun state (pos,value) -> 
                if state = EncounteredWall // same ax first matched rule, but here it prevents more recursion
                then EncounteredWall
                else 
                    connectedBoxes pos direction updatedBoxesFound matrix
                    |> BoxResult.merge state) NoAdjacentBoxes
     

let moveBoxes (position: int*int) (boxPos: int*int) (matrix : char array2d) =
    let rowCount, colCount = Global.matrixSize matrix
    let row,col = position
    let boxRow, boxCol= boxPos
    let dir = direction position boxPos

    let boxPosPair = 
        match matrix.[boxRow,boxCol] with
        | ']' -> boxRow, boxCol - 1
        | '[' -> boxRow, boxCol + 1

    let boxesToPush = connectedBoxes boxPos dir [boxPos;boxPosPair] matrix
        
        
    match boxesToPush with
    | BoxResult.EncounteredWall
        // match dir with 
        // | Up -> [boxRow-1,boxCol;boxRow-1,boxCol+1]
        // | Rt -> [boxRow,boxCol+1]
        // | Dn -> [boxRow-1,boxCol;boxRow-1,boxCol+1]
        // | Lt -> [boxRow,boxCol-1]


    let boxCount =
        match dir with 
        | Up -> matrix[0..boxRow,boxColLeft] |> Array.rev
        | Rt -> matrix[boxRow,boxColLeft..colCount-1]
        | Dn -> matrix[boxRow..rowCount-1,boxColLeft]
        | Lt -> matrix[boxRow,0..boxColLeft] |> Array.rev
        |> Array.takeWhile (fun x -> x = 'O')
        |> Array.length
    
    let afterRow,afterCol = 
        match dir with 
        | Up -> boxRow - boxCount, boxColLeft
        | Rt -> boxRow, boxColLeft + boxCount
        | Dn -> boxRow + boxCount, boxColLeft
        | Lt -> boxRow, boxColLeft - boxCount

    match matrix.[afterRow,afterCol] with
    | '.' -> 
        matrix.[afterRow,afterCol] <- 'O'
        matrix.[boxRow,boxColLeft] <- '@'
        matrix.[row,col] <- '.'
        boxRow,boxColLeft
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
    |> Seq.filter(fun (row,col) -> matrix.[row,col] = '[')
    |> Seq.fold(fun state (row,col) -> state + row * 100 + col) 0


// "input.example1"
// |> parse
// ||> applyInstructions 
// |> gps
// |> Global.shouldBe 2028

// "input.example2"
// |> parse
// ||> applyInstructions
// |> gps
// |> Global.shouldBe 10092

// "input.actual"
// |> parse
// ||> applyInstructions
// |> gps
// |> printfn "GPS sum is %d"