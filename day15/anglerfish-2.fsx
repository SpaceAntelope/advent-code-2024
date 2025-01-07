open System
open System.IO

#load "../global.fsx"

type Direction = 
    Up | Rt | Dn | Lt 
    with static member FromChar(c: char) = 
            match c with
            | '<' -> Lt
            | '>' -> Rt
            | '^' -> Up
            | 'v' -> Dn
            | dir -> failwithf "Unexpected direction %c" dir

type Point = int*int
type Cell = { Row: int; Col: int } with member x.AsPoint = x.Row,x.Col
type Box = Point*Point

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
        |> Array.map Direction.FromChar
    
    matrix, instructions

let rightFromLeftBoxPos (leftPos: int*int) =
    let row,col = leftPos
    row, col + 1

let leftFromRightBoxPos (leftPos: int*int) =
    let row,col = leftPos
    row, col - 1

let nextPosition (instruction: Direction) (pos: int*int) =
    let row,col = pos
    match instruction with
    | Up -> row-1, col
    | Rt -> row, col+1
    | Dn -> row+1, col
    | Lt -> row, col-1

let printWarehouse (matrix : char array2d) (path: (Point*Direction) seq) (init: (Point*Direction)) =  
    let indices = matrix |> Global.matrixIndices
    let filter c = indices |> Seq.filter (fun (row,col) -> matrix.[row,col] = c)
    // let initPos = indices |> Seq.find (fun (row,col) -> matrix.[row,col] = '@')
    let obstacles = filter '#' |> Set, '░'    
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
    let initPosition = 
        let pos,dir = init
        let c = 
            match dir with 
            | Up -> '↑'
            | Dn -> '↓'
            | Lt -> '←'
            | Rt -> '→'
        Set [pos],c

    let crates = 
        let left = filter '['  |> Set,'█'
        let right = filter ']' |> Set,'▉'
    
        [left;right]
        
    Global.printMatrixBase matrix [initPosition; obstacles; yield! directedPath; yield! crates ]


let rec isBoxMovable (matrix: char array2d) (dir : Direction) (boxPosLeft: Point) =
    let nextBoxLeft = nextPosition dir boxPosLeft
    let nextBoxRight = nextPosition dir (rightFromLeftBoxPos boxPosLeft)

    match dir, matrix.[fst nextBoxLeft, snd nextBoxLeft], matrix.[fst nextBoxRight, snd nextBoxRight] with
    | _, '.', '.' -> true
    | Lt, '.', _  -> true
    | Rt, _, '.' -> true

    | Lt, '#', _  -> false
    | Rt, _, '#' -> false
    | Up, '#', _ -> false
    | Up, _, '#' -> false
    | Dn, '#', _ -> false
    | Dn, _, '#' -> false

    | Lt, ']', _ -> isBoxMovable matrix dir (leftFromRightBoxPos (nextBoxLeft))
    | Rt, _, '[' -> isBoxMovable matrix dir nextBoxRight
    | Up, '[', ']' -> isBoxMovable matrix dir nextBoxLeft
    | Dn, '[', ']' -> isBoxMovable matrix dir nextBoxLeft
    | Up, ']', '[' -> isBoxMovable matrix dir (leftFromRightBoxPos (nextBoxLeft)) && isBoxMovable matrix dir nextBoxRight
    | Dn, ']', '[' -> isBoxMovable matrix dir (leftFromRightBoxPos (nextBoxLeft)) && isBoxMovable matrix dir nextBoxRight
    | Up, '.', '[' -> isBoxMovable matrix dir nextBoxRight
    | Dn, '.', '[' -> isBoxMovable matrix dir nextBoxRight
    | Up, ']', '.' -> isBoxMovable matrix dir (leftFromRightBoxPos (nextBoxLeft))
    | Dn, ']', '.' -> isBoxMovable matrix dir (leftFromRightBoxPos (nextBoxLeft))

    | dir', nextLeft, nextRight -> failwithf "Is Box Movable: Not sure what to do with %A %A = %c %A = %c" dir' nextBoxLeft nextLeft nextBoxRight nextRight

let rec moveBox (matrix: char array2d) (dir: Direction) (boxPosLeft: Point) =
    
        let nextLeftRow,nextLeftCol = nextPosition dir boxPosLeft
        let nextRightRow, nextRightCol  = nextPosition dir (rightFromLeftBoxPos boxPosLeft)
        
        match dir, matrix.[nextLeftRow, nextLeftCol], matrix.[nextRightRow, nextRightCol] with        
        | Lt, ']', _  -> moveBox matrix dir (nextLeftRow, nextLeftCol-1)
        | Rt, _, '[' -> moveBox matrix dir (nextRightRow, nextRightCol)
        | Up, '[',_ -> moveBox matrix dir (nextLeftRow-1, nextLeftCol)
        | Dn, '[',_ -> moveBox matrix dir (nextLeftRow+1, nextLeftCol)
        | Up, ']','.' -> moveBox matrix dir (nextLeftRow, nextLeftCol-1)
        | Dn, ']','.' -> moveBox matrix dir (nextLeftRow, nextLeftCol-1)
        | Up, '.','[' -> moveBox matrix dir (nextRightRow, nextRightCol)
        | Dn, '.','[' -> moveBox matrix dir (nextRightRow, nextRightCol)
        | Up, ']','[' -> 
                    moveBox matrix dir (nextLeftRow, nextLeftCol-1)
                    moveBox matrix dir (nextRightRow, nextRightCol)
        | Dn, ']','[' -> 
                    moveBox matrix dir (nextLeftRow, nextLeftCol-1)
                    moveBox matrix dir (nextRightRow, nextRightCol)

        | dir', nextLeft, nextRight -> failwithf "Move Box: Not sure what to do with %A %A = %c %A = %c" dir' (nextLeftRow, nextLeftCol) nextLeft (nextRightRow, nextRightCol) nextRight
        
        matrix.[nextLeftRow, nextLeftCol] <- '['
        matrix.[nextRightRow, nextRightCol] <- ']'
        match dir with 
        | Up | Dn -> 
            matrix.[fst boxPosLeft, snd boxPosLeft] <- '.'
            matrix.[fst boxPosLeft, snd boxPosLeft + 1] <- '.'
        | Rt -> matrix.[fst boxPosLeft, snd boxPosLeft] <- '.'
        | Lt -> matrix.[fst boxPosLeft, snd boxPosLeft + 1] <- '.'

        // caller should move robot

let applyInstructions (matrix: char array2d) (instructions: Direction array) =
    let state = Array2D.copy matrix
    let initPosition = matrix |> Global.matrixIndices |> Seq.find (fun (row,col)-> matrix.[row,col] = '@')
    
    let rec apply (index: int) (position: int*int) =

        let row,col = position
        if index = instructions.Length
        then state
        else
            let dir = instructions.[index]
            let nextRow, nextCol = nextPosition dir position

            if state.[nextRow,nextCol]='.'
            then 
                state.[row,col] <- '.'
                state.[nextRow,nextCol] <- '@'
                apply (index+1) (nextRow,nextCol)
            else if state.[nextRow,nextCol]='#'
            then apply (index+1) position
            else if isBoxMovable matrix dir (nextRow,nextCol)
            then
                match dir, state.[nextRow,nextCol] with
                // | _, '.' -> 
                //     state.[row,col] <- '.'
                //     state.[nextRow,nextCol] <- '@'
                //     apply (index+1) (nextRow,nextCol)
                | Rt, '[' -> 
                    moveBox state dir (nextRow,nextCol)
                    state.[row,col] <- '.'
                    state.[nextRow,nextCol] <- '@'
                    apply (index+1) (nextRow,nextCol)
                | Lt, ']' ->
                    moveBox state dir (nextRow,nextCol)
                    state.[row,col] <- '.'
                    state.[nextRow,nextCol] <- '@'
                    apply (index+1) (nextRow,nextCol)
                | Up, '[' 
                | Dn, '[' -> 
                    moveBox state dir (nextRow,nextCol)
                    state.[row,col] <- '.'
                    state.[nextRow,nextCol] <- '@'
                    apply (index+1) (nextRow,nextCol)
                | Up, ']' 
                | Dn, ']' -> 
                    moveBox state dir (nextRow,nextCol-1)
                    state.[row,col] <- '.'
                    state.[nextRow,nextCol] <- '@'
                    apply (index+1) (nextRow,nextCol)
                | _ -> apply (index+1) (row,col)
            else
                printWarehouse matrix [position,dir] (initPosition,Up)
                failwithf "Not sure what to do with %A at %A when facing %c." dir position matrix.[nextRow,nextCol] // apply (index+1) position
    
    apply 0 initPosition


exit 666
// type CellKind = Mover of Cell | Obstacle of Cll | Box of Cell*Cell | Empty

// type BigBox(matrix: char array2d, leftRow:int, leftCol:int) =
//     let left = { Row= leftRow; Col = leftCol }
//     let right = { Row = leftRow; Col = leftCol + 1 }

//     // static member Warehouse = Map<
//     member x.Next(dir: Direction) =
//         match dir with
//         | Up -> BigBox(matrix, left.Row-1, left.Col)
//         | Dn -> BigBox(matrix, left.Row+1, left.Col)
//         | Lt -> BigBox(matrix, left.Row  , left.Col-1)
//         | Rt -> BigBox(matrix, left.Row  , left.Col+1)

//     member x.IsMovable(dir: Direction) =
//         let lrow,lcol= nextPosition dir (left.ToTuple())
//         let rrow,rcol= nextPosition dir (right.ToTuple())

//         match matrix.[lrow,lcol],matrix.[rrow,rcol] with
//         | '#', _ 
//         | _, '#' -> false
//         let leftAdjacent = nextPosition dir (leftRow,leftCol)        
//         let row,col = leftAdjacent
                
        
        
//     member x.Move(dir : Direction) =
//         //



// parse "input.example2"
// |> fst
// |> Global.printMatrix



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
        
        
    // match boxesToPush with
    // | BoxResult.EncounteredWall
    //     // match dir with 
    //     // | Up -> [boxRow-1,boxCol;boxRow-1,boxCol+1]
    //     // | Rt -> [boxRow,boxCol+1]
    //     // | Dn -> [boxRow-1,boxCol;boxRow-1,boxCol+1]
    //     // | Lt -> [boxRow,boxCol-1]


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