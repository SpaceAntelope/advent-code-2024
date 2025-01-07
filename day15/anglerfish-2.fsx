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
        let left = filter '['  |> Set,'╠'
        let right = filter ']' |> Set,'╣'
    
        [left;right]
        
    Global.printMatrixBase matrix [initPosition; obstacles; yield! directedPath; yield! crates ]


// let rec isBoxMovable (matrix: char array2d) (dir : Direction) (boxPosLeft: Point) =
//     let nextBoxLeft = nextPosition dir boxPosLeft
//     let nextBoxRight = nextPosition dir (rightFromLeftBoxPos boxPosLeft)

//     match dir, matrix.[fst nextBoxLeft, snd nextBoxLeft], matrix.[fst nextBoxRight, snd nextBoxRight] with
//     | _, '.', '.' -> true
//     | Lt, '.', _  -> true
//     | Rt, _, '.' -> true

//     | Lt, '#', _  -> false
//     | Rt, _, '#' -> false
//     | Up, '#', _ -> false
//     | Up, _, '#' -> false
//     | Dn, '#', _ -> false
//     | Dn, _, '#' -> false

//     | Lt, ']', _ -> isBoxMovable matrix dir (leftFromRightBoxPos (nextBoxLeft))
//     | Rt, _, '[' -> isBoxMovable matrix dir nextBoxRight
//     | Up, '[', ']' -> isBoxMovable matrix dir nextBoxLeft
//     | Dn, '[', ']' -> isBoxMovable matrix dir nextBoxLeft
//     | Up, ']', '[' -> isBoxMovable matrix dir (leftFromRightBoxPos (nextBoxLeft)) && isBoxMovable matrix dir nextBoxRight
//     | Dn, ']', '[' -> isBoxMovable matrix dir (leftFromRightBoxPos (nextBoxLeft)) && isBoxMovable matrix dir nextBoxRight
//     | Up, '.', '[' -> isBoxMovable matrix dir nextBoxRight
//     | Dn, '.', '[' -> isBoxMovable matrix dir nextBoxRight
//     | Up, ']', '.' -> isBoxMovable matrix dir (leftFromRightBoxPos (nextBoxLeft))
//     | Dn, ']', '.' -> isBoxMovable matrix dir (leftFromRightBoxPos (nextBoxLeft))

//     | dir', nextLeft, nextRight -> failwithf "Is Box Movable: Not sure what to do with %A %A = %c %A = %c" dir' nextBoxLeft nextLeft nextBoxRight nextRight

// let rec moveBox (matrix: char array2d) (dir: Direction) (boxPosLeft: Point) =
    
//         let nextLeftRow,nextLeftCol = nextPosition dir boxPosLeft
//         let nextRightRow, nextRightCol  = nextPosition dir (rightFromLeftBoxPos boxPosLeft)
        
//         match dir, matrix.[nextLeftRow, nextLeftCol], matrix.[nextRightRow, nextRightCol] with        
//         | Lt, ']', _  -> moveBox matrix dir (nextLeftRow, nextLeftCol-1)
//         | Rt, _, '[' -> moveBox matrix dir (nextRightRow, nextRightCol)
//         | Up, '[',_ -> moveBox matrix dir (nextLeftRow-1, nextLeftCol)
//         | Dn, '[',_ -> moveBox matrix dir (nextLeftRow+1, nextLeftCol)
//         | Up, ']','.' -> moveBox matrix dir (nextLeftRow, nextLeftCol-1)
//         | Dn, ']','.' -> moveBox matrix dir (nextLeftRow, nextLeftCol-1)
//         | Up, '.','[' -> moveBox matrix dir (nextRightRow, nextRightCol)
//         | Dn, '.','[' -> moveBox matrix dir (nextRightRow, nextRightCol)
//         | Up, ']','[' -> 
//                     moveBox matrix dir (nextLeftRow, nextLeftCol-1)
//                     moveBox matrix dir (nextRightRow, nextRightCol)
//         | Dn, ']','[' -> 
//                     moveBox matrix dir (nextLeftRow, nextLeftCol-1)
//                     moveBox matrix dir (nextRightRow, nextRightCol)

//         | dir', nextLeft, nextRight -> failwithf "Move Box: Not sure what to do with %A %A = %c %A = %c" dir' (nextLeftRow, nextLeftCol) nextLeft (nextRightRow, nextRightCol) nextRight
        
//         matrix.[nextLeftRow, nextLeftCol] <- '['
//         matrix.[nextRightRow, nextRightCol] <- ']'
//         match dir with 
//         | Up | Dn -> 
//             matrix.[fst boxPosLeft, snd boxPosLeft] <- '.'
//             matrix.[fst boxPosLeft, snd boxPosLeft + 1] <- '.'
//         | Rt -> matrix.[fst boxPosLeft, snd boxPosLeft] <- '.'
//         | Lt -> matrix.[fst boxPosLeft, snd boxPosLeft + 1] <- '.'

//         // caller should move robot

let rec movableBoxes (matrix : char array2d) (boxPos: Point*Point) (dir: Direction) : Point array option =
    // let nextRow, nextCol = nextPosition dir pos
    let (lrow,lcol),(rrow,rcol) = boxPos
    match dir with 
    | Rt -> 
        let endIndex = 
            matrix.[lrow,lcol..] 
            |> Array.findIndex (fun c -> c <> '[' && c <> ']')
            |> fun offset -> lcol + offset 
        
        if matrix.[lrow,endIndex] = '.' then Some [| for col in lcol..endIndex-1 do lrow,col |]
        else None
    | Lt ->
        let endIndex = 
            matrix.[rrow,0..rcol]
            |> Array.findIndexBack (fun c -> c <> '[' && c <> ']')

        // matrix.[rrow,0..rcol] |> Array.fold (sprintf "%s%c") "" |> printfn "%s"
        // // printfn "Matched with %c %c"  matrix[lrow + rowOffset,lcol] matrix.[rrow+rowOffset,rcol]
        // printfn "HEY! %A %A %A" dir (rrow,rcol) endIndex

        if matrix.[rrow,endIndex] = '.' then Some [| for col in endIndex+1..rcol do rrow,col |]
        else None
    | dir -> 
        let rowOffset = if dir = Up then -1 else 1
        let l,r = boxPos
        let boxes = 
            match matrix[lrow + rowOffset,lcol],matrix.[rrow+rowOffset,rcol] with
            | '[', ']' -> 
                [|  movableBoxes matrix ((lrow + rowOffset,lcol),(rrow + rowOffset,rcol)) dir 
                    Some [|l;r|]|]                 
            | ']','[' ->
                [|  movableBoxes matrix ((lrow + rowOffset,lcol - 1),(lrow + rowOffset,lcol)) dir
                    movableBoxes matrix ((rrow + rowOffset,rcol),(rrow + rowOffset,rcol+1)) dir 
                    Some [|l;r|]|]
            | ']',_ ->
                [|  movableBoxes matrix ((lrow + rowOffset,lcol-1),(lrow + rowOffset,lcol)) dir 
                    Some [|l;r|]|]
            | _,'[' ->
                [|  movableBoxes matrix ((rrow + rowOffset,rcol),(rrow + rowOffset,rcol+1)) dir 
                    Some [|l;r|]|]
            | '.','.' -> [| Some [|lrow,lcol;rrow,rcol|] |]
            | _  -> [|None|]
            //|> Array.append [|Some[|l;r|]|]
        
        // matrix.[lrow,lcol..] |> Array.fold (sprintf "%s%c") "" |> printfn "%s"
        // printfn "Matched with %c %c"  matrix[lrow + rowOffset,lcol] matrix.[rrow+rowOffset,rcol]
        // printfn "HEY! %A %A %A" dir (lrow,lcol) boxes

        if boxes |> Array.exists Option.isNone
            then None
            else boxes |> Array.choose id |> Array.concat |> Some
    

let applyInstructions (matrix: char array2d) (instructions: Direction array) =
    let state = Array2D.copy matrix
    let initPosition = matrix |> Global.matrixIndices |> Seq.find (fun (row,col)-> matrix.[row,col] = '@')
    let printInstructions index = 
        if index < instructions.Length
        then
            instructions 
            |> Array.mapi (fun i dir -> if i = index then $"[{dir}]" else string dir)
            |> Array.skip (Math.Min(0,(index - 7)))
            |> Array.take (Math.Min(15, instructions.Length))
            |> Array.reduce (sprintf "%s %s")
            |> printfn "Index: %d %s" index
        else printfn "%A" instructions

    let rec apply (index: int) (position: int*int) =
        let printWh path init = 
            printWarehouse state path init
            printInstructions index                
            // Console.ReadLine() |> ignore

        let row,col = position

        if index = instructions.Length
        then 
            printWh [position,instructions.[index-1]] (initPosition,Up)
            state
        else
            let dir = instructions.[index]
            let nextRow, nextCol = nextPosition dir position

            // printWh [position,dir] (initPosition,Up)

            if state.[nextRow,nextCol]='.'
            then 
                state.[row,col] <- '.'
                state.[nextRow,nextCol] <- '@'
                apply (index+1) (nextRow,nextCol)
            else if state.[nextRow,nextCol]='#'
            then apply (index+1) position
            else 
                let box =
                    match state.[nextRow,nextCol] with
                    | '[' -> (nextRow,nextCol),(nextRow,nextCol+1)
                    | ']' -> (nextRow,nextCol-1),(nextRow,nextCol)
                    | c -> failwithf $"Not a box at %A{(nextRow,nextCol)} = %c{c}"
                                 
                (dir, movableBoxes state box dir)
                //|> Global.tee "Nearby boxes"
                |> function
                | dir, Some boxes ->
                    if dir = Rt 
                    then
                        boxes
                        |> Array.rev
                    else boxes
                    |> Array.iter (fun (row,col) -> 
                        match dir with
                        | Up -> state.[row-1,col] <- state.[row,col]
                        | Dn -> state.[row+1,col] <- state.[row,col]
                        | Lt -> state.[row,col-1] <- state.[row,col]
                        | Rt -> state.[row,col+1] <- state.[row,col]
                        state.[row,col] <- '.' )

                    state.[row,col] <- '.'
                    state.[nextRow,nextCol] <- '@'
                    apply (index+1) (nextRow,nextCol)        
                | _, None -> 
                    // state.[row,col] <- '.'
                    // state.[nextRow,nextCol] <- '@'
                    apply (index+1) (row,col)          
    
    apply 0 initPosition

let gps (matrix: char array2d) = 
    matrix
    |> Global.matrixIndices
    |> Seq.filter(fun (row,col) -> matrix.[row,col] = '[')
    |> Seq.sumBy(fun (row,col) -> row * 100 + col) 



"input.example2"
|> parse
||> applyInstructions 
|> gps
|> Global.shouldBe 9021

"input.test1"
|> parse
||> applyInstructions 
|> gps
|> printfn "Τhe sum of all boxes' final GPS coordinates is %d"

"input.actual"
|> parse
||> applyInstructions 
|> gps
|> printfn "Τhe sum of all boxes' final GPS coordinates is %d"

