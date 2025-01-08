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
        let left = filter '['  |> Set,'['
        let right = filter ']' |> Set,']'
    
        [left;right]
        
    Global.printMatrixBase matrix [initPosition; obstacles; yield! directedPath; yield! crates ]

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
            | _, '#' -> [|None|]
            | '#', _ -> [|None|]
            | ']',_ ->
                [|  movableBoxes matrix ((lrow + rowOffset,lcol-1),(lrow + rowOffset,lcol)) dir 
                    Some [|l;r|]|]
            | _,'[' ->
                [|  movableBoxes matrix ((rrow + rowOffset,rcol),(rrow + rowOffset,rcol+1)) dir 
                    Some [|l;r|]|]
            | '.','.' -> [| Some [|l;r|] |]
            | _  -> [|None|]

        if boxes |> Array.exists Option.isNone
            then None
            else boxes |> Array.choose id |> Array.concat |> Some
    

let applyInstructions (matrix: char array2d) (instructions: Direction array) =
    let state = Array2D.copy matrix
    let initPosition = matrix |> Global.matrixIndices |> Seq.find (fun (row,col)-> matrix.[row,col] = '@')
    let printInstructions index = 
        if index < instructions.Length
        then
            let start = Math.Max(0,index - 10)
            let length = Math.Min(20, instructions.Length - start)
            instructions 
            |> Array.mapi (fun i dir -> if i = index then $"[{dir}]" else string dir)
            |> Array.skip start
            |> Array.take length
            |> Array.reduce (sprintf "%s %s")
            |> printfn "Index: %d %s" index
        else printfn "%A" instructions

    let crateCount = 
        matrix 
        |> Global.matrixIndices 
        |> Seq.filter (fun (r,c) -> matrix.[r,c] = '[')
        |> Seq.length
    
    let validateState state =
        let mutable stateCrateCount = 0
        let mutable result = true
        let rows,cols = Global.matrixSize state
        
        for row in 0..rows-1 do
            for col in 0..cols-1 do
                let c = state.[row,col]
                try
                    if c = '['
                    then
                        stateCrateCount <- stateCrateCount + 1 
                        state.[row,col+1] |> Global.shouldBe ']'
                    if c = ']'
                    then state.[row,col-1] |> Global.shouldBe '['
                    if matrix.[row,col] = '#'
                    then c |> Global.shouldBe '#'
                with exn ->
                    printfn "Cells: %A = state: %c original: %c" (row,col) state.[row,col] matrix.[row,col]
                    printfn "Error: %s" exn.Message
                    result <- false
        
        stateCrateCount |> Global.shouldBe crateCount
        
        result

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

            if not (validateState state)
            then 
                printWh [position,dir] (initPosition,Up)
                printfn "Stopped at %d of %d" index instructions.Length
                printfn "Here it is %A %A" position dir
                exit 666

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
                // |> Global.tee "Nearby boxes"
                |> function
                | dir, Some boxes ->
                    if dir = Rt 
                    then
                        boxes
                        |> Array.rev
                    else boxes
                    |> Array.distinct
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
                    apply (index+1) (row,col)          
    
    apply 0 initPosition

let gps (matrix: char array2d) = 
    matrix
    |> Global.matrixIndices
    |> Seq.filter(fun (row,col) -> matrix.[row,col] = '[')
    |> Seq.sumBy(fun (row,col) -> row * 100 + col) 


let matrix = 
    "# # # # # # # # # # # # # #
 # # . . . . . . . . . . # #
 # # . . [ ] . . . . . . # #
 # # . . [ ] . . [ ] [ ] # #
 # # [ ] [ ] # # # # . . # #
 # # . . . [ ] . # # # # # #
 # # . . [ ] . . [ ] . . # #
 # # . . [ ] [ ] [ ] . . # #
 # # . . [ ] # # . . . . # #
 # # . . [ ] . . [ ] [ ] # #
 # # . . . [ ] . . . . . # #
 # # [ ] . . . . . . . . # #
 # # [ ] . @ # # . . . . # #
 # # . . . . [ ] . . . . # #
 # # # # # # # # # # # # # # "
    |> _.Split('\n')
    |> Array.map _.Trim().Split(' ')    
    |> array2D
    |> Array2D.map (fun str -> char str)

applyInstructions matrix [|Up;Up;Up;Up;Up|]

"input.test1"
|> parse
||> applyInstructions 
|> gps
|> printfn "Τhe sum of all boxes' final GPS coordinates is %d"

"input.test2"
|> parse
||> applyInstructions 
|> gps
|> printfn "Τhe sum of all boxes' final GPS coordinates is %d"

"input.example3"
|> parse
||> applyInstructions 
|> gps
|> Global.shouldBe 618

"input.example2"
|> parse
||> applyInstructions 
|> gps
|> Global.shouldBe 9021


"input.actual"
|> parse
||> applyInstructions 
|> gps
|> printfn "Τhe sum of all boxes' final GPS coordinates is %d"

