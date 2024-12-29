open System
open System.IO

#load "../global.fsx"
open System.Text.RegularExpressions
open System.Collections.Generic

(*
+---+---+---+
| 7 | 8 | 9 |
+---+---+---+
| 4 | 5 | 6 |
+---+---+---+
| 1 | 2 | 3 |
+---+---+---+
    | 0 | A |
    +---+---+

    +---+---+
    | ^ | A |
+---+---+---+
| < | v | > |
+---+---+---+    
*)

type Point = int*int

let numpad = 
    [|
        [|'7';'8';'9'|]
        [|'4';'5';'6'|]
        [|'1';'2';'3'|]
        [|'.';'0';'A'|]
    |] |> array2D

let arrowpad =
    [|
        [|'.';'^';'A'|]
        [|'<';'v';'>'|]
    |] |> array2D

let numpadIndex = 
    numpad
    |> Global.matrixIndices
    |> Seq.map (fun (row,col) -> numpad.[row,col], (row,col))
    |> readOnlyDict

let dirpadIndex =
    arrowpad
    |> Global.matrixIndices
    |> Seq.map (fun (row,col) -> arrowpad.[row,col], (row,col))
    |> readOnlyDict


let moveOnNumpad (startDigit: char) (lastDigit: char) =
    // printf "%c -> %c " startDigit lastDigit
    let row1,col1= numpadIndex[startDigit]
    let row2,col2= numpadIndex[lastDigit]

    let rep c count= "".PadRight(count,c)

    let horizontalMovement = 
        if col1 > col2 then rep '<' (col1-col2)
        else rep '>' (col2-col1)
    
    let verticalMovement = 
        if row1 > row2 then rep '^' (row1-row2)
        else rep 'v' (row2-row1)

    if row2 = 3 && col1 = 0 
    then horizontalMovement + verticalMovement    
    else if row1 = 3 && col2 = 0
    then verticalMovement + horizontalMovement
    else horizontalMovement + verticalMovement
    // |> fun x ->printfn "%s" x ; x

let checkIfPathContains (index: IReadOnlyDictionary<char,(int * int)>) (start: char) (path: string) (contained: Point) =
    let row,col= index.[start]
    let updatePoint c (row,col) = 
        match c with
        | '^' -> row-1,col
        | 'v' -> row+1,col
        | '<' -> row,col-1
        | '>' -> row,col+1
        | x -> failwithf "%c is not a valid arrow button." x
    path.ToCharArray() 
    |> Seq.scan (fun state current -> updatePoint current state ) (row,col) 
    |> Seq.exists (fun point -> point = contained)

let checkIfNumPadPathGoesThroughEmpty  (start: char) (path:string) =
    checkIfPathContains numpadIndex start path (3,0)
    
let checkIfDirPadPathGoesThroughEmpty (start: char) (path:string) =
    checkIfPathContains dirpadIndex start path (0,0)

let moveOnNumpadMulti (startDigit: char) (lastDigit: char) =
    // printf "%c -> %c " startDigit lastDigit
    let row1,col1= numpadIndex[startDigit]
    let row2,col2= numpadIndex[lastDigit]

    let rep c count= "".PadRight(count,c)

    let horizontalMovement = 
        if col1 > col2 then rep '<' (col1-col2)
        else rep '>' (col2-col1)
    
    let verticalMovement = 
        if row1 > row2 then rep '^' (row1-row2)
        else rep 'v' (row2-row1)

    [|  for i in 0..horizontalMovement.Length-1 do
            horizontalMovement.Substring(0,i) + verticalMovement + horizontalMovement.Substring(i)
        for i in 0..verticalMovement.Length-1 do
            verticalMovement.Substring(0,i) + horizontalMovement + verticalMovement.Substring(i)
    |] 
    |> function
    | [||] -> "" |> Array.singleton
    | candidateInstructions  -> candidateInstructions |> Array.filter ((checkIfNumPadPathGoesThroughEmpty startDigit)>>not)


let moveOnDirPad (startDigit: char) (lastDigit : char) =
    // printf "%c -> %c " startDigit lastDigit
    let row1,col1= dirpadIndex[startDigit]
    let row2,col2= dirpadIndex[lastDigit]

    let rep c count= "".PadRight(count,c)

    let horizontalMovement = 
        if col1 > col2 then rep '<' (col1-col2)
        else rep '>' (col2-col1)
    
    let verticalMovement = 
        if row1 > row2 then rep '^' (row1-row2)
        else rep 'v' (row2-row1)
    
    if row2 = 1 && col2 = 0 
    then verticalMovement + horizontalMovement    
    else horizontalMovement + verticalMovement
    // |> fun x ->printfn "%s" x ; x

let moveOnDirPadMulti (startDigit: char) (lastDigit : char) =
    // printfn "%c -> %c " startDigit lastDigit
    let row1,col1= dirpadIndex[startDigit]
    let row2,col2= dirpadIndex[lastDigit]

    let rep c count= "".PadRight(count,c)

    let horizontalMovement = 
        if col1 > col2 then rep '<' (col1-col2)
        else rep '>' (col2-col1)
    
    let verticalMovement = 
        if row1 > row2 then rep '^' (row1-row2)
        else rep 'v' (row2-row1)
    
    [|  for i in 0..horizontalMovement.Length-1 do
            horizontalMovement.Substring(0,i) + verticalMovement + horizontalMovement.Substring(i)
        for i in 0..verticalMovement.Length-1 do
            verticalMovement.Substring(0,i) + horizontalMovement + verticalMovement.Substring(i)
    |] 
    |> function
    | [||] -> "" |> Array.singleton
    | candidateInstructions  -> candidateInstructions |> Array.filter ((checkIfDirPadPathGoesThroughEmpty startDigit)>>not)
    // |> Global.tee ""

moveOnDirPadMulti 'A' '<'
|> Array.forall (fun path -> not <| path.StartsWith("<<"))
|> Global.shouldBe true

moveOnDirPadMulti '^' '<'
|> Array.forall (fun path -> not <| path.StartsWith("<"))
|> Global.shouldBe true

let evalInstructions (instr: string) =
    instr |> _.ToCharArray() |> Array.pairwise |> Array.sumBy(fun (x,y) -> if x = y then 1 else 0) //|> fun x -> x / instr.Length

let evalInstructionsButFilterForMinLength (instructions: string[]) =
    instructions
    |> Array.groupBy _.Length
    |> Array.minBy fst
    |> snd
    |> Array.maxBy (evalInstructions)

let dir2numMulti (code: string)  =
    let rec search (path: string) (depth: int) =
        if depth = code.Length
        then [|path|]
        else
            let prev  = if depth = 0 then 'A' else code.[depth-1]
            moveOnNumpadMulti prev code.[depth] 
            |> Array.collect (fun nextInstr -> search (path + nextInstr + "A") (depth+1))
    
    search "" 0
    // |> Array.maxBy (evalInstructions)
    |> evalInstructionsButFilterForMinLength
    |> Global.tee ""

let dir2dirMulti (directions: string)  =

    let rec search (_path: string list) (depth: int) =
        let path = String.concat "" _path
        let __path = String.concat " | " _path
        
        // if path.Length > 1 && depth < directions.Length
        // then printfn "\t%d %s[%c -> %c]%s %s" depth (directions.Substring(0,depth-1)) (directions.[depth-1]) (directions.[depth]) (directions.Substring(depth+1)) __path
        // else printfn "\t%d %s[A -> %c]%s" depth __path (directions.[0]) (directions.Substring(1))
        
        if depth = directions.Length
        then
            // printfn "[%s] %d %d %s" directions (path.Length) (evalInstructions path) path
            [|path|]
        else
            let prev  = if depth = 0 then 'A' else directions.[depth-1]
            moveOnDirPadMulti prev directions.[depth] 
            |> Array.collect (fun nextInstr -> search (_path@[nextInstr + "A"]) (depth+1))
    
    search [] 0
    // |> Array.maxBy (evalInstructions)
    |> evalInstructionsButFilterForMinLength
    |> Global.tee ""

// dir2numMulti "029A"
// dir2dirMulti "<A^A^^>AvvvA"

let rec nextA (current: Point) (instr: string) (depth: int) =
    let row,col = current
    match instr.[depth] with
    | '^' -> row-1,col
    | 'v' -> row+1,col
    | '<' -> row,col-1
    | '>' -> row,col+1
    | x -> failwithf "%c is not an arrow" x
    |> function
    | (3,2) -> current, depth
    | next -> nextA next instr (depth+1)
    

        // printfn "%c" numpad.[row,col]
let rec reverseNum (instr: string) (current: Point) (depth: int) =
    printfn "%d %A %c %c" depth current numpad.[fst current, snd current] instr.[depth]
    if depth < instr.Length-1
    then 
        let nextButtonPressedPoint,nextDepth = nextA current instr depth
        let nextButtonPressed = numpad[fst nextButtonPressedPoint, snd nextButtonPressedPoint]

                    //| 'A' -> 3,2 // A
        string nextButtonPressed + reverseNum instr (3,2) (nextDepth+1)
    else ""


// reverseNum "<A^A^^>AvvvA" (3,2) 0
// |> printfn "%A"

let rec reverseDir (instr: string) (current: Point) (depth: int)=
    let row,col= current
    let updatePoint c (row,col) = 
        match instr.[depth] with
        | '^' -> row-1,col
        | 'v' -> row+1,col
        | '<' -> row,col-1
        | '>' -> row,col+1
        | _ -> row,col // A
    
    arrowpad.[row,col] + reverseDir instr (row,col) (depth+1)


// exit 666

let dir2num (code: string) = 
    printfn "%s" code
    ("A" + code)
    |> _.ToCharArray() 
    |> Array.pairwise 
    |> Array.fold (fun state (a,b) -> 
        state + (moveOnNumpad a b) + "A") ""
    |> Global.tee ""
let dir2dir (dir: string) =
    dir
    |> _.ToCharArray() 
    |> Array.fold (fun (state: string,lastTargetDigit: char) (nextTargetDigit: char) -> 
       ( state + (moveOnDirPad lastTargetDigit nextTargetDigit) + "A"), nextTargetDigit) ("",'A')
    |> fst
    |> Global.tee ""

let calculateComplexity (code:string) (instructions:string) =
    let codeNum = int <| Regex.Match(code, "\d+").Value
    instructions.Length * codeNum

let decode = dir2num>>dir2dir>>dir2dir
let decodeMulti = dir2numMulti>>dir2dirMulti>>dir2dirMulti


// dir2dirMulti "<A^A>^^AvvvA"
// moveOnDirPadMulti 'A' '^' |> Array.iter (printfn "%A")

// exit 666

[|
    "029A", "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A",  68*29
    "980A", "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A", 60*980
    "179A", "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A", 68*179
    "456A", "<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A", 64*456
    "379A", "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A", 64*379
|] 
|> Array.iter (
    fun (code, directions, complexity) ->
        printfn "Trying to type %s..." code
        code 
        |> decode
        |> fun actualDir -> 
            actualDir.Length
            |> Global.shouldBe directions.Length 
            
            calculateComplexity code actualDir
            |> Global.shouldBe complexity
        )

"./input.example"
|> File.ReadAllLines
|> Array.map(fun code -> code, decode code)
|> Array.sumBy(fun (code, instr) -> calculateComplexity code instr)
|> Global.shouldBe 126384

"./input.actual"
|> File.ReadAllLines
|> Array.map (fun code -> code, decode code)
|> Array.sumBy(fun (code, instr) -> calculateComplexity code instr)
|> printfn "The sum of the complexities of the five codes on our list is %d"

[|
    "029A", "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A",  68*29
    "980A", "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A", 60*980
    "179A", "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A", 68*179
    "456A", "<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A", 64*456
  //  "379A", "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A", 64*379
|] 
|> Array.iter (
    fun (code, directions, complexity) ->
        printfn "Trying to type %s..." code
        code 
        |> decodeMulti
        |> fun actualDir -> 
            actualDir.Length
            |> Global.shouldBe directions.Length 
           
            calculateComplexity code actualDir
            |> Global.shouldBe complexity
        )

"./input.example"
|> File.ReadAllLines
|> Array.map(fun code -> code, decodeMulti code)
|> Array.sumBy(fun (code, instr) -> calculateComplexity code instr)
// |> Global.shouldBe 126384

"./input.actual"
|> File.ReadAllLines
|> Array.map (fun code -> code, decodeMulti code)
|> Array.sumBy(fun (code, instr) -> calculateComplexity code instr)
|> printfn "The sum of the complexities of the five codes on our list is %d"



    // path.ToCharArray() 
    // |> Seq.scan (fun state current -> updatePoint current state ) (row,col) 
    // |> Seq.exists (fun point -> point = contained)    
