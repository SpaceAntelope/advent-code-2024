open System
open System.IO

#load "../global.fsx"

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


// let keys = "0123456789A".ToCharArray()
// keys 
// |> Array.allPairs keys 
// |> Array.map (fun (c1,c2) -> moveOnNumpad c1 c2)

// let keys2 = "<>^vA".ToCharArray()
// keys2
// |> Array.allPairs keys2
// |> Array.map (fun (c1,c2) -> moveOnDirPad c1 c2)
let dir2num (code: string) = 
    printfn "%s" code
    ("A" + code)
    |> _.ToCharArray() 
    |> Array.pairwise 
    |> Array.fold (fun state (a,b) -> 
        state + (moveOnNumpad a b) + "A") ""
    |> Global.tee ""
let dir2dir (dir: string) =
    ("A" + dir)
    |> _.ToCharArray() 
    |> Array.pairwise 
    |> Array.fold (fun state (a,b) -> 
        state 
        + (moveOnDirPad a b) + "A" 
        + (moveOnDirPad b 'A') + "A") ""
    |> Global.tee ""

[|
    "029A", "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A"
    "980A", "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A"
    "179A", "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"
    "456A", "<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A"
    "379A", "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"
|] 
|> Array.iter (
    fun (code, directions) ->
        printfn "Trying to type %s..." code
        code 
        |> dir2num
        |> dir2dir
        |> dir2dir
        |> _.Length
        |> Global.shouldBe directions.Length)

// "./input.example"
// |> File.ReadAllLines
// |> Seq.map (dir2num>>dir2dir>>dir2dir>>dir2dir)
// |> Seq.iter(printfn "%s")