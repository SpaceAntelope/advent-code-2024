open System
open System.IO

#load "../global.fsx"
#load "./common.fsx"

open Common
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Diagnostics

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

// type Point = int*int

// let numpad = 
//     [|
//         [|'7';'8';'9'|]
//         [|'4';'5';'6'|]
//         [|'1';'2';'3'|]
//         [|'.';'0';'A'|]
//     |] |> array2D

// let arrowpad =
//     [|
//         [|'.';'^';'A'|]
//         [|'<';'v';'>'|]
//     |] |> array2D

// let numpadIndex = 
//     numpad
//     |> Global.matrixIndices
//     |> Seq.map (fun (row,col) -> numpad.[row,col], (row,col))
//     |> readOnlyDict

// let dirpadIndex =
//     arrowpad
//     |> Global.matrixIndices
//     |> Seq.map (fun (row,col) -> arrowpad.[row,col], (row,col))
//     |> readOnlyDict

// let testMovement =
//     let moveArrowPad = moveMulti arrowpad
//     moveArrowPad 'A' '<'
//     |> Array.forall (fun path -> not <| path.StartsWith("<<"))
//     |> Global.shouldBe true

//     moveArrowPad '^' '<'
//     |> Array.forall (fun path -> not <| path.StartsWith("<"))
//     |> Global.shouldBe true



let rec nextA (current: Point) (instr: string) =
    // printfn "%A %s" current instr
    if instr = "" 
    then current
    else 
        let row,col = current
        match instr.[0] with
        | '^' -> row-1,col
        | 'v' -> row+1,col
        | '<' -> row,col-1
        | '>' -> row,col+1
        | x -> failwithf "%c is not an arrow" x
        |> fun next -> nextA next (instr.Substring(1))
    
let executeInstructions (pad : char array2d) (instructions: string) =
    let startPoint = 
        pad 
        |> Global.matrixIndices 
        |> Seq.find (fun (row,col) -> pad.[row,col] = 'A')
    
    instructions
    |> _.TrimEnd('A')
    |> _.Split('A')
    |> Array.fold (fun (cerrentPoint,state) current -> 
        let nextRow,nextCol = nextA cerrentPoint current
        (nextRow,nextCol), state + string pad.[nextRow, nextCol]) (startPoint,"")
    |> snd


"<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"
|> Global.tee ""
|> executeInstructions arrowpad
|> Global.tee ""
|> executeInstructions arrowpad
|> Global.tee ""
|> executeInstructions numpad
|> Global.tee ""
|> Global.shouldBe "379A"


let encodeNum = encode numpad
let encodeArr = encode arrowpad
let decode = encodeNum>>encodeArr>>encodeArr
//(encode numpad)>>(encode arrowpad)>>(encode arrowpad)


//executeInstructions numpad "<A<^A^^^AA" |> printfn "--- %A"

decode "029A" 
|> Global.tee "1"
|> executeInstructions arrowpad
|> Global.tee "2"
|> executeInstructions arrowpad
|> Global.tee "3"
|> executeInstructions numpad
|> Global.tee "4"

// exit 555

decode "379A"
|> _.Length
|> Global.shouldBe 64

encode arrowpad "<A>Av<<AA>^AA>AvAA^A<vAAA>^A"
|> _.Length
|> printfn "%d"

encode numpad "379A"
|> _.Length
|> printfn "%d"

executeInstructions numpad "^A<<^^A>>AvvvA"
|> printfn "%s"

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


initEncode <- 0

"./input.example"
|> File.ReadAllLines
|> Array.map(fun code -> code, decode code)
|> Array.sumBy(fun (code, instr) -> calculateComplexity code instr)
|> Global.shouldBe 126384

initEncode <- 0


let decode2 = 
    Array.create 2 (encode arrowpad)
    |> Array.reduce (>>)
    << (encode  numpad)

let eNum = encode numpad 
let eArr  = encode arrowpad 
let decode3 code =
    code 
    |> eNum
    |> eArr
    |> eArr
    |> eArr


"./input.actual"
|> File.ReadAllLines
|> Array.map (fun code -> 
    printf "Trying %s ... " code
    let sw = Stopwatch()
    sw.Start()
    let result = decode3 code
    sw.Stop()
    printfn "done in %A" sw.Elapsed
    code, result)
|> Array.sumBy(fun (code, instr) -> calculateComplexity code instr)
|> printfn "The sum of the complexities of the five codes on our list is %d"

printfn $"Move cache hit: {moveCacheHit}"
printfn $"Move cache miss: {moveCacheMiss}"
printfn $"Encode cache hit: {encodeCacheHit}"
printfn $"Encode cache miss: {encodeCacheMiss}"