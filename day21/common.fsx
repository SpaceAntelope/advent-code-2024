open System
open System.IO
open System.Security.Cryptography

#load "../global.fsx"
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Text

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


let checkIfPathContains (index: IReadOnlyDictionary<char,(int * int)>) (start: char) (path: string) (contained: Point) =
    let row,col= index.[start]
    let updatePoint c (row,col) = 
        match c with
        | '^' -> row-1,col
        | 'v' -> row+1,col
        | '<' -> row,col-1
        | '>' -> row,col+1
    path.ToCharArray() 
    |> Seq.scan (fun state current -> updatePoint current state ) (row,col) 
    |> Seq.exists (fun point -> point = contained)


let mutable moveCache = Map.empty<(char*char*char), string>

let evalInstructions (instr: string) =
    if instr.Length = 1
    then 1L
    else
        let mutable consecutiveDirections = 0L
        instr 
        |> _.ToCharArray() 
        |> Array.pairwise 
        |> Array.sumBy(fun (x,y) -> 
            //  same buttons, no movement
            if x = y 
            then
                consecutiveDirections <- consecutiveDirections + 1L
            else consecutiveDirections <- 0L
            
            consecutiveDirections +
            // < is the furthest button to press, so if pressed first A is closer on next move
            if x='<' && y = 'v' then 1L
            else if x='<' && y = '^' then 1L
            else if x='v' && y = '>' then 1L
            else 0L)

let moveMulti (pad : char array2d) =
    let index = 
        pad
        |> Global.matrixIndices
        |> Seq.map (fun (row,col) -> pad.[row,col], (row,col))
        |> readOnlyDict

    let emptyPoint = index.['.']

    let filterPathsThatGoThroughEmptyPoint start path = checkIfPathContains index start path emptyPoint

    fun (startDigit: char) (lastDigit: char) ->
        let row1,col1= index[startDigit]
        let row2,col2= index[lastDigit]

        let rep c count= "".PadRight(count,c)

        let horizontalMovement = 
            if col1 > col2 then rep '<' (col1-col2)
            else rep '>' (col2-col1)
        
        let verticalMovement = 
            if row1 > row2 then rep '^' (row1-row2)
            else rep 'v' (row2-row1)

        // [| horizontalMovement + verticalMovement
        //    verticalMovement + horizontalMovement |]
        [|  for i in 0..horizontalMovement.Length-1 do
                horizontalMovement.Substring(0,i) + verticalMovement + horizontalMovement.Substring(i)
            for i in 0..verticalMovement.Length-1 do
                verticalMovement.Substring(0,i) + horizontalMovement + verticalMovement.Substring(i)
        |] 
        |> function
        | [||] -> "" |> Array.singleton
        | candidateInstructions  -> 
            candidateInstructions |> Array.filter ((filterPathsThatGoThroughEmptyPoint startDigit)>>not)
        |> Array.maxBy (evalInstructions)



let evalInstructionsButFilterForMinLength (instructions: string[]) =
    instructions
    |> Array.groupBy _.Length
    |> Array.minBy fst
    |> snd
    |> Array.maxBy (evalInstructions)



let mutable moveCacheHit = 0L
let mutable moveCacheMiss = 0L
let mutable encodeCacheHit = 0L
let mutable encodeCacheMiss = 0L
let encode' (pad: char array2d) (instructions: string) =
    let move = moveMulti pad
    let rec search (path: string) (depth: int) =
        if depth = instructions.Length
        then [|path|]
        else
            let prev  = if depth = 0 then 'A' else instructions.[depth-1]
            let next = instructions.[depth]

            let cacheKey = (pad.[0,0], prev, next)
            if moveCache.ContainsKey cacheKey |> not
            then 
                moveCacheMiss <- moveCacheMiss + 1L
                moveCache <- moveCache |> Map.add cacheKey (move prev next)
            else 
                moveCacheHit <- moveCacheHit + 1L

            [|moveCache.[cacheKey]|]
            //move prev instructions.[depth] 
            //|> Array.collect (fun nextInstr -> search (path + nextInstr + "A") (depth+1))
    
    search "" 0
    |> fun x-> printfn "Found %d paths for %s" x.Length instructions; x
    |> evalInstructionsButFilterForMinLength
    // |> Global.tee ""

// let mutable encodeCache = Map.empty<char*string*string,string>
//let mutable encodeCache = Map.empty<string,string>
let mutable encodeCache = Dictionary<string,string>()

let mutable initEncode = 0

let hash =
    let sha256 = SHA256.Create()
    
    fun (key: string) -> 
        Encoding.UTF8.GetBytes(key) 
        |> sha256.ComputeHash 
        |> Convert.ToHexString        
let encode (pad: char array2d)  =
    initEncode <- initEncode + 1
    printfn "encode initialized %d times" initEncode
    let mutable maxPathScore = 0L
    let move = moveMulti pad
    let rec search (path: string) (remainingInstructions: string) (lastButton: char) =
        if remainingInstructions = ""
        then
            // let evaluation = evalInstructions path
            // if evaluation > maxPathScore
            // then
            //     maxPathScore <- evaluation
                // printfn "Path length: %d" path.Length
                path
            // else ""
        else
            let prevBtn  = if path = "" then 'A' else lastButton// path.[path.Length-1] //ToCharArray() |> Array.last
            let nextBtn = remainingInstructions.[0]
            
            let nextInstruction =
                let moveCacheKey = (pad.[0,0], prevBtn, nextBtn)
                match moveCache |> Map.tryFind moveCacheKey with
                | Some x -> 
                    moveCacheHit <- moveCacheHit + 1L
                    x
                | None -> 
                    moveCacheMiss <- moveCacheMiss + 1L
                    moveCache <- moveCache |> Map.add moveCacheKey (move prevBtn nextBtn)                
                    moveCache.[moveCacheKey]

            let nextPath = path + nextInstruction + "A"
            let nextRemainingInstructions  = remainingInstructions.Substring(1)

            // let encodeCacheKey = sprintf "%c %s %s" pad.[0,0] nextPath (nextRemainingInstructions.Substring(Math.Min(nextRemainingInstructions.Length,50))) // |> hash
            // if encodeCache.ContainsKey encodeCacheKey |> not
            // then 
            //     encodeCacheMiss <- encodeCacheMiss + 1L
            //     encodeCache.Add(encodeCacheKey, (search nextPath nextRemainingInstructions nextBtn))
            //     // encodeCache <- encodeCache |> Map.add encodeCacheKey (search nextPath nextRemainingInstructions nextBtn)
            // else encodeCacheHit <- encodeCacheHit + 1L
            
            // encodeCache.[encodeCacheKey]
            search nextPath nextRemainingInstructions nextBtn
            
            //move prev instructions.[depth] 
            // |> Array.collect (fun nextInstr -> 
            //     let nextPath = path + nextInstr + "A"
            //     let nextInstr = remainingInstructions.Substring(1)
            //     let encodeCacheKey = pad.[0,0], nextPath, nextInstr

            //     if encodeCache.ContainsKey encodeCacheKey |> not
            //     then 
            //         encodeCacheMiss <- encodeCacheMiss + 1L
            //         encodeCache <- encodeCache |> Map.add encodeCacheKey (search nextPath nextInstr nextBtn)
            //     else encodeCacheHit <- encodeCacheHit + 1L
            //     encodeCache.[encodeCacheKey])
                // search nextPath nextInstr nextBtn)
    
    fun (instructions: string) -> 
        maxPathScore <- 0L
        search "" instructions '.'
        // |> fun x-> printfn "\tFound %d paths for %s" x.LongLength instructions; x
        //|> Array.maxBy evalInstructions
        // |> evalInstructionsButFilterForMinLength
    //|> Global.tee "Encoided: "

let calculateComplexity (code:string) (instructions:string) =
    let codeNum = int <| Regex.Match(code, "\d+").Value
    instructions.Length * codeNum
