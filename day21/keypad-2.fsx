open System
open System.IO

#load "../global.fsx"
#load "./common.fsx"

open System.Text.RegularExpressions
open System.Collections.Generic

let checkIfPathContains (index: IReadOnlyDictionary<char,(int * int)>) (start: char) (path: char list) (contained: Common.Point) =
    match path with 
    | [] -> false
    | path ->
        let row,col= index.[start]
        let updatePoint c (row,col) = 
            match c with
            | '^' -> row-1,col
            | 'v' -> row+1,col
            | '<' -> row,col-1
            | '>' -> row,col+1
            | x -> failwithf "%c is not a valid arrow button." x

        path
        |> Seq.scan (fun state current -> updatePoint current state ) (row,col) 
        |> Seq.exists (fun point -> point = contained)

let evalInstructions (instr: char list) =
    if instr.Length = 1
    then 1L
    else
        let mutable consecutiveDirectionBias = 0L
        instr 
        |> List.pairwise 
        |> List.sumBy(fun (x,y) -> 
            //  same buttons, so no movement, upstream robot just presses A again
            if x = y 
            then
                consecutiveDirectionBias <- consecutiveDirectionBias + 1L
            else consecutiveDirectionBias <- 0L
            
            consecutiveDirectionBias +
            // < is the furthest button to press, so if pressed first A is closer on next move
            if x='<' && y = 'v' then 1L
            else if x='<' && y = '^' then 1L
            else 0L)


let move (pad : char array2d) =
    let index = 
        pad
        |> Global.matrixIndices
        |> Seq.map (fun (row,col) -> pad.[row,col], (row,col))
        |> readOnlyDict

    let emptyPoint = index.['.']

    let pathPassesThroughEmptySpace start path = checkIfPathContains index start path emptyPoint

    let mutable cache = Map.empty<char*char*char,char list>

    fun (startDigit: char) (lastDigit: char) ->
        let cacheKey = (pad.[0,0],startDigit,lastDigit)
        
        match cache |> Map.tryFind cacheKey with
        | Some lst -> lst
        | None -> 
            let row1,col1= index[startDigit]
            let row2,col2= index[lastDigit]

            let rep c count= List.replicate count c 

            let horizontalMovement = 
                if col1 > col2 then rep '<' (col1-col2)
                else rep '>' (col2-col1)
            
            let verticalMovement = 
                if row1 > row2 then rep '^' (row1-row2)
                else rep 'v' (row2-row1)
            
            (* Consecutive identical digits will always beat zig-zagging paths 
             * Since arrow pad only has two rows this can probably be reduced further.
             *)
            [   for i in 0..horizontalMovement.Length-1 do
                    horizontalMovement.[..i-1] @ verticalMovement @ horizontalMovement.[i..] 
                for i in 0..verticalMovement.Length-1 do
                    verticalMovement.[..i-1] @ horizontalMovement @ verticalMovement.[i..]
            ]
            |> List.filter ((pathPassesThroughEmptySpace startDigit)>>not)
            |> function
            | [] -> []
            | lst -> lst |> List.maxBy (evalInstructions)
            // |> Global.tee $"{startDigit} -> {lastDigit}"
            |> fun result -> 
                cache <- Map.add cacheKey result cache
                cache.[cacheKey]
        
let moveSeries (pad : char array2d) = 
    let movepad = move pad
    
    // cache stuff here if needed

    fun (series: char list) ->
        ['A']@series 
        |> List.pairwise 
        |> List.collect (fun (start,stop) -> movepad start stop @ ['A'])


let mutable initEncode = 0
let encodingLength (pad: char array2d)  =
    initEncode <- initEncode + 1
    printfn "encode initialized %d times" initEncode
    
    let moveSeriesPad = moveSeries pad
    let mutable cache = Map.empty<int*char list, int64>
    
    let rec search (depth :int) (remainingInstructions: char list) =
        // printf "Depth: %d Current length: %d -- %A" depth remainingInstructions.Length remainingInstructions
        
        if remainingInstructions = [] then 0L
        else if depth = 0
        then
            // printfn "" 
            remainingInstructions |> List.length |> int64
        else
            let currentSeries, remaining = 
                let idxA = 
                    remainingInstructions 
                    |> List.findIndex (fun c -> c = 'A' )
                
                List.splitAt (idxA+1) remainingInstructions

            // printfn " -- Current: %A Remaining: %A" currentSeries remaining

            let cacheKey = depth, currentSeries

            let fullLayerMoveCountForCurrentSeries = 
                match cache |> Map.tryFind cacheKey with
                | Some length -> length
                | None -> 
                    let upstreamSeries = moveSeriesPad currentSeries
                    let result = search (depth-1) upstreamSeries // drill down length
                    cache  <- cache |> Map.add cacheKey result
                    cache[cacheKey]
            
            fullLayerMoveCountForCurrentSeries + (search depth remaining) // current layer length

    fun (depth: int) (instructions: char list) -> 
        search depth instructions
   

let calculateComplexity (code:string) (instructions:int64) =
    let codeNum = int64 <| Regex.Match(code, "\d+").Value
    instructions * codeNum

(* ----- *)

let moveNum = moveSeries Common.numpad

let enc = encodingLength Common.arrowpad

let decode (code: string) = 
    code
    |> _.ToCharArray() 
    |> List.ofArray 
    |> moveNum
    |> enc 2    


"./input.example"
|> File.ReadAllLines
|> Array.map(fun code -> code, decode code)
|> Array.sumBy(fun (code, length) -> calculateComplexity code length)
|> Global.shouldBeAndTee 126384
|> printfn "The sum of the complexities of the five example codes is %d"

"./input.actual"
|> File.ReadAllLines
|> Array.map(fun code -> code, decode code)
|> Array.sumBy(fun (code, length) -> calculateComplexity code length)
|> printfn "The sum of the complexities of the five puzzle input codes at 2 robots removed is %d"

let decode25 (code: string) = 
    code
    |> _.ToCharArray() 
    |> List.ofArray 
    |> moveNum
    |> enc 25

"./input.actual"
|> File.ReadAllLines
|> Array.map(fun code -> code, decode25 code)
|> Array.sumBy(fun (code, length) -> calculateComplexity code length)
|> printfn "The sum of the complexities of the five puzzle input codes at 25 robots removed is %d"
