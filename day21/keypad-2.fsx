open System
open System.IO

#load "../global.fsx"
#load "./common.fsx"

open Common
// let mutable moveCacheHit = 0L
// let mutable moveCacheMiss = 0L
// let mutable encodeCacheHit = 0L
// let mutable encodeCacheMiss = 0L
// let mutable initEncode = 0L
// let mutable encodeCache = Map.empty<char*string,uint64>

// let encodeLengthOnly (pad: char array2d)  =
//     initEncode <- initEncode + 1L
//     printfn "encode initialized %d times" initEncode
//     // let mutable maxPathScore = 0L
//     let move = moveMulti pad
//     let rec search (pathLength: uint64) (remainingInstructions: string) (lastButton: char) =
//         if remainingInstructions = ""
//         then
//             // let evaluation = evalInstructions path
//             // if evaluation > maxPathScore
//             // then
//                 // maxPathScore <- evaluation
//                 pathLength
//         else
//             let prevBtn  = if pathLength = 0UL then 'A' else lastButton
//             let nextBtn = remainingInstructions.[0]
            
//             let nextInstruction =
//                 let moveCacheKey = (pad.[0,0], prevBtn, nextBtn)
//                 if moveCache.ContainsKey moveCacheKey |> not
//                 then 
//                     moveCacheMiss <- moveCacheMiss + 1L
//                     moveCache <- moveCache |> Map.add moveCacheKey (move prevBtn nextBtn)
//                 else 
//                     moveCacheHit <- moveCacheHit + 1L
                
//                 moveCache.[moveCacheKey]

//             //let nextPath = path + nextInstruction + "A"
//             let nextPathLength = pathLength + uint64 nextInstruction.Length + 1UL
//             let nextRemainingInstructions  = remainingInstructions.Substring(1)

//             let encodeCacheKey = pad.[0,0], nextRemainingInstructions
//             if encodeCache.ContainsKey encodeCacheKey |> not
//             then 
//                 encodeCacheMiss <- encodeCacheMiss + 1L
//                 encodeCache <- encodeCache |> Map.add encodeCacheKey (search nextPathLength nextRemainingInstructions nextBtn)
//             else encodeCacheHit <- encodeCacheHit + 1L
            
//             encodeCache.[encodeCacheKey]            
    
//     fun (instructions: string) -> 
//         search 0UL instructions '.'
        


let decode = 
    Array.create 25 (encode arrowpad)
    |> Array.reduce (>>)
    << (encode  numpad)

// "./input.example"
// |> File.ReadAllLines
// |> Array.map(fun code -> code, decode code)
// |> Array.sumBy(fun (code, instr) -> calculateComplexity code instr)
// |> Global.shouldBe 126384

"./input.actual"
|> File.ReadAllLines
|> Array.map (fun code -> 
    printfn "Trying %s" code
    code, decode code)
|> Array.sumBy(fun (code, instr) -> calculateComplexity code instr)
|> printfn "The sum of the complexities of the five codes on our list is %d"

printfn $"Move cache hit: {moveCacheHit}"
printfn $"Move cache miss: {moveCacheMiss}"
printfn $"Encode cache hit: {encodeCacheHit}"
printfn $"Encode cache miss: {encodeCacheMiss}"