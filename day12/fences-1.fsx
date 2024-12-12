open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

#load "../global.fsx"
#load "./common.fsx"

let calculateCost path expected= 
    let matrix = 
        path
        |> Common.parse

    matrix    
    |> Common.findRegions
    |> List.map (List.ofSeq)
    |> List.fold (fun cost region -> 
        let perimeter = Common.perimeter matrix region
        
        cost + region.Length * perimeter.Length ) 0
    |> fun cost ->
        if expected > 0 then Global.shouldBe expected cost
        else printfn "Total cost estimated at %d" cost

calculateCost "./input.test1" 140
calculateCost "./input.test2" 772
calculateCost "./input.example" 1930
calculateCost "./input.actual" -1


 
let matrix = 
    "./input.example"
    |> Common.parse

Global.print matrix

matrix    
|> Common.findRegions
|> List.map (List.ofSeq)
|> List.map (fun reg -> Common.perimeter  matrix reg)
|> List.iter (Common.printPerim matrix)
    
    
    
//     let index = perim |> List.map (fun (r,c) -> r+1,c+1) |> HashSet
//     Array2D.init (rowCount+2) (colCount+2) (fun row col -> 
//         index.Contains(row,col) 
//         |> function 
//         | true -> 'x' 
//         | false when row = 0 || row = rowCount+1 || col = 0 || col = colCount + 1  -> ' '
//         | _ -> '.')
//     |> Global.print
//     )

// // |> List.fold (fun cost region -> 
// //     let perimeter = Common.perimeter region matrix
    
// //     cost + region.Length * perimeter.Length ) 0
// // |> fun cost ->
// //     if expected > 0 then Global.shouldBe expected cost
// //     else printfn "Total cost estimated at %d" cost