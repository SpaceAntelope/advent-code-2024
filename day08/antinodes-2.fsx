open System

#load "../global.fsx"
#load "./common.fsx"
open System.Text.RegularExpressions

let nodeIndex matrix =
    matrix
    |> Global.matrixIndices
    |> Array.ofSeq
    |> Array.filter (fun (row, col) -> matrix.[row, col] <> '.')
    |> Array.groupBy (fun (row, col) -> matrix.[row, col])

// same as 8-1
let antiNodesFromPair ((r1, c1), (r2, c2)) =
    let hDist = c2 - c1
    let vDist = r2 - r1

    match hDist, vDist with
    | x, y when x >= 0 && y >= 0 -> 
        [ r1 - y, c1 - x
          r1 + 2 * y, c1 + 2 * x ]
    | x, y when x <= 0 && y <= 0 -> 
        [ r1 - y, c1 - x
          r1 + 2 * y, c1 + 2 * x ]
    | x, y when x >= 0 && y <= 0 -> 
        [ r1 - y, c1 - x
          r1 + 2 * y, c1 + 2 * x ]
    | x, y when x <= 0 && y >= 0 -> 
        [ r1 - y, c1 - x
          r1 + 2 * y, c1 + 2 * x ]
    | x, y -> failwithf $"Don't know what to do with {x},{y}"

let lineFromPair (matrix: char array2d) ((node1:int*int),(node2: int*int)) =
    let (row1, col1) = node1
    let (row2, col2) = node2

    let slope = float (row1-row2) / float (col1-col2)
    let bias = float row1 - slope * float col1

    let isOnLine (r,c) = 
        let row' = float r
        let col' = float c
        
        if row1 = row2 
        then float row' = row1 
        else if col1 = col2 
        then col' = col1
        else
            let left = row'
            let right = slope * col' + bias
            let diff = Math.Abs(left-right)
            
            diff < 0.0001 // 💀

    matrix 
    |> Global.matrixIndices
    |> Array.ofSeq    
    |> Array.filter isOnLine


let antinodes (matrix: char array2d) =
    let rowCount = matrix |> Array2D.length1
    let colCount = matrix |> Array2D.length2
    
    nodeIndex matrix
    |> Array.collect (fun (freq, points) ->
        points
        // |> Common.getUniquePairs
        |> Seq.allPairs points
        |> Seq.filter (fun (a,b) -> a<>b)
        |> Array.ofSeq
        |> Array.collect (fun nodePair -> 
            [|  yield! lineFromPair matrix nodePair
                yield! (antiNodesFromPair>>Array.ofList) nodePair |]) 
        |> Seq.filter (fun (row,col) ->  row >= 0 && col >= 0 && row < rowCount && col < colCount)
        |> Array.ofSeq)

let example path expected =
    let matrix = 
        path
        |> Common.parseData
    
    let antinodes = antinodes matrix

    Common.printMatrix matrix antinodes
    
    if expected > -1 
    then
        antinodes
        |> Array.distinct
        |> Array.length
        |> Global.shouldBe expected

example"./input.example" 34
example "./input-2.example" -1
example "./input-3.example" -1

"./input.actual"
|> Common.parseData
|> antinodes
|> Array.distinct
|> Array.length
|> printfn "Discovered %d antinodes using resonant harmonics"
