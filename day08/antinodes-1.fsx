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
    |> fun x ->
        //printfn "h: %d v: %d N %A %A AN %A" hDist vDist (r1, c1) (r2, c2) x
        x

let antinodes (matrix: char array2d) =
    let rowCount =matrix |> Array2D.length1
    let colCount =matrix |> Array2D.length2
    nodeIndex matrix
    |> Array.collect (fun (freq, points) ->
        points
        |> Common.getUniquePairs
        |> Seq.collect antiNodesFromPair
        |> Seq.filter (fun (row,col) -> row >= 0 && col >= 0 && row < rowCount && col < colCount)
        |> Array.ofSeq)

let example()=
    let matrix = 
        "./input.example"
        |> Common.parseData
    
    let antinodes = antinodes matrix

    Common.printMatrix matrix antinodes
    
    antinodes
    |> Array.distinct
    |> Array.length
    |> Global.shouldBe 14

example()


"./input.actual"
|> Common.parseData
|> antinodes    
|> Array.distinct
|> Array.length
|> printfn "Discovered %d antinodes"