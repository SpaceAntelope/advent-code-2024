open System
open System.IO
open System.Text.RegularExpressions

let parseData path =
    path
    |> File.ReadAllLines
    |> Array.map _.ToCharArray()
    |> array2D

let euclideanDistance (r1, c1) (r2, c2) =
    Math.Sqrt(float ((r2 - r1) * (r2 - r1) + (c2 - c1) * (c2 - c1)))

let manhattanDistance (r1, c1) (r2, c2) =
    Math.Abs(float r1 - r2) + Math.Abs(float c1 - c2)

let printMatrix (matrix: char array2d) (antinodes: (int * int) seq) =
    matrix
    |> Array2D.mapi (fun row col value ->
        if antinodes |> Seq.contains (row, col) then
            '#'
        else
            value)
    |> sprintf " %A"
    |> fun str -> Regex.Replace(str, @"[\[\]\';]+", "")
    |> printfn "%s"    

let getUniquePairs (antinodes: (int * int) array) =
    seq {
        for an1 in 0 .. antinodes.Length - 1 do
            for an2 in 0 .. an1 - 1 do
                antinodes.[an1], antinodes.[an2]
    }
