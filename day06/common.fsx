open System.IO
open System.Text.RegularExpressions

#load "../global.fsx"

let parseData path =
    path
    |> File.ReadAllLines
    |> Array.map _.ToCharArray()
    |> array2D

type Path = Global.Point list


let printMatrix (obstacles : Path) (customObstacles: Path) (patrol: Path) 
    (position: int*int) (direction: int) (size: int*int) = 
    let (l1,l2) = size
    Array2D.init l1 l2 (fun row col -> 
        if obstacles |> List.contains (row,col) then '#'
        else if (row,col)=position then 
            match direction with 4 -> '<' | 8 -> '^' | 6 -> '>' | 2 -> 'v' | _ -> '_'
        else if customObstacles |> List.contains (row,col) then 'O'
        else if patrol |> List.contains (row,col) then 'X'
        else '.')
    |> sprintf "%A"
    |> fun str -> Regex.Replace(str, @"['\[\];]", "")
    |> printfn " %s\n"
let tryMinBy fn (source : 't seq) =
    match source with
    | seq when seq |> Seq.isEmpty -> None
    | seq -> Some (Seq.minBy fn seq)

let tryMaxBy fn (source : 't seq) =
    match source with
    | seq when seq |> Seq.isEmpty -> None
    | seq -> Some (Seq.maxBy fn seq)