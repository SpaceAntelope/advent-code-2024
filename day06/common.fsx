open System.IO
open System.Text.RegularExpressions
let parseData path =
    path
    |> File.ReadAllLines
    |> Array.map _.ToCharArray()
    |> array2D

type Path = (int*int) list

let printMatrix (obstacles : Path) (patrol: Path) 
    (position: int*int) (direction: int) (size: int*int) = 
    let (l1,l2) = size
    Array2D.init l1 l2 (fun row col -> 
        if obstacles |> List.contains (row,col) then '#'
        else if (row,col)=position then 
            match direction with 4 -> '<' | 8 -> '^' | 6 -> '>' | 2 -> 'v' | _ -> '_'
        else if patrol |> List.contains (row,col) then 'X'
        else '.')
    |> sprintf "%A"
    |> fun str -> Regex.Replace(str, @"['\[\];]", "")
    |> printfn " %s\n"

let tryMinBy fn (source : 't list) =
    match source with
    | [] -> None
    | l -> Some (List.minBy fn l)

let tryMaxBy fn (source : 't list) =
    match source with
    | [] -> None
    | l -> Some (List.maxBy fn l)