open System
open System.IO
open System.Collections.Generic

let parse path =
    path
    |> File.ReadAllLines
    |> Array.map _.ToCharArray()
    |> array2D

type Direction = Up | Rt | Dn | Lt
type Move = Fwd | Left | Right

let rec travel (position: int*int) (dir: Direction) (score: int) (visited: HashSet<int*int*int>) =
    let row,col = position
    if matrix.[row,col] = 'E'
    then [score]
    else 
        [Fwd;Left;Right] |> List.collect (fun move -> )