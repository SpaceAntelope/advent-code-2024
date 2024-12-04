open System.IO

#load "../global.fsx"

let nextCell row col dir =
    // directions if the numpad was a compass
    match dir with
    | 1 -> row + 1, col - 1
    | 2 -> row + 1, col
    | 3 -> row + 1, col + 1
    | 4 -> row, col - 1
    | 6 -> row, col + 1
    | 7 -> row - 1, col - 1
    | 8 -> row - 1, col
    | 9 -> row - 1, col + 1
    | _ -> failwith $"{dir} isn't a numpad direction." 

let xmasIndex = [|'X';'M';'A';'S'|]

let rec isXmas (row:int) (col:int) (dir:int) (depth:int) (arr: char array2d) : bool =
    let value = arr.[row,col]
    if depth = 3
    then value = 'S'
    else if xmasIndex[depth] = value
    then
        let (nextRow, nextCol) =
            match dir with
            | 1 -> row + 1, col - 1
            | 2 -> row + 1, col
            | 3 -> row + 1, col + 1
            | 4 -> row, col - 1
            | 6 -> row, col + 1
            | 7 -> row - 1, col - 1
            | 8 -> row - 1, col
            | 9 -> row - 1, col + 1
            | _ -> failwith $"{dir} isn't a numpad direction." 

        let rowCount = arr |> Array2D.length1
        let colCount = arr |> Array2D.length2
       
        if nextRow >= 0 && nextRow < rowCount && nextCol >= 0 && nextCol < colCount
        then isXmas nextRow nextCol dir (depth+1) arr
        else false
    else false

let countXmases (data : char array2d) =
    let rowCount = data |> Array2D.length1
    let colCount = data |> Array2D.length2

    [|0..colCount-1|]
    |> Array.allPairs [|0..(rowCount-1)|]
    |> Array.map (fun (row,col) -> 
        if data.[row,col] = 'X'
        then 
            [|1;2;3;4;6;7;8;9|] // directions
            |> Array.filter (fun dir -> isXmas row col dir 0 data)
            |> Array.length
        else 0)
    |> Array.reduce (+)

"./input.example"
|> File.ReadAllLines
|> Array.map _.ToCharArray()
|> array2D
|> countXmases
|> Global.shouldBe 18

"./input.actual"
|> File.ReadAllLines
|> Array.map _.ToCharArray()
|> array2D
|> countXmases
|> printfn "Found %d instances of the XMAS pattern"