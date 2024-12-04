open System.IO

#load "../global.fsx"

let isX_MasPattern (row: int) (col:int) (arr: char array2d) =
    let rowCount = arr |> Array2D.length1
    let colCount = arr |> Array2D.length2
    
    arr.[row,col] = 'A'
    && (row > 0 && col > 0 && row < rowCount-1 && col < colCount-1)
    && ((arr.[row-1,col-1] = 'M' && arr.[row+1,col+1] = 'S') || (arr.[row-1,col-1] = 'S' && arr.[row+1,col+1] = 'M'))
    && ((arr.[row-1,col+1] = 'M' && arr.[row+1,col-1] = 'S') || (arr.[row-1,col+1] = 'S' && arr.[row+1,col-1] = 'M'))
    
let countX_Mases (data : char array2d) =
    let rowCount = data |> Array2D.length1
    let colCount = data |> Array2D.length2
    
    [|0..colCount-1|]
    |> Array.allPairs [|0..(rowCount-1)|]
    |> Array.filter (fun (row,col) -> isX_MasPattern row col data)
    |> Array.length
    
"./input.example"
|> File.ReadAllLines
|> Array.map _.ToCharArray()
|> array2D
|> countX_Mases
|> Global.shouldBe 9

"./input.actual"
|> File.ReadAllLines
|> Array.map _.ToCharArray()
|> array2D
|> countX_Mases
|> printfn "Found %d instances of the X-MAS pattern"