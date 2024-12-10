open System
open System.IO
open System.Collections.Generic

#load "../global.fsx"
#load "./common.fsx"

let mapAllTrails (matrix : int array2d) =
    let rowCount = matrix |> Array2D.length1
    let colCount = matrix |> Array2D.length2

    let rec search (current:int*int) (visited: HashSet<int*int>) (path: (int*int*int) list)  : (int*int*int) list list= 
        //printfn "%A %A" current path
        let (row,col) = current
        let currentValue = matrix.[row,col]
        visited.Add (row,col) |> ignore

        if currentValue = 9 then [List.append path [row,col,currentValue] ]
        else
            let neighbourhood = 
                [| row-1, col;row, col-1; row, col+1; row+1,col|]
                |> Array.filter (fun (r,c)-> 
                    not (visited.Contains(r,c))
                    && r >= 0 && c>=0 && r < rowCount && c < colCount
                    && matrix.[r,c]-currentValue = 1 )
        
        
            [for next in neighbourhood do
                yield! [row,col,currentValue] 
                        |> List.append path  
                        |> search next visited ]

    matrix
    |> Global.matrixIndices
    |> Seq.filter (fun (row,col) -> matrix.[row,col] = 0)
    |> Seq.collect (fun trailhead -> search trailhead (HashSet<int*int>()) [])
    
"./input.example"
|> Common.parse 
|> mapAllTrails
|> Seq.length
|> Global.shouldBe 36

"./input.actual"
|> Common.parse 
|> mapAllTrails
|> Seq.length
|> printfn "Total trails discovered: %d"
