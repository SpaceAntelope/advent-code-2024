open System
open System.IO

#load "../global.fsx"
#load "./common.fsx"

open System.Collections.Generic

let mapAllTrails (matrix : int array2d) =
    let rowCount = matrix |> Array2D.length1
    let colCount = matrix |> Array2D.length2
    let visited = HashSet<int*int>()
    
    let rec search (current:int*int) (visited: HashSet<int*int>) (path: (int*int) list) : (int*int) list list= 
        let (row,col) = current
        let currentValue = matrix.[row,col]

        // Remove to solve for 10-2
        // visited.Add (row,col) |> ignore

        // If on a 9 return the complete path
        if currentValue = 9 then [List.append path [row,col] ]
        // Otherwise find eligible neihbouring cells and continue search
        else        
            
            [ row-1, col;row, col-1; row, col+1; row+1,col]
            |> List.filter (fun (r,c) -> 
                not (visited.Contains(r,c))
                && r >= 0 && c>=0 && r < rowCount && c < colCount
                && matrix.[r,c]-currentValue = 1 )
            |> List.collect (fun next ->
                [row,col] 
                |> List.append path  
                |> search next visited)

    // Find starting cells, i.e. contain 0
    matrix
    |> Global.matrixIndices
    |> Seq.filter (fun (row,col) -> matrix.[row,col] = 0)
    // Find all trails starting from those cells and flatten the result
    |> Seq.collect (fun trailhead -> search trailhead (HashSet<int*int>()) [])
    
"./input.test"
|> Common.parse 
|> mapAllTrails
|> Seq.countBy (fun path -> path |> List.head, path |> List.last)
|> Seq.iter (printfn "%A")

"./input.example"
|> Common.parse 
|> mapAllTrails
|> Seq.length
|> Global.shouldBe 81

"./input.actual"
|> Common.parse 
|> mapAllTrails
|> Seq.length
|> printfn "The sum total of trail rankings is %d"