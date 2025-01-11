open System.IO
open System.Collections.Generic

#load "../global.fsx"

type Point = int*int

let parse path = 
    path
    |> File.ReadAllLines
    |> Seq.map _.ToCharArray()
    |> array2D

let neighbourhood (matrix: char array2d) (row,col) =
    let rowCount = matrix |> Array2D.length1
    let colCount = matrix |> Array2D.length2
    
    [row-1,col;row,col+1;row+1,col;row,col-1]
    |> List.filter (fun (r,c) ->
        r>=0 && c>=0 
        && r < rowCount && c < colCount
        && matrix.[r,c] = matrix.[row,col])    

let perimeter (matrix: char array2d) (region: (int*int) list) =    
    let size = Global.matrixSize matrix
    let isOutOfBounds = Global.isOutOfBounds size
    
    let regionPlot = 
        region 
        |> List.head 
        |> fun (row,col) -> matrix.[row,col]
        
    region
    |> List.collect (fun (row,col) -> [row-1,col;row,col+1;row+1,col;row,col-1])
    |> List.filter (fun (row,col) ->
        isOutOfBounds (row,col) || matrix.[row,col] <> regionPlot)
        
let findRegions (matrix: char array2d) = 
    let rec fill (visited: HashSet<int*int>) (plot: int*int) =
        match visited.Add plot with
        | false -> []
        | true -> neighbourhood matrix plot
        |> List.iter (fill visited)

    let rec allRegions (remaining : (int*int) list) =
        [
            if remaining <> [] 
            then
                
                let region =  HashSet<int*int>() 
                remaining |> List.head |> fill region
                
                yield region
                
                yield! remaining 
                        |> List.except region
                        |> allRegions
        ]

    matrix
    |> Global.matrixIndices
    |> List.ofSeq
    |> allRegions  //(cells : (int*int*char) list) =

let printPerim matrix perimeter = 

    let rowCount = matrix |> Array2D.length1
    let colCount = matrix |> Array2D.length2
    
    let index = perimeter |> List.map (fun (r,c) -> r+1,c+1) |> HashSet
    
    Array2D.init (rowCount+2) (colCount+2) (fun row col -> 
        index.Contains(row,col) 
        |> function 
        | true -> 'x' 
        | false when row = 0 || row = rowCount+1 || col = 0 || col = colCount + 1  -> ' '
        | _ -> '.')
    |> Global.print