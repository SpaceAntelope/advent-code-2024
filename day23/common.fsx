#load "../global.fsx"
open System.IO

let printGraph (graph: char array2d) (nodes: string array) =  
    let (rows,cols) = Global.matrixSize graph

    let printColIndex () = 
        [|0..cols-1|] 
        |> Array.map (fun i -> nodes.[i]) 
        |> Array.map _.ToCharArray() 
        |> Array.transpose 
        |> Array.iter (fun digits-> 
            printf "       "
            digits |> Array.iter(printf " %c")
            printfn ""
        )

    // let path' = path |> Array.ofSeq

    printColIndex()
    for row in 0..rows-1 do
        printf $"%3d{row}: {nodes.[row]} "
        for col in 0..cols-1 do
            if graph[row,col] = 'O' then '⦾'       
            else ' '           
            |> printf "%c "
        printf $"{nodes.[row]}"
        if row < rows then printfn ""
    printColIndex()
    printfn ""


let parse path = 
    path 
    |> File.ReadAllLines 
    |> Array.fold (fun map current -> 
        let pair = current.Split('-')
        map |> Map.add pair.[0] pair.[1] |> Map.add pair.[1] pair.[0]) Map.empty<string,string>

let parse' path = 
    path 
    |> File.ReadAllLines 
    |> Array.map _.Split('-')

let edgesLookup (edges: (string*string) array) =
    edges
    // |> Array.append (edges |> Array.map(fun (a, b) -> b,a))
    |> Array.groupBy fst
    |> Array.map (fun (key, grp) -> key, fun (pc : string) -> grp |> Array.exists (fun (a,b) -> b = pc))
    |> readOnlyDict

let graphMatrix (nodes : (string*string) array) = 
    let edgeLookup = edgesLookup nodes    
    
    let nodes = 
        edgeLookup.Keys 
        |> Seq.sort 
        |> Array.ofSeq

    nodes, Array2D.init nodes.Length nodes.Length (fun row col -> 
        let a = nodes.[row]
        let b= nodes.[col]
        if (edgeLookup.[a] b) || (edgeLookup.[b] a) then 'O' else '.')
    
let halfPairs (nodes: string array) =
    [|  for i in 0..nodes.Length-1 do
            for j in 0..nodes.Length-1 do
                if j < i then nodes.[i], nodes.[j]  |]