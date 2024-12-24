#load "../global.fsx"
#load "./common.fsx"

open Common

open System
open System.IO

let triangularConnections (nodes: string array) (matrix: char array2d) =
    // printGraph matrix nodes
    let node2index =     
        nodes 
        |> Array.mapi (fun i n -> n,i) 
        |> readOnlyDict
    
    nodes
    |> Array.filter (fun n -> n.StartsWith('t')) 
    |> Array.collect(fun n -> 
        let rowIndex = node2index[n]
        let connectedNodes =
            [|0..nodes.Length-1|]            
            |> Array.filter (fun col -> matrix.[rowIndex,col] = 'O')
            |> Array.map (fun col -> nodes.[col])
        
        connectedNodes 
        |> halfPairs
        |> Array.filter (fun (a,b) -> a<>b && (matrix.[node2index[a], node2index[b]]='O'))
        |> Array.map(fun (a,b) ->[|n;a;b|]|>Array.sort)
        )
    |> Array.distinct
    |> Array.sortBy(Array.head)

"./input.example"
|> parse'
|> Array.map (fun arr -> arr[0],arr[1])
|> graphMatrix
||> triangularConnections
|> Global.seqShouldBe [|
    [|"co";"de";"ta"|]
    [|"co";"ka";"ta"|]
    [|"de";"ka";"ta"|]
    [|"qp";"td";"wh"|]
    [|"tb";"vc";"wq"|]
    [|"tc";"td";"wh"|]
    [|"td";"wh";"yn"|]
|]


"./input.actual"
|> parse'
|> Array.map (fun arr -> arr[0],arr[1])
|> graphMatrix
||> triangularConnections
|> Array.length
|> printfn "The number of sets of three inter-connected computers where at least one computer with a name that starts with t is %d"
