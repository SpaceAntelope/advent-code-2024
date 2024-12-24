#load "../global.fsx"
#load "./common.fsx"

open Common

open System
open System.IO

let parse path = 
    path 
    |> File.ReadAllLines 
    |> Array.map _.Split('-')
    |> Array.map (fun arr -> arr[0],arr[1])

let collectFullyConnected  (nodes: string array) (matrix: char array2d) =
    let node2index = nodes |> Array.mapi (fun i n-> n,i) |> readOnlyDict

    let node2connectedNodes = 
        let range = [|0..nodes.Length-1|]

        range
        |> Array.map (fun row -> 
                let node = nodes.[row]
                let connected =  
                    range 
                    |> Array.filter (fun col -> matrix.[row,col] = 'O')
                    |> Array.map (fun col -> nodes.[col])
                node,connected)
        |> readOnlyDict

    let mutable maxLength = 0

    let mutable cache = Map.empty<Set<string>*string, bool>
    let mutable cache2 = Map.empty<Set<string>*string, Set<string>[]>

    let cs = [|0;0|]

    let rec connect (fullyConnected: Set<string>)  (node: string ) depth =
        let nextNodes =             
            node2connectedNodes.[node] 
            |> Array.filter (fun nextNode -> 
                if cache |> Map.containsKey (fullyConnected, nextNode) |> not
                then
                    cs.[0] <- cs.[0] + 1
                    let result = 
                        fullyConnected |> Set.contains nextNode |> not
                        && fullyConnected |> Set.forall (fun connectedNode-> matrix.[node2index.[nextNode],node2index[connectedNode]] = 'O')
                    cache <- cache |> Map.add (fullyConnected, nextNode) result
                else 
                    cs.[1] <- cs.[1] + 1
                
                
                cache.[(fullyConnected,nextNode)])
                    
        [|
            if nextNodes.Length = 0 && fullyConnected.Count > maxLength
            then
                maxLength <- fullyConnected.Count 
                printfn "Max length: %d hit %d miss %d" maxLength cs.[1] cs.[0]
                printfn "%s" (fullyConnected |> Array.ofSeq |> Array.reduce(sprintf "%s,%s"))
                fullyConnected
            else    
                for nextNode in nextNodes do
                    if cache2 |> Map.containsKey (fullyConnected, nextNode) |> not
                    then
                        cs.[0] <- cs.[0] + 1                         
                        let result = connect (fullyConnected |> Set.add nextNode) nextNode (depth+1)
                        cache2 <- cache2 |> Map.add (fullyConnected, nextNode) result
                    else 
                        cs.[1] <- cs.[1] + 1
                        
                    yield! cache2.[fullyConnected,nextNode]
        |]

    // nodes
    // |> Array.splitInto 10
    // |> Array.map(fun nodeSegment ->
    //     async {
    //         return 
    //             nodeSegment
    //             |> Array.collect (fun node -> 
    //                 printfn "%s %d of %d" node node2index.[node] nodes.Length
    //                 connect (set [node]) node 0)
    //     }) 
    // |> Async.Parallel
    // |> Async.RunSynchronously
    // |> Array.collect id
    nodes 
    |> Array.collect(fun node -> 
        printfn "%s %d of %d" node (node2index.[node]+1) nodes.Length
        connect (set [node]) node 0)

"./input.example"
|> parse
|> graphMatrix
||> collectFullyConnected
|> Array.distinct
|> Array.sortBy Set.count
|> Array.last
|> Array.ofSeq
|> Array.sort
|> Array.reduce(sprintf "%s,%s")
|> Global.shouldBe "co,de,ka,ta"

"./input.actual"
|> parse
|> graphMatrix
||> collectFullyConnected
|> Array.distinct
|> Array.sortBy Set.count
|> Array.last
|> Array.ofSeq
|> Array.sort
|> Array.reduce(sprintf "%s,%s")
|> printfn "The password to get into the LAN party is %s"