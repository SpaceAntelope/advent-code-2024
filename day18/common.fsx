open System.Collections.Generic
open System

#load "../global.fsx"

type Coord = int*int

let neighbors (current: int*int)  =
    let row,col = current
    [   row,col-1
        row,col+1
        row-1,col
        row+1,col    ]

let AStar (size: int*int) (obstacles: Coord Set ) =     
    let rows,cols = size
    let initPosition = 0,0
    let endPosition = rows-1,cols-1
    let h (node: Coord) = Global.manhattan node endPosition
    let openSet = PriorityQueue<Coord, int>()
    let cameFrom = Dictionary<Coord,Coord>()
    let gScore = // score for how much you travelled
        List.allPairs [0..rows-1] [0..cols-1]
        |> List.fold (fun (state: Dictionary<Coord,int>) (row,col) -> 
                state.Add((row,col), Int32.MaxValue)
                state) (Dictionary<Coord,int>())
    let fScore = // score for how much you travelled and howmuich you have left
        List.allPairs [0..rows-1] [0..cols-1]
        |> List.fold (fun (state: Dictionary<Coord,int>) (row,col) -> 
                state.Add((row,col), Int32.MaxValue)
                state) (Dictionary<Coord,int>())
    
    let reconstructPath (current: Coord) =
        List.unfold (fun parent -> if cameFrom.ContainsKey(parent) then Some (cameFrom.[parent],cameFrom.[parent]) else None ) cameFrom[current]
        |> List.append [cameFrom[current]]

    openSet.Enqueue(initPosition,0)
    gScore.[initPosition] <- 0
    fScore.[initPosition] <- h initPosition

    let mutable shortestPath = []
    while openSet.Count > 0 do
        let current = openSet.Dequeue()
        if current = endPosition 
        then shortestPath <- reconstructPath current
        else
            neighbors current
            |> List.filter (fun (row,col) -> 
                row >= 0 && row < rows 
                && col >= 0 && col < cols                 
                //&& visited.[row,col] > path.Length //+ cols-col + rows-row
                && (not <| obstacles.Contains(row,col)))
            |> List.iter(fun neighbor ->
                    let tentativeG = gScore[current] + 1 // always distance = 1 between neighbors
                    if tentativeG < gScore[neighbor]
                    then
                        cameFrom[neighbor] <- current
                        gScore[neighbor] <- tentativeG
                        fScore[neighbor] <- tentativeG + (h neighbor)
                        openSet.Enqueue(neighbor, fScore[neighbor])
            )
    
    // printMaze size obstacles shortestPath []
    shortestPath 
