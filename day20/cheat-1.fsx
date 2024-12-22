#load "../global.fsx"
#load "./common.fsx"

open System
open System.Collections.Generic
open Common

// let neighbors' (current: Point) =
//     let row,col = current
//     [   Up (row+1,col)
//         Dn (row-1,col)
//         Rt (row,col+1)
//         Lt (row,col-1) ]

// let availableCheats' (neighbors: Neighbor list) (matrix: char array2d) : Cheat list =
//     neighbors 
//     |> List.filter (fun neighbor -> 
//         match neighbor with
//         | Up (r,c)
//         | Dn (r,c)
//         | Lt (r,c)
//         | Rt (r,c) when matrix.[r,c] = '#' -> true
//         | _ -> false )
//     |> List.choose (fun neighbor -> 
//         match neighbor with
//         | Up (row,col) when matrix.[row-1,col] <> '#'-> Some ((row,col),(row-1,col))
//         | Dn (row,col) when matrix.[row+1,col] <> '#'-> Some ((row,col),(row+1,col))
//         | Lt (row,col) when matrix.[row,col-1] <> '#'-> Some ((row,col),(row,col-1))
//         | Rt (row,col) when matrix.[row,col+1] <> '#'-> Some ((row,col),(row,col+1))
//         | x -> None        
//     )

let availableCheats (matrix: char array2d) (current: Point) : Cheat list = 
    let row,col= current
    let rows,cols = matrix |> Global.matrixSize
    let noOverflow point = isNotOverflowing (rows,cols) point

    [   (row-1,col),(row-2,col)
        (row+1,col),(row+2,col)
        (row,col-1),(row,col-2)
        (row,col+1),(row,col+2)    ]    
    |> List.filter (fun ((r1,c1),(r2,c2)) -> 
        noOverflow (r1,c1) && noOverflow (r2,c2)
        && matrix.[r1,c1] = '#' && matrix.[r2,c2] <> '#')

let applyCheats (matrix: char array2d) (fullPath: Point list) = 
    let remainingPathCache = 
        fullPath 
        |> List.mapi (fun index point -> point, fullPath.Length - index) 
        |> readOnlyDict
    
    let cheats = 
        fullPath 
        |> List.collect (fun current -> 
            availableCheats matrix current
            |> List.filter (fun (p1,p2) -> remainingPathCache.[current] > remainingPathCache.[p2]))
        |> List.distinct
    
    printfn "FUll Path Length: %d Cheats found: %d" fullPath.Length cheats.Length

    fullPath
    |> List.mapi (fun index point ->
        let neighborhood = neighbors point

        cheats 
        |> List.filter (fun (pt1,pt2) -> neighborhood |> List.contains pt1 && point <> pt2)
        |> List.map ( fun  activeCheat -> 
            let cheatStart,cheatEnd = activeCheat
            let cheatEndIndex = fullPath |> List.findIndex (fun pt -> pt = cheatEnd)
            
            let path = fullPath.[..index]@[cheatStart]@fullPath.[cheatEndIndex..]

            let diff = fullPath.Length - path.Length
            diff)) 
    |> List.collect id 
    |> List.filter (fun saved -> saved > 0)
    |> List.countBy id
    |> List.sortBy fst
    |> List.map (fun (saved,freq) -> { Saved = saved; CheatCount = freq })

(* did not work out *)
let exploreCheatingSpace (matrix: char array2d) (fullPath: Point list) = 
    let initPosition =
        fullPath        
        |> List.head

    matrix.[fst initPosition, snd initPosition] |> Global.shouldBe 'S'

    let cheats = 
        let index = HashSet<Point>()
        
        fullPath 
        |> List.collect (availableCheats matrix)         
        |> List.filter (fun (pt1, _) ->  index.Add(pt1)) // prevent inverted direction cheats
    
    printfn "Cheats found: %d" cheats.Length
    
    let remainingPathCache = 
        // let cheatIndex = 
        //     cheats 
        //     |> List.map (fun cheat -> fst cheat, -1) // so walk-through wall always gets chosen when in neighbors

        fullPath 
        //|> List.rev
        |> List.mapi (fun index point -> point, fullPath.Length - index) 
        // |> List.append cheatIndex
        |> readOnlyDict

    let rec countPicoseconds (position: int*int) (score: int) (activeCheat: Point) (shortestDistanceSoFarIndex: int array2d) depth =
        let row,col = position
        // printfn "%A %d %d" position score depth

        if matrix.[row,col] = 'E'
        then score
            //printMaze matrix updatedPath []
            //printfn "Score: %d Path length: %d %A" score (updatedPath.Length) (sw.Elapsed)
            //[updatedPath]
        else
            let neighborhood = 
                let n = neighbors position
                if n |> List.contains activeCheat && shortestDistanceSoFarIndex.[fst activeCheat, snd activeCheat] = Int32.MaxValue
                then [activeCheat]
                else n

            neighborhood
            |> List.filter(fun (row,col) -> 
                (score + 1) < shortestDistanceSoFarIndex.[row,col]
                && (matrix.[row,col] <> '#' || activeCheat = (row,col)))
            |> List.map(fun (nextRow,nextCol) -> 
                    let key = nextRow,nextCol
                    if remainingPathCache.ContainsKey key && shortestDistanceSoFarIndex.[fst activeCheat, snd activeCheat] < Int32.MaxValue
                    then score + remainingPathCache.[key]
                    else 
                        let newScore = score + 1
                        shortestDistanceSoFarIndex.[nextRow,nextCol] <- newScore
                        countPicoseconds key newScore activeCheat shortestDistanceSoFarIndex (depth + 1) )
            |> List.min

    let mutable index = 0
    let result = ResizeArray<int>()
    printfn "%A" cheats[5096]
    printMaze matrix [] [cheats[5096]] (-1,-1)

    exit 666
    for cheat in cheats |> List.skip 5097 do
        //|> List.mapi(fun index cheat -> 
            let (deletedWallRow, deletedWallCol),_ = cheat
            //let matrix' = Array2D.copy matrix
            //matrix'.[deletedWallRow,deletedWallCol] <
            let rows,cols= Global.matrixSize matrix
            let shortestDistanceSoFar = Array2D.create  rows cols (Int32.MaxValue)
            shortestDistanceSoFar.[fst initPosition, snd initPosition] <- 0
            // shortestDistanceSoFar.[deletedWallRow, deletedWallCol] <- -1
            
            printfn "Cheat #%d %A" index cheat
            // printMaze matrix [] [cheat]

            // Console.ReadLine() |> ignore

            let pathWithCheatScore = 
                countPicoseconds initPosition 0 (fst cheat) shortestDistanceSoFar 0 
                // |> List.last

            printfn "Picoseconds saved: %d" (fullPath.Length - 1 - pathWithCheatScore)
            // pathWithCheat
            // |> List.iter (fun path -> 
            //     printfn "Path saved %d psecs" (fullPath.Length - path.Length - 1)
            
            
            
            //pathWithCheat |> List.map(fun path -> fullPath.Length - 1 - path.Length)
            result.Add <| fullPath.Length - 1 - pathWithCheatScore
            index <- index + 1

    result
    |> List.ofSeq
    |> List.countBy id
    |> List.sortBy fst
    |> List.map (fun (saved,freq) -> { Saved = saved; CheatCount = freq })
    //|> List.iter (fun (score, freq) -> printfn $"There are {freq} cheats that save {score} picoseconds.")

"./input.example"
|> parse
|> fun matrix -> 
    matrix
    |> findFullPath
    |> applyCheats matrix
    |> fun cs -> cs |> List.iter (printfn "%O");cs
    |> Global.seqShouldBe [
            { Saved = 2; CheatCount = 14 }
            { Saved = 4; CheatCount = 14 }
            { Saved = 6; CheatCount = 2 }
            { Saved = 8; CheatCount = 4 }
            { Saved = 10; CheatCount = 2 }
            { Saved = 12; CheatCount = 3 }
            { Saved = 20; CheatCount =  1 }
            { Saved = 36; CheatCount =  1 }
            { Saved = 38; CheatCount =  1 }
            { Saved = 40; CheatCount =  1 }
            { Saved = 64; CheatCount =  1 }    ]


"./input.actual"
|> parse
    |> fun matrix -> 
    matrix
    |> findFullPath
    |> applyCheats matrix
    |> List.filter (fun result -> result.Saved >= 100)
    |> List.sumBy _.CheatCount
    |> printfn "%d cheats would save you at least 100 picoseconds"

