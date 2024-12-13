open System
open System.IO
open System.Collections.Generic

#load "../global.fsx"
#load "./common.fsx"

// type Direction = H | V

// let traversePerimeter (matrix : char array2d) (perimeter: (int*int) list) = 
//     let perimSet = HashSet<int*int>(perimeter)
//     let visited = HashSet<int*int>()
//     let perimNeibourhood (row,col) =
//         [   row-1, col-1; row-1,col; row-1,col+1;
//             row, col-1; row, col+1;
//             row+1, col-1; row+1,col; row+1,col+1
//         ] |> List.filter (fun (r,c ) -> perimSet.Contains(r,c) && (not>>visited.Contains)(r,c))

//     let rec walk (sideCounter: int) (row,col) =
//         match visited.Add row,col with
//         | true -> 
//             perimNeibourhood (row,col)             
//             |> function
//             | r,c when r = row -> walk sideCounter (r,c) 
//             | r,c when c = col -> walk sideCounter (r,c)  
//             | r,c when r <> row && c <> col -> walk (sideCounter+1) (r,c)
//         | false -> 0

//     ()
    


let calculateCost path expected= 
    let matrix = 
        path
        |> Common.parse


    printfn "%A" matrix
    matrix    
    |> Common.findRegions
    |> List.map (List.ofSeq)
    |> List.fold (fun cost region -> 
        let perimeter = Common.perimeter matrix region
        let tee msg x = printfn "%s %A" msg x; x
        let hSides = perimeter |> List.countBy fst |> tee "H: " |> List.length
        let vSides = perimeter |> List.countBy snd |> tee "V: " |> List.length
        printfn "region %c \n%A\nperimeter\n%A\nh: %d v: %d\n" (region |> List.head |> fun (r,c) -> matrix.[r,c]) region perimeter hSides vSides
        cost + region.Length * (hSides*vSides) ) 0
    |> fun cost ->
        if expected > 0 then Global.shouldBe expected cost
        else printfn "Total bulk cost estimated at %d" cost

// calculateCost "./input.test1" 80
// calculateCost "./input.test2" 236
// calculateCost "./input.example" 1206
// calculateCost "./input.example2" 368
// calculateCost "./input.actual" -1

let tee msg x = printfn "%s %A" msg x; x

let matrix = Common.parse "./input.test1"
matrix |> Global.print

let perims = 
    matrix
    |> Common.findRegions
    |> List.map (List.ofSeq)
    |> List.map (Common.perimeter matrix)
    |> List.map (fun per -> 
        Common.printPerim matrix per
        let (rowCount,colCount) = Global.matrixSize matrix
        

        per 
        |> List.groupBy fst
        |> List.sumBy (fun (rowIndex, rowGroup) -> 
            printfn "peri for row %d: %A" rowIndex (rowGroup |> List.sortBy snd)
            let sides = 
                rowGroup 
                |> List.sortBy snd
                //|> List.pairwise
                |> List.fold (fun sideCount (row,col) -> 
                        
                        if per |> List.contains (row+1,col) || per |>List.contains (row-1,col)
                        then sideCount
                        else

                            let nextFence = 
                                if rowGroup |> List.contains (row,col+1) && col < colCount
                                then Some(row,col+1)
                                else None

                            printfn "%A -> %A" (row,col) nextFence

                            match nextFence with
                            | None when 
                                (per |> List.contains (row-1,col+1) 
                                && per |> List.contains (row+1,col+1))
                                ||
                                per |> List.contains (row-1,col-1) 
                                && per |> List.contains (row+1,col-1)
                                    -> sideCount
                            | None -> sideCount + 1
                            | Some (nextRow,nextCol) when Math.Abs(nextCol-col) = 1 -> sideCount
                            | Some (nextRow,nextCol) when 
                                per |> List.contains (row-1,nextCol) 
                                && per |> List.contains (row+1,nextCol) -> sideCount
                            | Some (nextRow,nextCol) when 
                                per |> List.contains (row-1,nextCol-1) 
                                && per |> List.contains (row+1,nextCol-1) -> sideCount                        
                            | Some x ->                           
                                sideCount + 1) 0            

            printfn "Row: Horizontal sides in %A: %d" rowGroup sides
            sides)
            |> printfn "Total horizontal sides for region: %A"        
            )
    |> printfn "Total sides: %A"

        // per 
        // |> List.groupBy fst 
        // |> List.filter (fun (key,grp) -> 
        //     grp
        //     |> List.sortBy snd
        //     |> List.pairwise 
        //     |> List.forall (fun ((r1,c1),(r2,c2)) -> 
        //                 //printfn "%d,%d - %d, %d | %d " r1 c1 r2 c2 (r1-r2); 
        //                 Math.Abs(c1-c2)=1 ))
        // |> List.map snd
        // |> List.iter (printfn "h: %A")

        // per 
        // |> List.groupBy snd 
        // |> List.filter (fun (key,grp) -> 
        //     grp
        //     |> List.sortBy fst
        //     |> List.pairwise 
        //     // forall on empty returns true so we get 1 cell length sides too
        //     |> List.forall (fun ((r1,c1),(r2,c2)) -> 
        //                 //printfn "%d,%d - %d, %d | %d " r1 c1 r2 c2 (r1-r2); 
        //                 Math.Abs(r1-r2)=1 ))
        // |> List.map snd
        // |> List.iter (printfn "v: %A")
        // )
