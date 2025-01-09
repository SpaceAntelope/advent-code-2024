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
    



let isOutOfBoundsFactory matrix = 
    let size = Global.matrixSize matrix
    let isOutOfBounds = Global.isOutOfBounds size
    fun row col -> isOutOfBounds (row,col)


let borderOfRegion matrix region=
    let isOutOfBounds = isOutOfBoundsFactory matrix
    region
    |> Seq.filter (fun (row,col) -> 
        let n = Common.neighbourhood matrix (row,col)
        n |> List.exists (fun (r,c) -> isOutOfBounds r c || matrix.[row,col] <> matrix.[r,c]))

type Point = int*int
type Dir = 
    | Up | Dn | Lt | Rt
    with 
        static member From (p1: Point, p2: Point) =
            let r1,c1 = p1
            let r2,c2 = p2
            match (r1-r2,c1-c2) with
            | diffRow, diffCol when diffRow = 0 && diffCol > 0 -> Lt
            | diffRow, diffCol when diffRow = 0 && diffCol < 0 -> Rt
            | diffRow, diffCol when diffRow > 0 && diffCol = 0 -> Up
            | diffRow, diffCol when diffRow < 0 && diffCol = 0 -> Dn
            | diffRow, diffCol  -> failwithf $"Don't how to get from %A{(r1,c1)} to $A{(r2,c2)}"
        member x.RotateRight =
            match x with
            | Up -> Rt
            | Rt -> Dn
            | Dn -> Lt
            | Lt -> Up

type DirectedBorder = {
    Row: int
    Col: int
    Directions: Dir list } 
    with 
        member x.HasDir(dir:Dir) = 
            x.Directions |> List.contains dir

let regionBorder matrix region = 
    let isOutOfBounds = isOutOfBoundsFactory matrix
    
    let regionPlot = 
        region 
        |> List.head 
        |> fun (row,col) -> matrix.[row,col]
        
    region
    |> List.map (fun point -> point, Common.neighbourhood matrix point )
    |> List.filter (fun (pt, n) ->
        n |> List.exists (fun (row,col) -> isOutOfBounds row col || matrix.[row,col] <> regionPlot))
    |> List.map fst
    |> Set.ofList

let regionDirectedBorder matrix region = 
    let isOutOfBounds = isOutOfBoundsFactory matrix
    
    let regionPlot = 
        region 
        |> List.head 
        |> fun (row,col) -> matrix.[row,col]
    
    let isOutOfRegion (row, col) = isOutOfBounds row col || matrix.[row,col] <> regionPlot
    region
    |> List.map (fun point -> point, Common.neighbourhood matrix point )
    |> List.filter (fun (pt, n) ->
        n |> List.exists isOutOfRegion)
    |> List.map (fun (pt1,n) ->
        pt1, n |> List.filter isOutOfRegion |> List.map (fun pt2 -> Dir.From(pt1,pt2)) )
    |> List.map (fun (pt,dir) -> { Row = fst pt; Col = snd pt; Directions = dir })

let sidesOfRegion matrix region =
    let borderSet = regionDirectedBorder matrix region

    let topSides = 
        borderSet 
        |> List.groupBy (fun border -> 
            border.Row,border.HasDir Up)
    let bottomSides = 
        borderSet 
        |> List.groupBy (fun border -> 
            border.Row,border.HasDir Dn)
    let leftSides = 
        borderSet 
        |> List.groupBy (fun border -> 
            border.Col,border.HasDir Lt)
    let rightSides = 
        borderSet 
        |> List.groupBy (fun border -> 
            border.Col,border.HasDir Rt)

    let horizontalSideCount rowSet = 
        rowSet 
        |> List.map snd 
        |> List.sumBy (fun rowGroup -> 
            rowGroup
            |> List.sortBy _.Col 
            |> List.pairwise 
            |> List.filter (fun (bd1,bd2) -> bd2.Col - bd1.Col <> 1) 
            |> List.length 
            |> (+) 1)
    
    let verticalSideCount rowSet = 
        rowSet 
        |> List.map snd 
        |> List.sumBy (fun colGroup -> 
            colGroup
            |> List.sortBy _.Row
            |> List.pairwise 
            |> List.filter (fun (bd1,bd2) -> bd2.Col - bd1.Col <> 1) 
            |> List.length 
            |> (+) 1)
    
    horizontalSideCount topSides
    + horizontalSideCount bottomSides
    + verticalSideCount leftSides
    + verticalSideCount rightSides

let showRegions path =
    let matrix = 
        path
        |> Common.parse

    matrix
    |> Common.findRegions
    |> List.iter (fun region -> 
        let perim= Common.perimeter matrix (region |> List.ofSeq)
        Common.printPerim matrix perim
        
        regionDirectedBorder matrix (region |> List.ofSeq)
        |> printfn "RDB: %A"

        sidesOfRegion matrix (region |> List.ofSeq)
        |> printfn "Found %d sides"
    )

showRegions "input.test1"
showRegions "input.test2"
showRegions "input.example"
showRegions "input.example2"

// let sidesOfRegion region = 
//     let isOutOfBounds = isOutOfBoundsFactory matrix
//     let isInBounds row col = (isOutOfBounds row col) |> not
//     let border = regionBorder matrix region
//     let start = border |> Set.minElement

//     let rec walk (current: Point) (previous: Point) (currentDir: Dir) walked =
//         let row,col = current
//         let prevRow, prevCol = previous
        
//         if walked |> Set.contains (row,col,dir) 
//         then 0
//         else if isInBounds row col && matrix.[row,col] = matrix.[prevRow,prevCol]
//         then 
//             let walked' = walked |> Set.add (row,col,dir)
//             let score = if currentDir <> lastDir then 1 else 0
            
//             score 
//             + walk (row+1,col) currentDir Dn walked'
//             + walk (row-1,col) currentDir Up walked'
//             + walk (row,col+1) currentDir Rt walked'
//             + walk (row,col-1) currentDir Lt walked'
//         else 0
      
                    
                        


                

exit 666

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

// let matrix = Common.parse "./input.test1"
// matrix |> Global.print

// let perims = 
//     matrix
//     |> Common.findRegions
//     |> List.map (List.ofSeq)
//     |> List.map (Common.perimeter matrix)
//     |> List.map (fun per -> 
//         Common.printPerim matrix per
//         let (rowCount,colCount) = Global.matrixSize matrix
        

//         per 
//         |> List.groupBy fst
//         |> List.sumBy (fun (rowIndex, rowGroup) -> 
//             printfn "peri for row %d: %A" rowIndex (rowGroup |> List.sortBy snd)
//             let sides = 
//                 rowGroup 
//                 |> List.sortBy snd
//                 //|> List.pairwise
//                 |> List.fold (fun sideCount (row,col) -> 
                        
//                         if per |> List.contains (row+1,col) || per |>List.contains (row-1,col)
//                         then sideCount
//                         else

//                             let nextFence = 
//                                 if rowGroup |> List.contains (row,col+1) && col < colCount
//                                 then Some(row,col+1)
//                                 else None

//                             printfn "%A -> %A" (row,col) nextFence

//                             match nextFence with
//                             | None when 
//                                 (per |> List.contains (row-1,col+1) 
//                                 && per |> List.contains (row+1,col+1))
//                                 ||
//                                 per |> List.contains (row-1,col-1) 
//                                 && per |> List.contains (row+1,col-1)
//                                     -> sideCount
//                             | None -> sideCount + 1
//                             | Some (nextRow,nextCol) when Math.Abs(nextCol-col) = 1 -> sideCount
//                             | Some (nextRow,nextCol) when 
//                                 per |> List.contains (row-1,nextCol) 
//                                 && per |> List.contains (row+1,nextCol) -> sideCount
//                             | Some (nextRow,nextCol) when 
//                                 per |> List.contains (row-1,nextCol-1) 
//                                 && per |> List.contains (row+1,nextCol-1) -> sideCount                        
//                             | Some x ->                           
//                                 sideCount + 1) 0            

//             printfn "Row: Horizontal sides in %A: %d" rowGroup sides
//             sides)
//             |> printfn "Total horizontal sides for region: %A"        
//             )
//     |> printfn "Total sides: %A"

//         // per 
//         // |> List.groupBy fst 
//         // |> List.filter (fun (key,grp) -> 
//         //     grp
//         //     |> List.sortBy snd
//         //     |> List.pairwise 
//         //     |> List.forall (fun ((r1,c1),(r2,c2)) -> 
//         //                 //printfn "%d,%d - %d, %d | %d " r1 c1 r2 c2 (r1-r2); 
//         //                 Math.Abs(c1-c2)=1 ))
//         // |> List.map snd
//         // |> List.iter (printfn "h: %A")

//         // per 
//         // |> List.groupBy snd 
//         // |> List.filter (fun (key,grp) -> 
//         //     grp
//         //     |> List.sortBy fst
//         //     |> List.pairwise 
//         //     // forall on empty returns true so we get 1 cell length sides too
//         //     |> List.forall (fun ((r1,c1),(r2,c2)) -> 
//         //                 //printfn "%d,%d - %d, %d | %d " r1 c1 r2 c2 (r1-r2); 
//         //                 Math.Abs(r1-r2)=1 ))
//         // |> List.map snd
//         // |> List.iter (printfn "v: %A")
//         // )
