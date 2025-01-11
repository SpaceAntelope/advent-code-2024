open System
open System.IO
open System.Collections.Generic

#load "../global.fsx"
#load "./common.fsx"

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

type DirectedBorder = {
    Row: int
    Col: int
    Directions: Dir list } 
    with 
        member x.HasDir(dir:Dir) = 
            x.Directions |> List.contains dir
        override x.ToString() = $"{x.Row},{x.Col} %A{x.Directions}"

let neighbourhood point = 
    let row,col = point
    [row-1,col;row,col+1;row+1,col;row,col-1]

let isOutOfBoundsFactory matrix = 
    let size = Global.matrixSize matrix
    let isOutOfBounds = Global.isOutOfBounds size
    fun row col -> isOutOfBounds (row,col)

let regionDirectedBorder matrix region = 
    let isOutOfBounds = isOutOfBoundsFactory matrix
    
    let regionPlot = 
        region 
        |> List.head 
        |> fun (row,col) -> matrix.[row,col]
    
    let isOutOfRegion (row, col) = isOutOfBounds row col || matrix.[row,col] <> regionPlot
    region
    |> List.map (fun point -> point, neighbourhood point )
    |> List.filter (fun (pt, n) ->
        n |> List.exists isOutOfRegion)
    |> List.map (fun (pt1,n) ->
        pt1, n |> List.filter isOutOfRegion |> List.map (fun pt2 -> Dir.From(pt1,pt2)) )
    |> List.map (fun (pt,dir) -> { Row = fst pt; Col = snd pt; Directions = dir })

let sidesOfRegion matrix region =
    let borderSet = regionDirectedBorder matrix region

    // borderSet |> printfn "RDB: %A"
    let getRelevantGroup grp = 
        grp 
        |> List.filter (fun ((_,hasDir),_) -> hasDir) 
        |> List.map snd

    let topSides = 
        borderSet |> List.groupBy (fun border -> border.Row,border.HasDir Up) |> getRelevantGroup
    let bottomSides = 
        borderSet |> List.groupBy (fun border -> border.Row,border.HasDir Dn) |> getRelevantGroup
    let leftSides = 
        borderSet |> List.groupBy (fun border -> border.Col,border.HasDir Lt) |> getRelevantGroup
    let rightSides = 
        borderSet |> List.groupBy (fun border -> border.Col,border.HasDir Rt) |> getRelevantGroup

    let horizontalSideCount rowSet = 
        rowSet 
        |> List.sumBy (fun rowGroup -> 
            rowGroup
            |> List.sortBy _.Col 
            |> List.pairwise 
            |> List.filter (fun (bd1,bd2) -> bd2.Col - bd1.Col <> 1) 
            |> List.length 
            |> (+) 1)
    
    let verticalSideCount rowSet = 
        rowSet 
        |> List.sumBy (fun colGroup -> 
            colGroup
            |> List.sortBy _.Row
            |> List.pairwise 
            |> List.filter (fun (bd1,bd2) -> bd2.Row - bd1.Row <> 1) 
            |> List.length 
            |> (+) 1)
    
    horizontalSideCount topSides
    + horizontalSideCount bottomSides
    + verticalSideCount leftSides
    + verticalSideCount rightSides

let calculateFenceCost path =
    let matrix = 
        path
        |> Common.parse

    matrix
    |> Common.findRegions
    |> List.sumBy (fun region -> 
        // let perim = Common.perimeter matrix (region |> List.ofSeq)
        // Common.printPerim matrix perim
        
        let sides = sidesOfRegion matrix (region |> List.ofSeq)

        let costOfRegion = sides * region.Count

        // printfn "Found %d sides for a cost of %i" sides costOfRegion
        
        costOfRegion)

"input.test1"
|> calculateFenceCost 
|> Global.shouldBe 80

"input.test2"
|> calculateFenceCost 
|> Global.shouldBe 436

"input.test3"
|> calculateFenceCost 
|> Global.shouldBe 236

"input.example"
|> calculateFenceCost 
|> Global.shouldBe 1206

"input.example2"
|> calculateFenceCost 
|> Global.shouldBe 368

"input.actual"
|> calculateFenceCost 
|> printfn "The new total price of fencing all regions on the map is %i"
