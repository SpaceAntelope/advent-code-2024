open System
open System.Text.RegularExpressions

type Point = int*int

let shouldBe expected actual= 
    if expected <> actual 
    then 
        failwithf "Comparison between expected %A and actual %A failed." expected actual
let shouldBeAndTee expected actual= 
    if expected <> actual 
    then 
        failwithf "Comparison between expected %A and actual %A failed." expected actual
    else actual

let seqShouldBe expected actual= 
    if Seq.length expected <> Seq.length actual then failwithf $"Seq comparison failed because element count doesn't match. Expected count is {Seq.length expected} while actual count is {Seq.length actual}"

    Seq.zip expected actual
    |> Seq.iteri (fun index (expected, actual) -> 
            if expected <> actual 
            then failwithf "Comparison between expected %A and actual %A failed at index %d." expected actual index)

let matrixIndices matrix =
    let rows = matrix |> Array2D.length1
    let cols = matrix |> Array2D.length2
    ([0..rows-1],[0..cols-1])
    ||> Seq.allPairs

let print (matrix : 'a array2d) =
    matrix
    |> sprintf "%A"
    |> fun x -> Regex.Replace(x, @"[\[\]""';]+","")
    |> printfn " %s\n"

let matrixSize (matrix : 'a array2d) =
    let rows = matrix |> Array2D.length1
    let cols = matrix |> Array2D.length2
    rows,cols

let printMatrix (matrix : 'a array2d) =
    let rows,cols = matrixSize matrix

    for row in 0..rows-1 do
        for col in 0..cols-1 do
            printf " %O" matrix.[row,col]
        printfn ""

let tee msg x = 
    printfn "%s: %A" msg x
    x

let manhattan (a: int*int) (b: int*int) = 
    let r1,c1 = a
    let r2,c2 = b
    Math.Abs(r2-r1) + Math.Abs(c2-c1)

let isOutOfBounds (size: int*int) (point: Point) =
    let rows,cols = size
    let row,col = point

    row > rows-1 || col > cols-1 || col < 0 || row < 0


//[ for row in -1..6 do for col in -1..6 do (row,col) ,isOutOfBounds (5,5) (row,col) ]
// |> List.filter (fun (pt,isOut) -> isOut = true) |> List.map fst


// let rec f n = 
//     if n = 0L 
//     then 1L
//     else n * f (n-1L) 

// let comboCount n r= (f n) / ((f r) * f(n-r))

// comboCount 222L 8L


// let printMaze (maze: char array2d) (iconRules : Map<string, Set<int*int>*char>) =
//     let (rows,cols) = matrixSize maze
//     // let obsSet = obstacles |>  Set.ofSeq
//     // let pathSet = path |> Set.ofSeq
//     // let initSet = init |> readOnlyDict
//     let printColIndex () = 
//         [|0..cols-1|] 
//         |> Array.map (sprintf "%03d") 
//         |> Array.map _.ToCharArray() 
//         |> Array.transpose 
//         |> Array.iter (fun digits-> 
//             printf "   "
//             digits |> Array.iter(printf " %c")
//             printfn ""
//         )

//     // let path' = path |> Array.ofSeq

    

//     printColIndex()
//     for row in 0..rows-1 do
//         printf "%03d " row
//         for col in 0..cols-1 do                    
//             iconRules 
//             |> Map.fold(fun key (cells,icon) -> 
//                 if Set.contains (row,col) cells
//                 then icon
//             )
//             if initSet.ContainsKey (row,col) 
//             then //'⦾' //⦿
//                 match initSet[row,col] with 
//                 | Up -> '▲'
//                 | Dn -> '▼'
//                 | Lt -> '◄'
//                 | Rt -> '►'
//             else if obsSet.Contains (row,col)
//             then '░'
//             else if pathSet.Contains(row,col)
//             then '▇'
//             else maze.[row,col]
//             |> printf "%c "
//         printf "%03d" row
//         if row < rows then printfn ""
//     printColIndex()
//     printfn ""
   
    