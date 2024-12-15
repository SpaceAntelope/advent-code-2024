open System.Text.RegularExpressions
let shouldBe expected actual= 
    if expected <> actual 
    then 
        failwithf "Comparison between expected %A and actual %A failed." expected actual

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