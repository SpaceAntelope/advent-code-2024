open System
open System.Text.RegularExpressions

let shouldBe expected actual= 
    if expected <> actual 
    then 
        failwithf "Comparison between expected %A and actual %A failed." expected actual

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



// let rec f n = 
//     if n = 0L 
//     then 1L
//     else n * f (n-1L) 

// let comboCount n r= (f n) / ((f r) * f(n-r))

// comboCount 222L 8L