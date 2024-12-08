let shouldBe expected actual= 
    if expected <> actual 
    then 
        failwithf "Comparison between expected %A and actual %A failed." expected actual

let matrixIndices matrix =
    let rows = matrix |> Array2D.length1
    let cols = matrix |> Array2D.length2
    ([0..rows-1],[0..cols-1])
    ||> Seq.allPairs