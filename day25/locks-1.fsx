open System
open System.IO
open System.Text.RegularExpressions

#load "../global.fsx"
type Schematic = Key | Lock

let parse path =
    path
    |> File.ReadAllLines
    |> Array.chunkBySize 8
    |> Array.map (fun lines -> 
        let schematicKind = if lines.[0].[0] = '#' then Lock else Key
        let schematicNums = 
            lines
            |> Array.filter (String.IsNullOrEmpty>>not)
            |> Array.map _.ToCharArray() 
            |> Array.transpose 
            |> Array.map (fun column -> (column |> Array.filter (fun c -> c = '#' ) |> Array.length) - 1 )
        schematicKind, schematicNums)

let compareLocksAndKeys (schematics : (Schematic*int array)[]) =
    schematics
    |> Array.groupBy fst
    // |> Global.tee "parsed"
    |> fun arr ->
        (snd arr.[0])
        |> Array.allPairs (snd arr.[1])
        |> Array.filter (fun (key,lock) -> 
            // printfn "%A %A" key lock
            let s1 = snd key
            let s2 = snd lock
            s1 
            |> Array.zip s2 
            |> Array.forall (fun (k,l) -> 
                // printfn "%d %d = %d" k l (k+l); 
                k+l<=5)
            )
    |> Array.length

"./input.example"
|> parse
|> compareLocksAndKeys
|> Global.shouldBe 3

"./input.actual"
|> parse
|> compareLocksAndKeys
|> printfn "Found %d unique lock/key pairs fit together without overlapping in any column"


// "./input.example"
// |> parse
// |> Array.groupBy fst
// // |> Global.tee "parsed"
// |> fun arr ->
//     (snd arr.[0])
//     |> Array.allPairs (snd arr.[1])
//     |> Array.filter (fun (key,lock) -> 
//         printfn "%A %A" key lock
//         let s1 = snd key
//         let s2 = snd lock
//         s1 
//         |> Array.zip s2 
//         |> Array.forall (fun (k,l) -> 
//             // printfn "%d %d = %d" k l (k+l); 
//             k+l<=5)
//         )
// |> Array.length
// |> Global.shouldBe 3

