open System
open System.IO
open System.Text.RegularExpressions

#load "../global.fsx"
type Schematic = 
    | Key of int[] 
    | Lock of int[]

let parse path =
    path
    |> File.ReadAllLines
    |> Array.chunkBySize 8
    |> Array.map (fun lines -> 
        let schematicNums = 
            lines
            |> Array.filter (String.IsNullOrEmpty>>not)
            |> Array.map _.ToCharArray() 
            |> Array.transpose 
            |> Array.map (fun column -> 
                (column |> Array.filter (fun c -> c = '#' ) |> Array.length) - 1 )
        
        if lines.[0].[0] = '#' 
        then Lock schematicNums 
        else Key schematicNums )

let compareLocksAndKeys (schematics : Schematic array) =
    let locks, keys = 
        schematics 
        |> Array.partition (function Lock _ -> true | _ -> false)
    
    let decon schematic = match schematic with Lock x | Key x -> x

    locks 
    |> Array.map decon
    |> Array.allPairs (keys |> Array.map decon)
    |> Array.filter (fun (lock,key) ->
        
        lock
        |> Array.zip key
        |> Array.forall (fun (k,l) -> k + l <= 5))
    |> Array.length

"./input.example"
|> parse
|> compareLocksAndKeys
|> Global.shouldBe 3

"./input.actual"
|> parse
|> compareLocksAndKeys
|> printfn "Found %d unique lock/key pairs fit together without overlapping in any column"