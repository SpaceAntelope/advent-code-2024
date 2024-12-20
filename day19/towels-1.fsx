open System
open System.IO

#load "../global.fsx"
open System.Collections.Generic

let parse path=
    let lines =
        path
        |> File.ReadAllLines

    let patterns = lines.[0].Split(',') |> Array.map _.Trim()
    let designs = lines |> Array.skip 2

    patterns, designs

let synthesize (genepool: string[]) (target: string) =
    let relevantGenes = genepool |> Array.filter (fun gene -> target.Contains gene)
    let rec search index (genome : string list) : string list option  =        
        if target = (genome |> String.concat "")
        then Some genome                
        else if index >= target.Length
        then None
        else
            relevantGenes 
            |> Seq.filter (fun gene -> 
                gene.[0] = target.[index] 
                && target.Length - index >= gene.Length
                && gene = target.Substring(index,gene.Length))
            |> Seq.tryPick (fun gene -> search (index + gene.Length) (genome@[gene]) )

    search 0 []

parse "./input.example"
|> fun (patterns,designs) ->
    designs    
    |> Array.map ((synthesize patterns)>>Option.isSome)
    |> Global.shouldBe [|true;true;true;true;false;true;true;false|]


parse "./input.actual"
|> fun (patterns,designs) ->
    designs    
    |> Array.choose (synthesize patterns)
    |> Array.length
    |> printfn "There are %d possible designs."

// parse "./input.actual"
// |> fun (patterns,designs) ->
//     designs    
//     |> Array.filter ((synthesize patterns)>>Option.isNone)
//     |> fun lines -> File.WriteAllLines("./unsolvable.designs", lines)