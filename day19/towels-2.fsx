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

let countPossible (genepool: string[]) (target: string) =
    let relevantGenes = 
        genepool 
        |> Array.filter (target.Contains)

    let downstreamResultCache = Dictionary<string*string,int64>()

    let rec search index =
        if index = target.Length
        then 1L
        else 
            relevantGenes 
            |> Seq.filter (fun gene -> 
                gene.[0] = target.[index] 
                && target.Length - index >= gene.Length
                && gene = target.Substring(index,gene.Length))
            |> Seq.sumBy (fun gene -> 
                let remaining = target.Substring(index)
                let key = remaining , gene
                if downstreamResultCache.ContainsKey(key)
                then downstreamResultCache.[key]
                else 
                    let result = search (index + gene.Length)
                    downstreamResultCache.Add(key, result)
                    result)

    search 0


parse "./input.example"
|> fun (patterns,designs) ->
    designs    
    |> Array.map (countPossible patterns)
    |> Global.shouldBe [|2;1;4;6;0;1;2;0|]

parse "./input.actual"
|> fun (patterns,designs) ->
    designs    
    |> Array.sumBy (countPossible patterns)    
    |> printfn "If you add up the number of different ways you could make each design you get %d"
