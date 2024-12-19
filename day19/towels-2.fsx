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

let mutable count = 0

let countPossible (genepool: string[]) (target: string) =
    let relevantGenes = 
        genepool 
        |> Array.filter (fun gene -> target.Contains gene)

    let rec search index =
        if index = target.Length
        then 
            count <- count + 1
            if count % int 1e6 = 0 then printfn "count: %d" count
            1
        else 
            relevantGenes 
            |> Seq.filter (fun gene -> 
                gene.[0] = target.[index] 
                && target.Length - index >= gene.Length
                && gene = target.Substring(index,gene.Length))
            |> Seq.sumBy (fun gene -> search (index + gene.Length))

    search 0
    |> Global.tee target

let synthesize (genepool: string[]) (target: string) =
    
    let relevantGenes = genepool |> Array.filter (fun gene -> target.Contains gene)
    
    // printfn "%d %d" genepool.Length relevantGenes.Length

    let rec search index (genome : string list) =
        
        //printfn "%d %A %s %s" index genome target (genome |> String.concat "")
        // printfn "%s" (genome |> String.concat "|")
        seq {            
            if target = (genome |> String.concat "")
            then 
                yield genome                
            else if index >= target.Length
            then yield! []
            else
                yield! 
                    relevantGenes 
                    |> Seq.filter (fun gene -> 
                        gene.[0] = target.[index] 
                        && target.Length - index >= gene.Length
                        && gene = target.Substring(index,gene.Length) 
                        )
                    // |> Global.tee "synths: "                    
                    |> Seq.collect (fun gene -> search (index + gene.Length) (genome@[gene]) )
        }
    search 0 []



    // |> Array.ofSeq
    // |> fun x -> 
    //         count <- count + 1
    //         printfn "Currently on %d, found %d solutions" count (x.Length)
    //         x




parse "./input.example"
|> fun (patterns,designs) ->
    designs    
    |> Array.map ((synthesize patterns))
    |> Array.map (Seq.length)
    |> Global.shouldBe [|2;1;4;6;0;1;2;0|]

parse "./input.example"
|> fun (patterns,designs) ->
    designs    
    |> Array.map (countPossible patterns)
    |> Global.shouldBe [|2;1;4;6;0;1;2;0|]


// exit 666

let unsolvable = 
    "./unsolvable.designs"
    |> File.ReadAllLines

// count <- 0

let test (genepool: string[]) (target: string) =
    
    let relevantGenes = genepool |> Array.filter (fun gene -> target.Contains gene)    
    
    relevantGenes
    |> Array.countBy (fun g -> g.Length)
    |> Array.sortBy fst
    // target.ToCharArray() 
    // |> Array.pairwise 
    // |> Array.map (fun (x,y) -> sprintf "%c%c" x y)
    // |> Array.filter (fun str -> 
    //     relevantGenes 
    //     |> Array.exists (fun gene -> 
    //         gene.Contains(str)) 
    //     |> not)
    // |> Array.length

parse "./input.actual"
|> fun (patterns,designs) ->

    patterns
    |> Array.countBy (fun g -> g.Length)
    |> Array.sortBy fst
    |> printfn "%A"

    // designs
    // |> Array.filter (fun design -> unsolvable |> Array.contains design |> not) 
    // |> Array.map(fun design -> design, test patterns design)
    // // |> Array.sortBy snd
    // |> Array.iter(printfn "%A")

exit 666

parse "./input.actual"
|> fun (patterns,designs) ->
    designs
    |> Array.filter (fun design -> unsolvable |> Array.contains design |> not)
    // |> Array.take 1
    // |> Array.map(fun str -> str.Substring(0, str.Length/2))
    |> Array.sumBy (fun target ->
        count <- count + 1
        printfn "Design #%d" count
        let halfLength = target.Length/2
        let left = 
            target.Substring(0,halfLength)
            |> countPossible patterns
            |> int64
        let right = 
            target.Substring(halfLength)
            |> countPossible patterns
            |> int64
        
        left * right)
    // |> Array.map (fun x -> async { return (synthesize patterns x)})
    // |> Async.Parallel
    // |> Async.RunSynchronously
    |> printfn "If you add up the number of different ways you could make each design you get %d"