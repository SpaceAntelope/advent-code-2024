open System
open System.IO
open System.Collections.Generic

let parseInput (inputPath: string) =

    let (rulesUnparsed, updatesUnparsed) =
        inputPath
        |> File.ReadAllLines
        |> Array.filter (String.IsNullOrEmpty>>not)
        |> Array.partition (fun line -> line.Contains("|"))    

    let rules = 
        rulesUnparsed
        |> Array.map (fun line ->
            let pages = line.Split('|')
            int pages.[0], int pages.[1])

    let pages = 
        rules 
        |> Array.collect (fun (x,y) -> [|x;y|]) 
        |> Array.distinct

    let updates =
        updatesUnparsed
        |> Array.map _.Split(',')
        |> Array.map (Array.map int)

    printfn "Found %d pages, %d rules, %d updates" pages.Length rules.Length updates.Length

    pages, rules, updates


(* Forgot to expand the branch, and also the whole thing is unneccessary, 
 * it was enough to just use the original rules
 *) 
let generateExpandedRuleset (pages: int[]) (rules: (int*int)[]) =
    let ruleIdx = 
        rules 
        |> Array.groupBy fst 
        |> Array.map (fun (key,grp)-> 
            key, grp |> Array.map snd)
        |> readOnlyDict

    let rec expand (node:int) (branch : int list) (visited : System.Collections.Generic.HashSet<int>) =
        visited.Add node |> ignore

        let newNodes =
            match ruleIdx.TryGetValue node with
            | true, grp -> 
                grp
                |> Array.except visited        
            | _ -> [||]
            |> List.ofArray        

        let newBranch = branch |> List.insertAt branch.Length node 
        
        if newNodes.Length = 0 
        then 
            [ newBranch ]
        else
            newNodes
            |> List.collect (fun n -> expand n newBranch visited)

    pages
    |> List.ofArray 
    |> List.collect (fun page -> expand page [] (HashSet<int>()))
    |> List.collect(List.pairwise)
    |> List.distinct

let middleElement (update: int array) =
    update 
    |> Array.length
    |> fun ln -> ln/2
    |> Array.get update 