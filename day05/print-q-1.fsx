open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

#load "../global.fsx"

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
    |> List.filter (fun r -> r.Length > 1)
    |> List.collect(List.pairwise)
    |> List.distinct

let sumMiddleElementsOfValidUpdates (updates : int[][])  (ruleset: (int*int) list) =

    let validateUpdate (update: int array) = 
        update |> Array.pairwise |> Array.forall (fun pair -> ruleset |> List.contains pair)

    let middleElement (update: int array) =
        update 
        |> Array.length
        |> fun ln -> ln/2
        |> Array.get update 

    updates
    |> Array.filter validateUpdate
    |> Array.map middleElement
    |> Array.sum



"./day05/input.example"
|> parseInput
|> fun (pages, rules, updates) ->
    (pages, rules)
    ||> generateExpandedRuleset
    |> sumMiddleElementsOfValidUpdates updates
|> Global.shouldBe 143


"./day05/input.actual"
|> parseInput
|> fun (pages, rules, updates) ->
    (pages, rules)
    ||> generateExpandedRuleset
    |> sumMiddleElementsOfValidUpdates updates
|> printfn "The sum of the middle element of each valid update is %d"
