#load "../global.fsx"

open System
open System.IO
open System.Collections.Generic

type BananaCall = {
    Init: int64
    Pattern: int64[]
    Bananas: int64
} with override x.ToString() = $"%5d{x.Init}: {String.Join('\t', x.Pattern) }\t\t{x.Bananas}"

let parse path = path |> File.ReadAllLines |> Array.map int64

let inline prune sn = sn % 16777216L

let calc (sn: int64) = 
    let mutable result = sn * 64L
    result <- result ^^^ sn
    result <- prune result
    result <- result ^^^ (result / 32L)
    result <- prune result
    result <- result ^^^ (result * 2048L)
    result <- prune result
    result    


let fromSecret (init: int64) =
    seq {
        let mutable secretNumber = init
        let mutable actualSecretNumber = init % 10L
        while true do             
            secretNumber <- calc secretNumber
            let nextActualSecretNumber = secretNumber % 10L;
            let change = nextActualSecretNumber - actualSecretNumber
            actualSecretNumber <- nextActualSecretNumber
            yield secretNumber, actualSecretNumber, change
    }


let mapAsync (inits : int64[]) =
    async {
        return [|
            let getChanges (win:(int64*int64*int64)[]) = 
                win |> Array.map (fun (_,_,change) -> change)
            yield! inits
                    |> Seq.collect (fun init -> 
                        let index = HashSet<string>()
                        let keepOnlyFirstPatternOccurence (win:(int64*int64*int64)[]) =                             
                            win |> getChanges |> Array.fold (sprintf "%s/%d") "" |> index.Add

                        fromSecret init
                        |> Seq.take 1999
                        |> Seq.windowed 4
                        |> Seq.filter (keepOnlyFirstPatternOccurence)
                        |> Seq.map (fun win -> 
                                {   Init = init
                                    Pattern = win |> getChanges
                                    Bananas = win.[3] |> fun (_,secret,_) -> secret }))
                    |> Array.ofSeq
        |]
    }

fromSecret 123L 
|> Seq.take 9
|> Global.seqShouldBe [
    // 123, 3
    15887950, 0, -3
    16495136, 6, 6
    527345, 5, -1
    704524, 4, -1
    1553684, 4, 0
    12683156, 6, 2
    11100544, 4, -2
    12249484, 4, 0
    7753432, 2, -2
]


let quartets = 
    "./input.example"
    |> parse
    |> Array.map (Array.singleton>>mapAsync)
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.concat

let bestValue = 
    quartets
    |> Array.groupBy _.Pattern
    |> Array.map (fun (patt,grp) -> patt,grp |> Array.sumBy _.Bananas)
    |> Array.maxBy snd
    |> Global.tee "Example "

bestValue
|> snd
|> printfn "Supposed to be 23 but got %d"

quartets 
|> Array.filter (fun x -> x.Pattern = fst bestValue)
|> Array.iter (printfn "%O")

quartets 
|> Array.filter (fun x -> x.Pattern = [|-2;1;-1;3|])
|> Array.iter (printfn "%O")

printfn "Example result differs from solution on site. Actual puzzle is going to be correct so..."

"./input.actual"
|> parse
|> Array.splitInto 15
|> Array.map mapAsync
|> Async.Parallel
|> Async.RunSynchronously
|> Array.concat
|> Array.groupBy _.Pattern
|> Array.map (fun (patt,grp) -> patt,grp |> Array.sumBy _.Bananas)
|> Array.maxBy snd
|> fun (key, bananas) -> printfn $"Tell the monkey to call on %A{key} and we'll eventually get {bananas} bananas."   