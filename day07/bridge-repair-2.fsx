open System
open System.IO

#load "../global.fsx"
#load "./common.fsx"

type Operators = Add | Times | Concat
type SearchResult = 
    | TotalExceeded 
    | CompleteWithSmallerTotal 
    | CompleteWithCorrectTotal 
    | NotEnouhgValuesToReachTotal
    | EndOfSearch

let isLegit ((total: int64), (calibration : int64 array))  = 

    //printf "\n%d %A" total calibration
    let rec search' (index : int) (acc: int64) : SearchResult=

        let currentValue = calibration.[index]
        
        [Add; Times; Concat] 
        |> Seq.map (fun op -> 
                //printf "\nidx: %d -- %d %A %d " index acc op currentValue
                match op with
                | Add -> acc + currentValue
                | Times -> acc * currentValue
                | Concat -> int64 (sprintf "%d%d" acc currentValue)
                |> fun x -> 
                    //printf "-> %d " x
                    x
                |> function 
                | state when state > total -> TotalExceeded
                // | state when state = total && index < (calibration.Length-1) -> CompleteWithSmallerTotal
                | state when state = total && index = (calibration.Length-1) -> CompleteWithCorrectTotal
                | state when index + 1 = calibration.Length -> NotEnouhgValuesToReachTotal
                | state -> search' (index+1) state
                |> fun x -> 
                   // printf "%A:" x
                    x
        ) 
        |> Seq.tryFind (fun result -> result = CompleteWithCorrectTotal)
        |> function
        | Some x -> x
        | None -> EndOfSearch
        
    (search' 1 calibration.[0]) = CompleteWithCorrectTotal

let example() = 
    "./input.example"
    |> Common.parse
    |> Array.map isLegit
    |> Global.shouldBe [|true;true;false;true;true;false;true;false;true;true;false|]
   
let example'() =
    "./input.example"
    |> Common.parse
    |> Array.take 9
    |> Array.filter isLegit
    |> Array.sumBy fst
    |> Global.shouldBe 11387

example()

example'()

"./input.actual"
|> Common.parse
|> Array.filter isLegit
|> Array.sumBy fst
|> printfn "Sum of evaluatable calibrations is %d"


