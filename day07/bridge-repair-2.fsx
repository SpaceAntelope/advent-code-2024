open System
open System.IO

#load "../global.fsx"
#load "./common.fsx"

type Ops = Add | Times | Concat
type SearchResult = 
    | TotalExceeded 
    | CompleteWithSmallerResult 
    | CompleteWithCorrectResult 
    | RanOutOfCalibrationVaules

let isLegit ((total: int64), (calibration : int64 array)) = 

    printf "%d %A" total calibration
    let rec search' (index : int) (acc: int64) =

        let currentValue = calibration.[index]
        
        [Add; Times; Concat] 
        |> List.exists (fun op -> 
                printf "\nidx: %d -- %d %A %d " index acc op currentValue
                match op with
                | Add -> acc + currentValue
                | Times -> acc * currentValue
                | Concat -> int64 (sprintf "%d%d" acc currentValue)
                |> fun x -> 
                    printf "-> %d " x
                    x
                |> function 
                | state when state > total -> false
                | state when state = total && index < (calibration.Length-1) -> false
                | state when state = total && index = (calibration.Length-1) -> true
                | state -> 
                    if index+1 = calibration.Length
                    then false
                    else search' (index+1) state
                |> fun x -> 
                    printf "%b" x
                    x
        )
        
    search' 1 calibration.[0]

// "./input.example"
// |> Common.parse
// |> Array.map isLegit
// |> Global.shouldBe [|true;true;false;true;true;false;true;false;true;true|]

// "./input.example"
// |> Common.parse
// |> Array.filter isLegit
// |> Array.sumBy fst
// |> Global.shouldBe 11387

"./input.actual"
|> Common.parse
|> Array.filter (fun (total,_) -> total < 1000)
|> Array.filter (fun (total,_) -> [916L;930L;641L;113L;984L;422L;588L;527L] |> List.contains total)
//|> Array.filter (isLegit>>not)
|> (Array.last>>Array.singleton)
|> Array.map isLegit
//|> Array.iter (fun (t,c) -> printfn "%d %A" t c)
// |> Array.sumBy fst
// |> printfn "Sum of evaluatable calibrations is %d"

