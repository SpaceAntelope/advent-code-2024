open System
open System.IO

#load "../global.fsx"

let parse (path: string) =
    path
    |> File.ReadAllLines
    |> Array.map (fun line -> 
        let parts = line.Split(':')
        let nums = 
            parts.[1].Trim().Split(' ') 
            |> Array.map int64
            //|> Array.append 'x'
        int64 parts[0],nums)

let isLegit  ((total: int64), (calibration: int64 array)) =
    let operatorCount = Math.Pow(2, float calibration.Length-1.0) - 1.0|> int
    
    // printfn "%d %s" operatorCount (Convert.ToString(operatorCount,2))
    
    let operatorSets =
        [|0..operatorCount|]
        |> Array.map (fun index ->
                Convert
                    .ToString(index,2)
                    .PadLeft(calibration.Length-1,'0')
                    .ToCharArray()
                |> Array.append [|'x'|] )

    // printfn "%A %A\n\t%A\n" total calibration operatorSets

    operatorSets
    |> Array.exists(fun ops ->
            // printfn "current ops: %A" ops
            // printfn "current cbr: %A" calibration
            calibration 
            |> Array.zip ops
            |> Array.fold (fun state (op, num) -> 
                // printfn "%A %A %A" state op num
                match op with
                | 'x' -> num
                | '1' -> state * num
                | '0' -> state + num
                ) 0
            |> fun x -> 
                // printfn "%d" x
                x = total
        )

"./input.example"
|> parse
// |> Array.take 2
|> Array.map isLegit
|> Global.shouldBe [|true;true;false;false;false;false;false;false;true|]

"./input.actual"
|> parse
|> Array.filter isLegit
|> Array.sumBy fst
|> printfn "Sum of evaluatable calibrations is %d"