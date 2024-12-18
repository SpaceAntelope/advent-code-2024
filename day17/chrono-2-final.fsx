open System
open System.IO

(* Compressed version of the puzzle instructions, probably specific to my input *)
let output A = 
    let b = 
        let b1 = (A % 8L) ^^^ 2L
        let c = A / int64 (Math.Pow(2.0, float b1))
        let b2 = b1 ^^^ 3L  
        b2 ^^^ c
    b%8L

let validate (target: int64[]) (A:int64) = 
    target 
    |> Array.mapi (fun index targetElement -> targetElement, output <| int64 (A / int64 (Math.Pow(8.0, float index))))
    |> Array.forall (fun (expected, actual) -> expected = actual)

let bruteForceSearch (target: int64[]) =
    let maxValue = 10.0**(float target.Length+1.0) - 1.0  |> int64
    seq {0L..maxValue} |> Seq.find (validate target)

"./input.actual"
|> File.ReadAllLines
|> Array.skip 4
|> Array.last
|> _.Split(' ')
|> Array.last
|> _.Split(',')
|> Array.map int64
|> Array.splitInto 2
|> Array.map bruteForceSearch 
|> Array.map (fun x -> Convert.ToString(x,8))
|> Array.reduce(fun state current -> sprintf "%s%s" current state)
|> fun x -> Convert.ToInt64(x,8)
|> printfn "The lowest positive initial value for register A that causes the program to output a copy of itself is %d"