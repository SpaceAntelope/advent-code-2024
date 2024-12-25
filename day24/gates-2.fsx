open System
open System.IO
open System.Text.RegularExpressions

#load "../global.fsx"



type Gate = AND | OR | XOR


type Connection = {
    Left: string
    Right: string
    Gate: Gate
    Output: string
} with 
    override x.ToString() = $"{x.Left} {x.Gate} {x.Right} -> {x.Output}"
    static member From(line: string) = 
            let m = Regex.Match(line,@"(?<s1>\w+) (?<op>AND|OR|XOR) (?<s2>\w+) \-\> (?<s3>\w+)")
            let left = m.Groups["s1"].Value
            let op = 
                match m.Groups["op"].Value with 
                | x when x = "OR" -> OR 
                | x when x = "AND" -> AND 
                | x when x = "XOR" -> XOR 
                | x -> failwithf "Unknown operator '%s'" x
            let right = m.Groups["s2"].Value
            let out = m.Groups["s3"].Value
            {
                Left = left 
                Right = right
                Gate = op
                Output = out
            }

(* 
    To find the problem nodes use convert-input-to-dot-notation.ps1 to visualize the puzzle input,
    which has a very obvious patent, even if you don't know it's called a ripple adder.

    The evaluation rules below were enough to yield all problematic instances, that are trivially
    to manually check against the graphviz svg.
*)

let connections = 
    "./input.actual"
    |> File.ReadAllLines
    |> Array.skipWhile (fun line -> not <| Regex.IsMatch(line, "AND|OR"))
    |> Array.map Connection.From
                
let outIndex = 
    connections 
    |> Array.map (fun con -> con.Output, con)
    |> readOnlyDict

(* ripple adder circuit evaluation rules *)
printfn "Problematic gates:"

let ```Result digits (zxx) are always output of XOR except for the least significant digit`` =
    connections 
    |> Array.filter (fun con ->
        con.Output.StartsWith("z") && (con.Gate = AND || con.Gate = OR ) && con.Output <> "z45")
    |> Array.iter(printfn "%O")

let ```All OR gates must get their input ony from AND gates``=
    connections 
    |> Array.filter (fun con -> 
            con.Gate = OR
            && (outIndex.[con.Left].Gate <> AND || outIndex.[con.Right].Gate <> AND))
    |> Array.iter(printfn "%O")
printfn "--------"

(* Modified 24-1 to evaluate different swap sets *)
let parse  (swaps : Map<string,string>) (lines:string[]) =    
    lines
    |> Array.map (fun line -> 
        if line.Contains("->")
        then 
            let conn = Connection.From line
        
            if swaps |> Map.containsKey conn.Output
            then { conn with Output = swaps.[conn.Output] }.ToString()
            else conn.ToString()
        else 
            line    
        )
    |> Array.filter (String.IsNullOrEmpty>>not)
    |> Array.partition _.Contains(':')
    |> fun (eval, uneval) ->
        let unevalIndex = 
            uneval
            |> Array.map (fun line -> 
                let m = Regex.Match(line,@"(?<s1>\w+) (?<op>AND|OR|XOR) (?<s2>\w+) \-\> (?<s3>\w+)")
                let s1 = m.Groups["s1"].Value
                let op = 
                    match m.Groups["op"].Value with 
                    | x when x = "OR" -> OR 
                    | x when x = "AND" -> AND 
                    | x when x = "XOR" -> XOR 
                    | x -> failwithf "Unknown operator '%s'" x
                let s2 = m.Groups["s2"].Value
                let s3 = m.Groups["s3"].Value
                s3, fun (evalIndex) -> 
                    let operands=
                        evalIndex 
                        |> Map.tryFind s1
                        |> Option.bind (fun s -> 
                            match evalIndex |> Map.tryFind s2 with 
                            | Some x -> Some(s,x) 
                            | _ -> None)

                    match operands, op with
                    | None, _ -> None
                    | Some (a,b), XOR -> Some (a ^^^ b)
                    | Some (a,b), AND -> Some (a &&& b)
                    | Some (a,b), OR -> Some (a ||| b) ) 
            |> Map
        unevalIndex


let fireCircuit (evaluated: Map<string,int64>) (notEvaluated: Map<string,(Map<string,int64> -> int64 option)>) =
    let mutable evaluated' = evaluated
    let mutable notEvaluated' = notEvaluated
    while notEvaluated'.Count > 0 do        
        notEvaluated' <- 
            notEvaluated' 
            |> Map.filter(fun key f ->  
                match f(evaluated') with
                | Some result -> 
                    evaluated' <- evaluated' |> Map.add key result
                    false
                | None -> true)

    evaluated'.Keys 
    |> Seq.filter (fun key-> key.StartsWith("z") )
    |> Seq.sortDescending
    |> Seq.map (fun key -> evaluated'.[key])
    |> Seq.fold(sprintf "%s%d") ""
    |> fun x -> printfn "= %s" x; x
    |> fun str -> Convert.ToInt64(str, 2)
    
let int2evalIndex (length: int) (x: int64) (y: int64) = 
    printfn "Testing for %d + %d = %d" x y (x+y)
    printfn "which is:"
    printfn "  %s" <| Convert.ToString(x, 2).PadLeft(45,'0')
    printfn "+ %s" <| Convert.ToString(y, 2).PadLeft(45,'0')
    let map (n: int64) (l: string) =
        Convert.ToString(n, 2)
        |> _.PadLeft(length, '0')
        |> _.ToCharArray() 
        |> Array.rev 
        |> Array.fold (fun (i,map) c -> 
            i+1, map |> Map.add $"{l}%02d{i}" (int64 c - 48L)) (0,Map.empty<string,int64>)
        |> snd
    
    let xIndex = map x "x"
    let yIndex = map y "y"

    Map(Seq.concat [ (Map.toSeq xIndex) ; (Map.toSeq yIndex) ])


(* only correct for my input *)
let swapsList = ["z18";"qgd";"z10";"mwk";"z33";"gqp";"jmh";"hsw"] 
let swaps = 
    swapsList 
    |> List.chunkBySize 2 
    |> List.collect (fun arr -> [arr[0],arr[1];arr[1],arr[0]])
    |> Map

let adderCustomInputs= 
    (* 
        Test with numbers that use all 45 registers and are easy to make sense in binary.
        If the result is expected to be almost all ones or all zeroes you can probably use
        concentrate your swaps to the regions where that stops being the case to come to 
        a fully automated solution instead of a mostly manual one like this.

        The numbers here in binary are:    
          011111111111111111111111111111111111111111111
        + 000000000000000000000000000000000000000000001
        = 100000000000000000000000000000000000000000000
    *)
    int2evalIndex 45 17592186044415L 1L

"./input.actual"
|> File.ReadAllLines
|> parse swaps
|> fireCircuit adderCustomInputs
|> Global.shouldBe 17592186044416L

swapsList 
|> List.sort 
|> List.reduce (sprintf "%s,%s") 
|> printfn "If you sort the names of the eight wires involved in a swap and then join those\nnames with commas you get: %s"
