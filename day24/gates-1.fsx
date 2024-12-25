open System
open System.IO
open System.Text.RegularExpressions

#load "../global.fsx"

type Operators = AND | OR | XOR

let parse path =    
    path
    |> File.ReadAllLines
    |> Array.filter (String.IsNullOrEmpty>>not)
    |> Array.partition _.Contains(':')
    |> fun (eval, uneval) ->
        let evalIndex = 
            eval
            |> Array.map (fun line -> 
                let arr = line.Split(':'); 
                arr.[0],int64 (arr.[1].Trim()))
            |> Map

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
                    //printfn "(%s, %s) %A %A -> %s" s1 s2 operands op s3 
                    match operands, op with
                    | None, _ -> None
                    | Some (a,b), XOR -> Some (a ^^^ b)
                    | Some (a,b), AND -> Some (a &&& b)
                    | Some (a,b), OR -> Some (a ||| b) ) 
            |> Map
                    // |> function
                    // | Some x -> evalIndex |> Map.add s3 x
                    // | None -> evalIndex
        evalIndex, unevalIndex
    
let fireCircuit (evaluated: Map<string,int64>) (notEvaluated: Map<string,(Map<string,int64> -> int64 option)>) =
    let mutable evaluated' = evaluated
    let mutable notEvaluated' = notEvaluated
    let orderOfApplication = ResizeArray<string>()
    while notEvaluated'.Count > 0 do
        
        notEvaluated' <- 
            notEvaluated' 
            |> Map.filter(fun key f ->  
                match f(evaluated') with
                | Some result -> 
                    orderOfApplication.Add key
                    evaluated' <- evaluated' |> Map.add key result
                    false
                | None -> true)
        //printfn "%d %d" evaluated'.Count notEvaluated'.Count
        //evaluated'.Keys |> Seq.sort |> Seq.iter (fun key -> printfn "%A = %A" key evaluated'.[key])
    
    File.WriteAllLines("./order-of-application.actual", orderOfApplication)

    evaluated'.Keys 
    |> Seq.filter (fun key-> key.StartsWith("z") )
    |> Seq.sortDescending
    |> Seq.map (fun key -> evaluated'.[key])
    |> Seq.fold(sprintf "%s%d") ""
    |> fun str -> Convert.ToInt64(str, 2)
    
    
        
"./input.example1"
|> parse        
||> fireCircuit
|> Global.shouldBe 4

"./input.example2"
|> parse        
||> fireCircuit
|> Global.shouldBe 2024

"./input.actual"
|> parse        
||> fireCircuit
|> printfn "The decimal it outputs on the wires starting with z is %d"