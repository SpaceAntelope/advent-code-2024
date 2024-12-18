open System
open System.IO

#load "../global.fsx"

type State = {
    A: int64
    B: int64
    C: int64
    Instructions: int list
    Index: int
    Out: int64 list
}

let parse path = 
    let lines =
        path 
        |> File.ReadAllLines
    {   A = lines.[0].Split(' ') |> Array.last |> int64
        B = lines.[1].Split(' ') |> Array.last |> int64
        C = lines.[2].Split(' ') |> Array.last |> int64
        Index = 0
        Instructions = 
            lines.[4].Split(' ') 
            |> Array.last 
            |> _.Split(',') 
            |> List.ofArray 
            |> List.map int
        Out = []    }

let evaluateComboOperand (op: int64) (state: State) =
    match op with
    | x when x >=0 && x <= 3 -> x |> int64
    | 4L -> state.A
    | 5L -> state.B
    | 6L -> state.C
    | x -> failwithf "Invalid operand %d" x

let mutable printcommands = true
let mutable fmt = 10
let b (i: int64) =
        match fmt with
        | 10 -> string i
        | 8 -> Convert.ToString(i,8)
        | 2 -> Convert.ToString(i,2)
        | _ -> failwithf "sure"

let evaluateCommand (opcode: int) (literalOperand: int64) (state: State) =
    let comboValue = evaluateComboOperand literalOperand state
    
    let prnt cmd =
        if printcommands 
        then 
            [   sprintf "%2d: %s C:%d L:%d" state.Index cmd comboValue literalOperand |> _.PadRight(40)
                sprintf "A: %-16s B: %-16s C: %-16s"  (b state.A) (b state.B) (b state.C) //|> _.PadRight(75)
                sprintf " %A" (state.Out |> List.map b) ] 
            |> List.reduce(+) |> printfn "%s"

    match opcode with
    | adv when adv = 0 -> 
        prnt "adv"
        { state with A = state.A / int64 (Math.Pow(2.0, float comboValue)); Index = state.Index + 2}
    | bxl when bxl = 1 -> 
        prnt "bxl"
        { state with B = state.B ^^^ literalOperand ; Index = state.Index + 2}
    | bst when bst = 2 -> 
        prnt "bst"
        { state with B = comboValue % 8L ; Index = state.Index + 2}
    | jnz when jnz = 3 -> 
        prnt "jnz"
        if printcommands then printfn ""
        if state.A <> 0 
        then { state with Index = int literalOperand } 
        else { state with Index = state.Index + 2 }
    | bxc when bxc = 4 -> 
        prnt "bxc"
        { state with B = state.B ^^^ state.C; Index = state.Index + 2}
    | out when out = 5 -> 
        prnt "out"
        { state with Out = [ yield! state.Out; yield comboValue % 8L]; Index = state.Index + 2}
    | bdv when bdv = 6 -> 
        prnt "bdv"
        { state with B = state.A / int64 (Math.Pow(2.0, float comboValue)); Index = state.Index + 2}
    | cdv when cdv = 7 -> 
        prnt "cdv"
        { state with C = state.A / int64 (Math.Pow(2.0, float comboValue)); Index = state.Index + 2}
    | x -> failwithf "Unknown opcode %d" x


let bespokeComputation (state: State) =
    let b,c = 
        let b1 = (state.A % 8L) ^^^ 2L // ^^^ 3L)
        let c = state.A / int64 (Math.Pow(2.0, float b1))
        let b2 = b1 ^^^ 3L  
        b2 ^^^ c, c
    
    { state with 
        A = state.A / 8L
        B = b
        C = c
        Out = state.Out@[int64(b%8L)] }

let printState (state: State) = 
    printf "A: %-16s B: %-16s C: %-16s"  (b state.A) (b state.B) (b state.C) //|> _.PadRight(75)
    printfn "\t%A" state.Out

let runBespokeProgram (computerState: State) = 
    let mutable state = computerState
    printState state
    while state.A <> 0 do
        state <- bespokeComputation state
        printState state

    state
    // List.unfold (fun state -> 
    //     let newState = shortComputation state
    //     if newState.A = 0 
    //     then None
    //     else Some(newState,newState)
    // ) computerState
    // |> List.last

let next (state: State) =
    if state.Index >= state.Instructions.Length
    then None
    else 
        let opcode = state.Instructions.[state.Index]
        let operand = state.Instructions.[state.Index+1]
        // printfn "opcode: %d operand: %d index: %d" opcode operand state.Index
        Some (evaluateCommand opcode operand state)
    
let runProgram (state: State) =
    state 
    |> List.unfold (next >> (Option.map (fun nextState -> nextState,nextState)))    
    |> List.last

// let runTargetedProgram (state: State) (expected: int list) =
//     let mutable finalState = Some {state with A = -1 }
//     let mutable progOutput = []
//     while progOutput <> expected do        
//         let mutable nextState = Some { state with A = nextState.A + 1}
//         while nextState.IsSome do                
//             nextState <- 
//                 next nextState
//                 |> Option.bind (fun ns -> 
//                     if ns.Out = expected |> List.take ns.Out.Length 
//                     then Some ns
//                     else None
//                 )
//         progOutput <- 
//             match nextStatae with

let runTargetedProgram (state: State) (expected: int64 list) =
    // let expected = state.Instructions
    state 
    |> List.unfold (fun nextState ->
            next nextState
            |> Option.bind (fun ns ->
                //printfn "%d %A" ns.Index ns.Out
                if ns.Out.IsEmpty 
                then Some(ns,ns)
                else if ns.Out = (expected |> List.take ns.Out.Length)
                then Some(ns,ns)
                else None)
                )
    |> List.last

printfn "%A" fsi.CommandLineArgs

if fsi.CommandLineArgs.Length > 1
then 
    let showStack() = 
        "./input.actual"
        |> parse
        // |> fun state -> {state with A = 137438953472L }
        |> fun state -> {state with A = int64 fsi.CommandLineArgs.[1] }
        |> fun state -> 
            runBespokeProgram state |> ignore
            runProgram state 
        |> fun state -> state.Out |> List.map string |> fun x -> String.Join (',', x)
        |> printfn "Joined output is %s"

    showStack()
    fmt <- 8
    showStack()
    fmt <- 2
    showStack()

    0L
else 

    // "./input.example"
    // |> parse
    // |> runShortProgram
    // |> fun state -> state.Out
    // |> Global.shouldBe [4;6;3;5;6;3;5;2;1;0]

    printfn "-------"


    "./input.actual"
    |> parse
    // |> fun   state -> {state with A = 549755813888L }
    // |> runShortProgram
    |> runProgram
    |> _.Out 
    |> List.map string
    |> List.reduce(sprintf "%s,%s") 
    |> Global.shouldBe "3,7,1,7,2,1,0,6,3"

    "./input.actual"
    |> parse
    // |> fun   state -> {state with A = 549755813888L }
    |> runBespokeProgram
    |> _.Out 
    |> List.map string
    |> List.reduce(sprintf "%s,%s") 
    |> Global.shouldBe "3,7,1,7,2,1,0,6,3"

    // 0L
    let solve (targetOutput: int64[]) =
        let b A = 
            let b1 = (A % 8L) ^^^ 2L // ^^^ 3L)
            let c = A / int64 (Math.Pow(2.0, float b1))
            let b2 = b1 ^^^ 3L  
            b2 ^^^ c
        let output A = (b A)%8L
        
        printfn "%A" targetOutput

        let rec solve' (depth: int) (path: int64 list) =
            if path <> [] 
            then 
                printf "\nDepth %d Path: %A Target: %A" depth path targetOutput.[depth..]
                path |> List.reduce(fun s c -> s*8L+c) |> fun x -> printfn " A = %d Output: %A Next: %A" x (output x) targetOutput.[depth-1]
            // else printfn ""
            
            [
                if depth = 0 
                then path
                else
                    yield! 
                        [0L..7L] 
                        |> List.map (fun i -> i, path |> List.fold (fun s c -> s * 8L + c) i)
                        // |> Global.tee "mapping: "
                        |> List.filter (fun (octalDigit,A) -> 
                            printfn "A: %d Out: %A Target: %A" A (output A) targetOutput.[depth-1]
                            output A = targetOutput.[depth-1])
                        // |> Global.tee "filtered: "
                        |> List.collect (fun (octalDigit,totalA) -> solve' (depth-1) (path |> List.append [octalDigit]))                
                        // |> Global.tee "collected: "
            ]

        solve' targetOutput.Length []
        // { state with 
        //     A = state.A / 8L
        //     B = b
        //     C = state.A/8L
        //     Out = state.Out@[b%8L] }
        //     for i in 0..7 do 
    
    solve [|2L;4L;1L;2L;7L;5L;1L;3L;4L;3L;5L;5L;0L;3L;3L;0L|]
    |> printfn "%A"


    666L




    // "./input.actual"
    // |> parse
    // // |> fun state -> {state with A = 137438953472L }
    // |> fun state -> {state with A = 40210765889012L }
    // |> fun state -> {state with A = 80421323455988L }
    // |> runProgram
    // |> fun state -> state.Out |> List.map string |> fun x -> String.Join (',', x)
    // |> printfn "Joined output is %s"

    // printcommands <- false

    // "./input.actual"
    // |> parse
    // |> fun state ->
    //     // let mutable A = 550510000000L
    //     let mutable A = 1L
    //     let mutable finalState = { state with A = A-1L }
    //     let expected = state.Instructions |> List.map int64
        
    //     while finalState.Out <> expected do
    //         A <- A + 1L
    //         finalState <- runTargetedProgram { state with A = A } expected
    //         if A % 10000000L = 0 then printfn "A: %d Out: %A" finalState.A finalState.Out
    //         // if finalState.Out <> [] then printfn "A: %d Out: %A" A finalState.Out
        
    //     printfn "Done! A: %d Out: %A" finalState.A finalState.Out

    // let dfs (initA: int64 list) (state: State) = 
    //     let expected = state.Instructions |> List.map int64
    //     let mutable longestOut = 0
    //     let rec dfs' (depth:int)  (octA: int64 list ) =     

    //         [0L..7L] 
    //         |> List.reduce(fun s c ->                 
    //                 // printfn "%d %A" depth octA
    //                 let updatedOctA = octA |> List.updateAt depth c
    //                 let decA = updatedOctA |> List.reduce (fun s' c' -> s' * 8L + c')
    //                 let newState = runTargetedProgram { state with A = decA } expected

    //                 if newState.Out.Length > longestOut 
    //                 then 
    //                     printfn "Longest out: %d %A for %d (%s)" newState.Out.Length newState.Out decA (updatedOctA |> List.fold (fun s c -> s + string c) "")
    //                     longestOut <- newState.Out.Length

    //                 if newState.Out = expected
    //                 then decA
    //                 else if depth < 8//expected.Length-1
    //                 then dfs' (depth+1) updatedOctA
    //                 else 0L)
                    

    //     // List.replicate (state.Instructions.Length) 0L
    //     // |> dfs' 0
    //     dfs' 0 initA

    // let dfs2 (state: State) =
    //     let expected = state.Instructions |> List.map int64
    //     let mutable longestOut = 0
    //     let rec dfs' (depth:int) (octA: int64 list ) =        
    //         [0L..7L] 
    //         |> List.reduce(fun s c ->                 
    //                 // printfn "%d %A" depth octA
    //                 let updatedOctA = octA |> List.updateAt depth c
    //                 let decA = updatedOctA |> List.reduce (fun s' c' -> s' * 8L + c')
    //                 let newState = runTargetedProgram { state with A = decA } expected

    //                 if newState.Out.Length > longestOut 
    //                 then 
    //                     printfn "Longest out: %d %A for %d (%s)" newState.Out.Length newState.Out decA (updatedOctA |> List.fold (fun s c -> s + string c) "")
    //                     longestOut <- newState.Out.Length

    //                 if newState.Out = expected
    //                 then decA
    //                 else if depth < expected.Length-1
    // //                 then dfs' (depth+1) updatedOctA
    // //                 else 0L)
                    

    // //     List.replicate (state.Instructions.Length) 0L
    // //     |> dfs' 0


    // "./input.actual"
    // |> parse
    // // |> dfs [4;0;2;1;0;7;6;5;8;8;9;0;1;2]
    // |> dfs [0;0;0;0;0;0;7;4;1;0;4;3;0;8]



    // let rec count depth current = 
    //     if depth = 0
    //     then [current]
    //     else 
    //         [0..7]
    //         |> List.collect(fun i -> count (depth-1) (current * 8 + i))

    // [1..7] 
    // |> List.collect (count 14)
    // |> List.distinct
    // |> fun k ->
    //         {|  A = k |> List.length
    //             B = k |> List.min
    //             C = k |> List.max   |}

// let validator  (target: int64 list) (A:int64) = 
//     let tentative = target |> List.mapi (fun i _ -> output <| int64 (A / int64 (Math.Pow(8.0, float i))))
//     // printfn "%A" tentative
//     tentative = target