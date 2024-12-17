open System
open System.IO

#load "../global.fsx"

type State = {
    A: int
    B: int
    C: int
    Instructions: int list
    Index: int
    Out: int list
}

let parse path = 
    let lines =
        path 
        |> File.ReadAllLines
    {
        A = lines.[0].Split(' ') |> Array.last |> int
        B = lines.[1].Split(' ') |> Array.last |> int
        C = lines.[2].Split(' ') |> Array.last |> int
        Index = 0
        Instructions = 
            lines.[4].Split(' ') 
            |> Array.last 
            |> _.Split(',') 
            |> List.ofArray 
            |> List.map int
        Out = []
    }

let evaluateComboOperand (op: int) (state: State) =
    match op with
    | x when x >=0 && x <= 3 -> x
    | 4 -> state.A
    | 5 -> state.B
    | 6 -> state.C
    | x -> failwithf "Invalid operand %d" x

let evaluateCommand (opcode: int) (literalOperand: int) (state: State) =
    let comboValue = evaluateComboOperand literalOperand state
    match opcode with
    | adv when adv = 0 -> { state with A = state.A / int (Math.Pow(2.0, float comboValue)); Index = state.Index + 2}
    | bxl when bxl = 1 -> { state with B = state.B ^^^ literalOperand ; Index = state.Index + 2}
    | bst when bst = 2 -> { state with B = comboValue % 8 ; Index = state.Index + 2}
    | jnz when jnz = 3 -> 
        if state.A <> 0 
        then { state with Index = literalOperand } 
        else { state with Index = state.Index + 2 }
    | bxc when bxc = 4 -> { state with B = state.B  ^^^ state.C; Index = state.Index + 2}
    | out when out = 5 -> { state with Out = [ yield! state.Out; yield comboValue % 8]; Index = state.Index + 2}
    | bdv when bdv = 6 -> { state with B = state.A / int (Math.Pow(2.0, float comboValue)); Index = state.Index + 2}
    | cdv when cdv = 7 -> { state with C = state.A / int (Math.Pow(2.0, float comboValue)); Index = state.Index + 2}
    | x -> failwithf "Unknown opcode %d" x


let next (state: State) =
    if state.Index >= state.Instructions.Length
    then None
    else 
        let opcode = state.Instructions.[state.Index]
        let operand = state.Instructions.[state.Index+1]
        // printfn "opcode: %d operand: %d index: %d" opcode operand state.Index
        Some (evaluateCommand opcode operand state)
        

"./input.example"
|> parse
|> List.unfold (next >> (Option.map (fun nextState -> nextState,nextState)))
|> List.last
|> fun state -> state.Out
|> Global.shouldBe [4;6;3;5;6;3;5;2;1;0]


"./input.actual"
|> parse
|> List.unfold (next >> (Option.map (fun nextState -> nextState,nextState)))
|> List.last
|> fun state -> state.Out |> List.map string |> fun x -> String.Join (',', x)
|> printfn "Joined output is %s"
