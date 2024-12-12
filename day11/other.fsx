open System
open System.IO
open System.Diagnostics

let input = File.ReadAllLines(@"./input.actual")

let initialStones =
    input.[0].Split(' ')
    |> Array.map UInt64.Parse

let numStonesWithCache blinks stones =
    let mutable cache : Map<uint64 * int, uint64> = Map.empty

    let rec numStones blinks stone =
        printfn "%d %d" blinks stone
        if blinks = 0 then
            1UL
        else
            match Map.tryFind (stone, blinks) cache with
            | Some length -> length
            | None ->
                let st = stone.ToString()
                let num =
                    match stone with
                    | 0UL -> numStones (blinks - 1) 1UL
                    | _ when st.Length % 2 = 0 ->
                        numStones (blinks - 1) (st.Substring(0, st.Length / 2) |> UInt64.Parse)
                        + numStones (blinks - 1) (st.Substring(st.Length / 2) |> UInt64.Parse)
                    | n -> numStones (blinks - 1) (n * 2024UL)
                cache <- cache |> Map.add (stone, blinks) num
                num
    
    stones
    |> Array.map (numStones blinks)
    |> Array.sum
    |> fun x -> 
        cache |> Map.iter (printfn "%A %A"); 
        x

let part1() =
    let totalStones = numStonesWithCache 25 initialStones
    printfn "Stones: %i" totalStones

let part2() =
    let sw = Stopwatch()
    sw.Start()
    let totalStones = numStonesWithCache 75 initialStones
    sw.Stop()
    printfn "Stones: %i in %A" totalStones sw.Elapsed

part2()

numStonesWithCache 75 [|0UL|]

// let rec f depth result = 
//     printf "%d %d" depth result
//     if depth = 0 then 1
//     else 
//         let r = 
//             let rnd = Random().Next()
//             printfn " %d" rnd
//             match rnd with
//             | x when x % 2 = 0 -> f (depth-1) -2
//             | x -> f (depth - 1) 1
//         r

// printfn "f: %A" <| f 25 1        