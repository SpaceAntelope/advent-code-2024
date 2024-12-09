open System.IO

let parse path =
    path
    |> File.ReadAllText
    |> _.ToCharArray()
    |> Array.map (int<<string)
    |> Array.chunkBySize 2
    |> Array.mapi (fun i chunk -> 
        [|
            yield! Array.create chunk.[0] i
            if chunk.Length > 1 then
                yield! Array.create chunk.[1] -1
        |])
    |> Array.collect id

let fs2str (fs: int array) =
    fs |> Array.fold(fun state current-> state + if current >= 0 then (string current) else ".") ""

let checksum (fs: int array) = 
    fs 
    |> Array.indexed 
    |> Array.filter (fun (i,x) -> x > -1)
    |> Array.fold (fun state (i, x) -> 
        state + int64 i * int64 x) 0L
