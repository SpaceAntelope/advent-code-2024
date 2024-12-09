open System
open System.IO

#load "../global.fsx"
#load "./common.fsx"

let rec compact (filesystem: int array) =  
    // printfn "%s" <| fs2str filesystem 
    let firstSpaceIndex = Array.findIndex (fun x -> x < 0) filesystem
    let lastFileIndex = Array.findIndexBack (fun x -> x > -1) filesystem
    if lastFileIndex < firstSpaceIndex
    then filesystem
    else 
        filesystem.[firstSpaceIndex] <- filesystem.[lastFileIndex]
        filesystem.[lastFileIndex] <- -1
        compact filesystem

let inputTesting =
    "./input.test"
    |> Common.parse
    |> Common.fs2str
    |> Global.shouldBe "0..111....22222"

    "./input.example"
    |> Common.parse
    |> Common.fs2str
    |> Global.shouldBe "00...111...2...333.44.5555.6666.777.888899"


let compactTesting = 
    "./input.test"
    |> Common.parse
    |> compact
    |> Common.fs2str
    |> Global.shouldBe "022111222......"

    "./input.example"
    |> Common.parse
    |> compact
    |> Common.fs2str
    |> Global.shouldBe "0099811188827773336446555566.............."

    "./input.example"
    |> Common.parse
    |> compact
    |> Common.checksum
    |> Global.shouldBe 1928


"./input.actual"
|> Common.parse
|> compact
|> Common.checksum
|> printfn "Compacted filesystem checksum is %d"