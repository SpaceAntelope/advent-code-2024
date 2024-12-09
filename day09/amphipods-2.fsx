open System
open System.IO

#load "../global.fsx"
#load "./common.fsx"

let compact (filesystem: int array) =  
    let findMinimumViableSpace fileIndex fileLength = 
        let rec find currentIndex spaceLength = 
            if spaceLength = fileLength then currentIndex - spaceLength
            else if fileIndex = currentIndex then -1
            else if filesystem.[currentIndex] >= 0 then find (currentIndex+1) 0
            else if filesystem.[currentIndex] < 0 then find (currentIndex+1) (spaceLength+1)
            else -1
        
        find 0 0

    let rec compact' fileId =

        let (fileIndex,fileLength) = 
            let first = Array.findIndex (fun x -> x = fileId) filesystem
            let last = Array.findIndexBack (fun x -> x = fileId) filesystem
            first, last - first + 1

        let spaceStartingIndex = findMinimumViableSpace fileIndex fileLength
       
        if spaceStartingIndex >= 0 
        then
            filesystem.[spaceStartingIndex.. spaceStartingIndex + fileLength - 1] <- filesystem.[fileIndex .. fileIndex + fileLength - 1]
            filesystem.[fileIndex .. fileIndex + fileLength - 1] <- Array.create fileLength (-1)
            //printfn "%s" <| Common.fs2str filesystem 

        if fileId >= 0 then compact' (fileId - 1)
        else filesystem
    
    filesystem |> Array.max |> compact'

"./input.example"
|> Common.parse
|> compact
|> Common.fs2str
|> Global.shouldBe "00992111777.44.333....5555.6666.....8888.."

"./input.example"
|> Common.parse
|> compact
|> Common.checksum
|> Global.shouldBe 2858

"./input.actual"
|> Common.parse
|> compact
|> Common.checksum
|> printfn "When accounting for fragmentation, compacted filesystem checksum is %d"