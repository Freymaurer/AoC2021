open System
open System.IO


let inputDataPath =
    let openDir = IO.Path.GetFullPath (__SOURCE_DIRECTORY__)
    Path.Combine(openDir,"..\supplements\d1-input.txt")

let inputData =
    File.ReadAllLines inputDataPath
    |> Array.map int


// Part 1

let countIncreases (data:int []) =
    [
        for ind = 0 to data.Length-1 do
            match ind with
            | 0 -> ()
            | notFirstIncrease  when data.[ind] > data.[ind-1] -> data.[notFirstIncrease]
            | notFirstElse -> ()
    ]
    |> List.length


/// Answer: 1446 
countIncreases inputData


// Part 2

let binnedData =
    inputData
    |> Array.mapi (fun i num ->
        /// Cannot be used to build triplet
        if i = 0 || i = inputData.Length-1 then
            None
        else
            inputData.[i-1] + num + inputData.[i+1] |> Some
    )
    |> Array.choose id


/// Answer: 1486
let binData = countIncreases binnedData
