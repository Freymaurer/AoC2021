open System
open System.IO

let inputDataPath =
    let openDir = IO.Path.GetFullPath (__SOURCE_DIRECTORY__)
    Path.Combine(openDir,"..\supplements\d3-input.txt")

let inputData =
    File.ReadAllLines inputDataPath


// Part 1

let calculatePowerConsumption =
    // Cannot be empty, all must be same length
    let binaryLength = inputData.[0].Length
    [|
        for i in 0 .. binaryLength-1 do
            let groupedArr = inputData |> Array.groupBy (fun binaryString -> binaryString.[i])
            let gammaRate = groupedArr |> Array.map (fun x -> fst x, snd x |> Array.length) |> Array.maxBy snd |> fst
            let epsilonRate = groupedArr |> Array.map (fun x -> fst x, snd x |> Array.length) |> Array.minBy snd |> fst
            i, gammaRate, epsilonRate
    |]
    |> Array.fold (fun acc (_,gamma,epsilon) -> fst acc + string gamma, snd acc + string epsilon ) ("","")
    |> fun (gamma,epsilon) -> 
        let binGamma = Convert.ToInt32(gamma,2)
        let binEpsilon = Convert.ToInt32(epsilon,2)
        binGamma * binEpsilon

// Answer: 3277364

let testData = [|
    "00100"
    "11110"
    "10110"
    "10111"
    "10101"
    "01111"
    "00111"
    "11100"
    "10000"
    "11001"
    "00010"
    "01010"
|]

// Part 2

// Start with the full list of binary numbers from your diagnostic report and consider just the first bit of those numbers.
// Filter by BIT criteria, check if only one value left, if not repeat with the next bit.
// BIT OXYGEN: Determine the most common value (0 or 1) in the current bit position, and keep only numbers with that bit in that position. 
//      If 0 and 1 are equally common, keep values with a 1 in the position being considered.
// BIT CO2: determine the least common value (0 or 1) in the current bit position, and keep only numbers with that bit in that position. 
//      If 0 and 1 are equally common, keep values with a 0 in the position being considered.


let rec calculateLifeSupportRating bitIndex (dataOxygen:string []) (dataCO2:string []) length =
    if dataOxygen.Length = 1 && dataCO2.Length = 1 then
        let decimalOxygen   = Convert.ToInt32(dataOxygen.[0],2)
        let decimalCO2      = Convert.ToInt32(dataCO2.[0],2)
        decimalOxygen * decimalCO2
    elif bitIndex > length-1 then
        failwith "Could not find CO2 or Oxygen"
    else
        let filteredDataOxygen = 
            if dataOxygen.Length = 1 then
                dataOxygen
            else
                let groupedByCount = dataOxygen |> Array.groupBy (fun binaryString -> binaryString.[bitIndex])
                let (mostCommon,count1),(leastCommon,count2) = 
                    let counts = groupedByCount |> Array.map (fun x -> fst x, snd x |> Array.length) |> Array.sortByDescending snd
                    counts.[0], counts.[1]
                let isEquallyCommon = count1 = count2
                let v = if isEquallyCommon then "1" else string mostCommon
                dataOxygen |> Array.filter (fun binaryStr -> 
                    string binaryStr.[bitIndex] = v
                )
        let filteredDataCO2 = 
            if dataCO2.Length = 1 then
                dataCO2
            else
                let groupedByCount = dataCO2 |> Array.groupBy (fun binaryString -> binaryString.[bitIndex])
                let (mostCommon,count1),(leastCommon,count2) = 
                    let counts = groupedByCount |> Array.map (fun x -> fst x, snd x |> Array.length) |> Array.sortByDescending snd
                    counts.[0], counts.[1]
                let isEquallyCommon = count1 = count2
                let v = if isEquallyCommon then "0" else string leastCommon
                dataCO2 |> Array.filter (fun binaryStr -> 
                    string binaryStr.[bitIndex] = v
                )
        printfn "OXY: %A" filteredDataOxygen.Length
        printfn "CO2: %A" filteredDataCO2.Length
        calculateLifeSupportRating (bitIndex+1) filteredDataOxygen filteredDataCO2 length


let res (data:string []) = 
    let l = data.[0].Length
    calculateLifeSupportRating 0 data data l

res inputData