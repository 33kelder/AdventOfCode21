module Day3

open System.IO
open System

let parseBits (str:string) = 
    str
    |> Seq.toArray 
    |> Array.map string
    |> Array.map int

let inline (++) a b = Array.map2 (+) a b

let allDiagnostics = 
    File.ReadLines(@"Day3\\Day3Input.txt")
    |> Seq.map parseBits
    |> Seq.toList

let getOxygenRateAtIndex (summDiagnostics:int[]) index nrOfDiagnostics  =
    if (((double summDiagnostics[index]) / (double nrOfDiagnostics)) >= 0.5) then 1 else 0
let getCO2RateAtIndex (summDiagnostics:int[]) index nrOfDiagnostics  =
    if (((double summDiagnostics[index]) / (double nrOfDiagnostics)) >= 0.5) then 0 else 1
let getDecimal (ints:int[]) = 
    let string = String.Join("", ints)
    Convert.ToInt64(string,2);

let rec findGeneratorRating getRateAtIndex (diagnostics:int[] list) (index:int) =
    let summDiagnostics = diagnostics |> List.reduce (fun acc arr -> acc ++ arr)
    let nrOfDiagnostics = diagnostics.Length
    let oxygenRateAtIndex = getRateAtIndex summDiagnostics index nrOfDiagnostics
    let filteredDiagnostics = 
        diagnostics
        |> List.filter (fun d -> d[index] = oxygenRateAtIndex)
    if (filteredDiagnostics.Length > 1)
    then
        findGeneratorRating getRateAtIndex filteredDiagnostics (index + 1)
    else
        filteredDiagnostics[0]

let oxygenGeneratorRatingDecimal = 
    findGeneratorRating getOxygenRateAtIndex allDiagnostics 0 
    |> getDecimal
let co2GeneratorRatingDecimal = 
    findGeneratorRating getCO2RateAtIndex allDiagnostics 0 
    |> getDecimal
let answer = oxygenGeneratorRatingDecimal * co2GeneratorRatingDecimal