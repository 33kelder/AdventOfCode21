module Day3

open System.IO
open System

let parseString (str:string) = 
    str
    |> Seq.toArray 
    |> Array.map string
    |> Array.map int
    |> Array.append [|1|]

let inline (++) a b = Array.map2 (+) a b

let allDiagnostics = 
    File.ReadLines(@"Day3\\Day3Input.txt")
    |> Seq.map parseString
    |> Seq.reduce (fun acc arr -> acc ++ arr)

let nrOfDiagnostics = Array.head allDiagnostics
let summDiagnostics = Array.tail allDiagnostics

let isGammaRate nr i  = if (((double i) / (double nr)) > 0.5) then 1 else 0
let gammaRateArray = 
    summDiagnostics
    |> Array.map (isGammaRate nrOfDiagnostics)
let gammaRateString = String.Join("", gammaRateArray)
let gammaRateDecimal = Convert.ToInt64(gammaRateString,2);
let epsilonRateArray = gammaRateArray |> Array.map (fun i -> if (i = 0) then 1 else 0)
let epsilonRateString = String.Join("", epsilonRateArray)
let epsilonRateDecimal = Convert.ToInt64(epsilonRateString,2);

let answer = gammaRateDecimal * epsilonRateDecimal

