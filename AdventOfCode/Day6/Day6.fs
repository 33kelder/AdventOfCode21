module Day6

open System.IO
open System

let allInput = 
    File.ReadLines(@"Day6\\Day6Input.txt")
    |> Seq.toList
    |> List.head
let initialFishes = 
    allInput.Split(",")
    |> Array.map int64
let summaryFish = [|
        initialFishes |> Array.filter (fun f -> f = 0) |> Array.length |> int64
        initialFishes |> Array.filter (fun f -> f = 1) |> Array.length |> int64
        initialFishes |> Array.filter (fun f -> f = 2) |> Array.length |> int64
        initialFishes |> Array.filter (fun f -> f = 3) |> Array.length |> int64
        initialFishes |> Array.filter (fun f -> f = 4) |> Array.length |> int64
        initialFishes |> Array.filter (fun f -> f = 5) |> Array.length |> int64
        initialFishes |> Array.filter (fun f -> f = 6) |> Array.length |> int64
        initialFishes |> Array.filter (fun f -> f = 7) |> Array.length |> int64
        initialFishes |> Array.filter (fun f -> f = 8) |> Array.length |> int64
    |]


let birthRateInDays = 7
let daysBeforFirstCycle = 2
let fishAfterDay fish = if (fish > 0) then (fish - 1) else birthRateInDays - 1

let rec getFishes (summaryFish:int64[]) totalDays day = 
    let summaryFishAfterDay = [|
        summaryFish[1]
        summaryFish[2]
        summaryFish[3]
        summaryFish[4]
        summaryFish[5]
        summaryFish[6]
        summaryFish[7] + summaryFish[0]
        summaryFish[8]
        summaryFish[0]
    |]
    if day = totalDays 
    then summaryFishAfterDay
    else getFishes summaryFishAfterDay totalDays (day + 1)

let answer = 
    getFishes summaryFish 256 1 
    |> Array.sum

