module Day7

open System.IO
open System

let allInput = 
    File.ReadLines(@"Day7\\Day7Input.txt")
    |> Seq.toList
    |> List.head
let initialPositions = 
    allInput.Split(",")
    |> Array.map int

let minPosition = initialPositions |> Array.min
let maxPosition = initialPositions |> Array.max
let nrOfCrabs = initialPositions.Length

let getFuelCost i =
    initialPositions
    |> Array.map (fun p -> 
            let steps = Math.Abs(p - i)
            [|1 .. steps|] |> Array.sum
        )
    |> Array.sum

let getMinimumFuelCost (minFuelCost, minFuelCostPosition) position =
    let fuelCostAtPosition = getFuelCost position
    if (fuelCostAtPosition < minFuelCost)
    then (fuelCostAtPosition, position)
    else (minFuelCost, minFuelCostPosition)

let (fuelCost, position) =
    [|0 .. maxPosition - 1|]
    |> Array.fold getMinimumFuelCost (Int32.MaxValue, 0)

let answer = fuelCost

