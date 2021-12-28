module Day11

open System.IO
open System.Collections
open System
open Utils

let sampleSeq = 
    seq {
        yield "11111"
        yield "19991"
        yield "19191"
        yield "19991"
        yield "11111"
    }

let energyLevelsArray = 
    File.ReadLines(@"Day11\\Day11Input.txt")
    //File.ReadLines(@"Day11\\Day11InputSample.txt")
    //sampleSeq
    |> Seq.map (fun s -> s |> Seq.map (fun c -> int (string c)) |> Seq.toArray)
    |> Seq.toArray
let maxI = energyLevelsArray.Length

let energyLevels = Array2D.init maxI maxI (fun x y -> energyLevelsArray[x][y])

let surroundingPoints = [|(-1,-1);(-1,0);(-1,1);(0,-1);(0,0);(0,1);(1,-1);(1,0);(1,1)|]

let getFlashes energyLevels =
    let mutable flashes = Array.empty
    energyLevels |> Array2D.iteri (fun x y el -> if el = 10 then flashes <- Array.append flashes [|(x,y)|])
    flashes

let rec doFlash surroundingPoints (energyLevels:int[,]) flashing flashed totalNrOfFlashes =
    let mutable newFlashes = Array.empty
    flashing
    |> Array.iter (fun (x, y) -> 
        surroundingPoints 
        |> Array.filter (fun (dx, dy) -> (x + dx, y + dy) >=< getDimensions energyLevels)
        |> Array.map (fun (dx, dy) -> (x + dx, y + dy))
        |> Array.iter (fun (ix, iy) -> 
            energyLevels[ix, iy] <- energyLevels[ix, iy] + 1 
            if energyLevels[ix, iy] = 10 && (not (Array.contains (ix, iy) flashed))
            then newFlashes <- Array.append newFlashes [|(ix, iy)|] ))
    let newTotalNrOfFlashes = (totalNrOfFlashes + flashing.Length)
    if newFlashes.Length = 0
    then newTotalNrOfFlashes
    else doFlash surroundingPoints energyLevels newFlashes (Array.append flashed flashing) newTotalNrOfFlashes

let rec getNrOfFlashesPart1 surroundingPoints energyLevels (steps:int) (totalNrOfFlashes:int) =
    energyLevels |> Array2D.iteri (fun x y el -> energyLevels[x, y] <- energyLevels[x, y] + 1)
    let mutable flashes = getFlashes energyLevels
    let nrOfFlashes = doFlash surroundingPoints energyLevels flashes Array.empty 0
    energyLevels |> Array2D.iteri (fun x y el -> if energyLevels[x, y] >= 10 then energyLevels[x, y] <- 0)
    if steps > 1
    then getNrOfFlashesPart1 surroundingPoints energyLevels (steps - 1) (totalNrOfFlashes + nrOfFlashes)
    else totalNrOfFlashes + nrOfFlashes

let rec getNrOfStepsPart2 surroundingPoints energyLevels (steps:int) =
    energyLevels |> Array2D.iteri (fun x y el -> energyLevels[x, y] <- energyLevels[x, y] + 1)
    let mutable flashes = getFlashes energyLevels
    let nrOfFlashes = doFlash surroundingPoints energyLevels flashes Array.empty 0
    energyLevels |> Array2D.iteri (fun x y el -> if energyLevels[x, y] >= 10 then energyLevels[x, y] <- 0)
    if nrOfFlashes < energyLevels.Length
    then getNrOfStepsPart2 surroundingPoints energyLevels (steps + 1)
    else steps + 1

let answer = getNrOfStepsPart2 surroundingPoints energyLevels 0 

