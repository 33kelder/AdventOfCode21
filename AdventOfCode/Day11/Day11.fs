module Day11

open System.IO
open System.Collections
open System


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
let printEnergyLevels () =
    for i in 0..maxI-1 do
        for j in 0..maxI-1 do
            Console.Write energyLevels[i,j]
            Console.Write " "
        Console.WriteLine ""
    Console.WriteLine ""

let surroundingPoints = [|(-1,-1);(-1,0);(-1,1);(0,-1);(0,0);(0,1);(1,-1);(1,0);(1,1)|]

let getFlashes() =
    let mutable flashes = Array.empty
    energyLevels 
        |> Array2D.iteri (fun x y el -> if el = 10 then flashes <- Array.append flashes [|(x,y)|])
    flashes

let (>=<) (x, y) (min, max) = min <= x && x <= max && min <= y && y <= max

let rec getNrOfFlashes (steps:int) (totalNrOfFlashes:int) =
    energyLevels |> Array2D.iteri (fun x y el -> energyLevels[x, y] <- energyLevels[x, y] + 1)
    let mutable flashes = getFlashes ()
    let rec doFlash flashing flashed totalNrOfFlashes =
        let mutable newFlashes = Array.empty
        flashing
        |> Array.iter (fun (x, y) -> 
            surroundingPoints 
            |> Array.iter (fun (ix, iy) -> 
                if (x + ix, y + iy) >=< (0, maxI - 1)
                then 
                    energyLevels[x + ix, y + iy] <- energyLevels[x + ix, y + iy] + 1 
                    if energyLevels[x + ix, y + iy] = 10 && (not (Array.contains (x + ix, y + iy) flashed))
                    then 
                        newFlashes <- Array.append newFlashes [|(x + ix, y + iy)|]
                )
            )
        let newTotalNrOfFlashes = (totalNrOfFlashes + flashing.Length)
        if newFlashes.Length = 0
        then newTotalNrOfFlashes
        else doFlash newFlashes (Array.append flashed flashing) newTotalNrOfFlashes
    let nrOfFlashes = doFlash flashes Array.empty 0
    energyLevels |> Array2D.iteri (fun x y el -> if energyLevels[x, y] >= 10 then energyLevels[x, y] <- 0)
    //printEnergyLevels ()
    if steps > 1
    then getNrOfFlashes (steps - 1) (totalNrOfFlashes + nrOfFlashes)
    else totalNrOfFlashes + nrOfFlashes

let answer = getNrOfFlashes 100 0 

