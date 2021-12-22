module Day9

open System.IO

let heights = 
    File.ReadLines(@"Day9\\Day9Input.txt")
    |> Seq.map (fun s -> s |> Seq.map (fun c -> int (string c)) |> Seq.toArray)
    |> Seq.toArray

let maxX = heights.Length - 1
let maxY = heights[0].Length - 1
let surroundingPoints = [|
    (0,-1)
    (-1,0)
    (1,0)
    (0,1)
|]

let surroundingIsHigher (ix, iy) (points:int[][]) (surroundingPointX, surroundingPointY) =
    ix + surroundingPointX < 0 ||
    ix + surroundingPointX > maxX ||
    iy + surroundingPointY < 0 ||
    iy + surroundingPointY > maxY ||
    points[ix + surroundingPointX][iy + surroundingPointY] > points[ix][iy]

let mutable nrMinPoints = 0
let mutable sumOfMinPoints = 0
for ix in 0..maxX do
    for iy in 0..maxY do
        let isNotMinPoint = 
            surroundingPoints
            |> Array.map (surroundingIsHigher (ix,iy) heights)
            |> Array.exists (fun surroundingIsHigher -> surroundingIsHigher = false)
        match isNotMinPoint with
        | true -> ()
        | false -> 
            nrMinPoints <- nrMinPoints + 1
            let risk = heights[ix][iy] + 1
            sumOfMinPoints <- sumOfMinPoints + risk

let answer = sumOfMinPoints

