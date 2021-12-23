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
let surroundingIsHigher (ix, iy) (surroundingPointX, surroundingPointY) =
    ix + surroundingPointX < 0 ||
    ix + surroundingPointX > maxX ||
    iy + surroundingPointY < 0 ||
    iy + surroundingPointY > maxY ||
    heights[ix + surroundingPointX][iy + surroundingPointY] > heights[ix][iy]

let getHigherSurroundingPoints (ix,iy) =
    surroundingPoints
    |> Array.filter (fun (surroundingPointX, surroundingPointY) ->
        ix + surroundingPointX >= 0 &&
        ix + surroundingPointX <= maxX &&
        iy + surroundingPointY >= 0 &&
        iy + surroundingPointY <= maxY &&
        heights[ix + surroundingPointX][iy + surroundingPointY] > heights[ix][iy] &&
        heights[ix + surroundingPointX][iy + surroundingPointY] < 9)
    |> Array.map (fun (surroundingPointX, surroundingPointY) -> (ix + surroundingPointX, iy + surroundingPointY))

let mutable minPoints = Array.empty
for ix in 0..maxX do
    for iy in 0..maxY do
        let isNotMinPoint = 
            surroundingPoints
            |> Array.map (surroundingIsHigher (ix,iy))
            |> Array.exists (fun surroundingIsHigher -> surroundingIsHigher = false)
        match isNotMinPoint with
        | true -> ()
        | false -> minPoints <- Array.append minPoints [|(ix,iy)|] 

let rec getBassinPoints (previousFoundPoints:(int*int)[]) (ix, iy) =
    let higherSurroundingPoints = getHigherSurroundingPoints (ix, iy)
    let newPoints = higherSurroundingPoints |> Array.except previousFoundPoints
    let foundPoints = Array.append previousFoundPoints newPoints
    match newPoints with
    | [||] -> foundPoints
    | newPoints -> 
        let mutable newFoundPoints = foundPoints
        for i in 0..(Array.length newPoints - 1) do
            let bp = getBassinPoints newFoundPoints newPoints[i]
            newFoundPoints <- Array.append newFoundPoints bp
        newFoundPoints |> Array.distinct

let ps = minPoints //|> Array.except [|(0,1);(2,2);(4,6)|]
let multiplyLargests =
    ps
    |> Array.map (fun (ix,iy) -> getBassinPoints [|(ix,iy)|] (ix,iy))
    |> Array.map (fun p -> Array.length p)
    |> Array.sortByDescending (fun h -> h)
    |> Array.take 3
    |> Array.reduce (*)

let answer = multiplyLargests

