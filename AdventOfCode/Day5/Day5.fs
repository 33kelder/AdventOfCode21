module Day5

open System.IO
open System

type Line = { X1:int; Y1:int; X2:int; Y2:int }

let getLine (s:string) =
    let coordinates = s.Split([|" -> "; ","|], StringSplitOptions.RemoveEmptyEntries)
    {Line.X1 = int coordinates[0]; Y1= int coordinates[1]; X2= int coordinates[2]; Y2= int coordinates[3]}

let allInput = File.ReadLines(@"Day5\\Day5Input.txt")
let filteredInput = 
    allInput
    |> Seq.map getLine
    |> Seq.filter (fun line -> line.X1 = line.X2 || line.Y1 = line.Y2)
    |> Seq.map (fun line -> 
            match line with
            | line when line.X1 = line.X2 && line.Y1 <= line.Y2 -> [| for i in line.Y1 .. line.Y2 -> (line.X1, i)|]
            | line when line.X1 = line.X2 && line.Y1 > line.Y2 -> [| for i in line.Y2 .. line.Y1 -> (line.X1, i)|]
            | line when line.Y1 = line.Y2 && line.X1 <= line.X2 -> [| for i in line.X1 .. line.X2 -> (i, line.Y1)|]
            | line when line.Y1 = line.Y2 && line.X1 > line.X2 -> [| for i in line.X2 .. line.X1 -> (i, line.Y1)|]
            | _ -> Array.empty
        )
    |> Seq.reduce Array.append
    |> Seq.groupBy (fun c -> c)
    |> Seq.filter (fun (point) -> 
            let (_, ps) = point
            let points = Seq.toArray ps
            points.Length > 1
        )

let answer = filteredInput |> Seq.toList |> List.length

