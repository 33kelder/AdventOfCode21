module Day12

open System.IO
open System.Collections
open System
open Utils

let sampleSeq1 = seq {yield "start-A";yield "start-b";yield "A-b";yield "A-c";yield "b-d";yield "A-end";yield "b-end"}
let sampleSeq2 = seq {yield "dc-end";yield "HN-start";yield "start-kj";yield "dc-start";yield "dc-HN";yield "LN-dc";yield "HN-end";yield "kj-sa";yield "kj-HN";yield "kj-dc"}

type Cave = {Name:string;IsStart:bool;IsEnd:bool;IsSmall:bool}
type Connection = {Cave1:Cave;Cave2:Cave}
type PathSegment = {CaveFrom:Cave;CaveTo:Cave}
type Path = Cave[]
let isStart (name:string) = name.ToLower() = "start"
let isEnd (name:string) = name.ToLower() = "end"
let isSmall (name:string) = name |> Seq.exists (fun c -> Char.IsUpper(c)) |> not
let createCave name = {Cave.Name = name; IsStart = isStart name; IsEnd = isEnd name; IsSmall = isSmall name}
let startCave = createCave "start"
let endCave = createCave "end"
let getLastCave (path:PathSegment[]) = 
    if path.Length = 0 
    then startCave
    else (Array.last path).CaveTo
let createNewPathSegment lastCave connection =
    if connection.Cave1 = lastCave 
    then {PathSegment.CaveFrom = connection.Cave1; CaveTo = connection.Cave2}
    else {PathSegment.CaveFrom = connection.Cave2; CaveTo = connection.Cave1}

let printPath (caves:PathSegment[]) = 
    Console.Write "start"
    caves |> Array.iter (fun ps -> Console.Write("-" + ps.CaveTo.Name))
    Console.WriteLine ""

let connections = 
    File.ReadLines(@"Day12\\Day12Input.txt")
    //File.ReadLines(@"Day12\\Day12InputSample.txt")
    //sampleSeq2
    |> Seq.map (fun s -> 
        let caves = s.Split('-') 
        {Connection.Cave1 = createCave caves[0]; Cave2 = createCave caves[1]} )
    |> Seq.toArray

let rec createPaths connections (path:PathSegment[]) =
    let lastCave = getLastCave path
    if lastCave = endCave
    then [|path|]
    else 
        let connectionsFromLastCave = 
            connections 
            |> Array.filter (fun conn -> conn.Cave1 = lastCave || conn.Cave2 = lastCave)
            |> Array.filter (fun conn -> 
                let newPathSegment = createNewPathSegment lastCave conn
                let notGoToStart = newPathSegment.CaveTo <> startCave
                let notVisitSmallCaveMoreOnce = path |> Array.exists (fun ps -> ps.CaveTo = newPathSegment.CaveTo && ps.CaveTo.IsSmall) |> not
                let smallCavesVisistedTwice = 
                    path
                    |> Array.filter (fun ps -> ps.CaveTo <> startCave && ps.CaveTo <> endCave && ps.CaveTo.IsSmall)
                    |> Array.groupBy (fun ps -> ps.CaveTo)
                    |> Array.filter (fun (smallCave, smallCaveSegments) -> smallCaveSegments.Length > 1)
                    |> Array.length
                notGoToStart && (notVisitSmallCaveMoreOnce || smallCavesVisistedTwice = 0) )
            |> Array.map (fun conn -> createNewPathSegment lastCave conn)
            |> Array.filter (fun ps -> ps.CaveTo <> startCave)
            |> Array.map (fun ps -> Array.append path [|ps|])
        let subPaths = 
            connectionsFromLastCave
            |> Array.map (fun path -> createPaths connections path)
        let paths = subPaths |> Array.fold (fun paths1 paths2 -> Array.append paths1 paths2) Array.empty
        paths

let paths = createPaths connections Array.empty
//paths |> Array.iter (fun path -> printPath path)
let answer = paths |> Array.length

