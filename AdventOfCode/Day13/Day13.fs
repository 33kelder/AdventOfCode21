module Day13

open System.IO
open System.Collections
open System
open Utils

type Dot = {X:int;Y:int}
type FoldDirection = |Up|Left
type Instruction = {FoldDirection:FoldDirection;LineNr:int}
let foldAlong = "fold along "
let printDots (dots:Dot[]) (maxX, maxY) = 
    for j in 0..maxY - 1 do
        for i in 0..maxX - 1 do
            if (dots |> Array.exists (fun dot -> dot.X = i && dot.Y = j))
            then Console.Write "X"
            else Console.Write "."
        Console.WriteLine ""
    Console.WriteLine ""

let (dots,instructions)  = 
    //File.ReadLines(@"Day13\\Day13InputSample.txt")
    File.ReadLines(@"Day13\\Day13Input.txt")
    |> Seq.fold 
        (fun (dots,instructions) s -> 
            match s with
            | s when s.StartsWith(foldAlong) ->
                let instructionParts = s.Replace(foldAlong, "").Split('=')
                let instruction = {
                   Instruction.FoldDirection = if instructionParts[0] = "y" then FoldDirection.Up else FoldDirection.Left
                   LineNr = int instructionParts[1]
                }
                (dots, List.append instructions [instruction])
            | s when s.Contains(',') ->
                    let dotParts = s.Split(',')
                    let dot = {
                        Dot.X = int dotParts[0]
                        Y = int dotParts[1]
                    }
                    (Array.append dots [|dot|], instructions)
            | _ -> (dots,instructions) )
        (Array.empty, List.empty)
let maxY = dots |> Array.map (fun dot -> dot.Y) |> Array.sort |> Array.last
let maxX = dots |> Array.map (fun dot -> dot.X) |> Array.sort |> Array.last
    
let rec foldPaper dots instructions (maxX,maxY)=
    match instructions with
    | instruction::restInstructions ->
        match instruction.FoldDirection with
        |FoldDirection.Up -> 
            let newDots = 
                dots
                |> Array.map (fun dot -> 
                    let newY = 
                        if dot.Y <= instruction.LineNr
                        then dot.Y
                        else instruction.LineNr - (dot.Y - instruction.LineNr)
                    { Dot.X = dot.X ; Y = newY })
                |> Array.distinct
            let (newMaxX, newMaxY) = (maxX, instruction.LineNr)
            foldPaper newDots restInstructions (newMaxX, newMaxY)
        |FoldDirection.Left -> 
            let newDots = 
                dots
                |> Array.map (fun dot -> 
                    let newX = 
                        if dot.X <= instruction.LineNr
                        then dot.X
                        else instruction.LineNr - (dot.X - instruction.LineNr)
                    { Dot.X = newX ; Y = dot.Y })
                |> Array.distinct
            let (newMaxX, newMaxY) = (instruction.LineNr, maxY)
            foldPaper newDots restInstructions (newMaxX, newMaxY)
    | [] -> (dots, maxX, maxY)

let firstInstruction = [instructions[0]]
let (foldedPaperDots, foldedMaxX, foldedMaxY) = foldPaper dots instructions (maxX,maxY)
printDots foldedPaperDots (foldedMaxX, foldedMaxY)
let answer = foldedPaperDots.Length

