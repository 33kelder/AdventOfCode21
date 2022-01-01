module Day14

open System.IO
open System.Collections
open System
open Utils
open System.Collections.Generic

type PairInsertion = {Pair:string;Insert:char;Count:int}
let printPolymer (polymer:char[]) =
    polymer |> Array.iter (fun c -> Console.Write c)
    Console.WriteLine ""

let (polymerTemplate,pairInsertions)  = 
    //File.ReadLines(@"Day14\\Day14InputSample.txt")
    File.ReadLines(@"Day14\\Day14Input.txt")
    |> Seq.fold 
        (fun (polymerTemplate,pairInsertions) s -> 
            match s with
            | s when s.Contains(" -> ") ->
                let pairInsertionParts = s.Split(" -> ")
                let pairInsertion = {
                    PairInsertion.Pair = pairInsertionParts[0]
                    Insert = char pairInsertionParts[1]
                    Count = 0
                }
                (polymerTemplate, Array.append pairInsertions [|pairInsertion|])
            | s when s.Trim().Length > 0 -> (s |> Seq.toArray, pairInsertions)
            | _ -> (polymerTemplate,pairInsertions) )
        (Array.empty, Array.empty)

let rec doStep (polymer:char[]) (pairInsertions:PairInsertion[]) nrOfSteps =
    if nrOfSteps = 0 
    then polymer
    else 
        let mutable newPolymer = [||]
        for i in 0..polymer.Length - 2 do
            let pair = (string polymer[i]) + (string polymer[i+1])
            let pairInsertion = pairInsertions |> Array.tryFind (fun pairInsertion -> pairInsertion.Pair = pair)
            if pairInsertion.IsSome
            then newPolymer <- (Array.append newPolymer [| polymer[i]; pairInsertion.Value.Insert|])
        newPolymer <- (Array.append newPolymer [| Array.last polymer |])
        doStep newPolymer pairInsertions (nrOfSteps - 1)

let polymerPart1 = doStep polymerTemplate pairInsertions 1

let groupedElementsPart1 = 
    polymerPart1
    |> Array.groupBy (fun c -> c)
    |> Array.sortBy (fun (c, cArray) -> cArray.Length)

let (leastCommonElementPart1,leastCommonElementArrayPart1) = groupedElementsPart1[0]
let (mostCommonElementPart1,mostCommonElementArrayPart1) = groupedElementsPart1 |> Array.last
let leastCommonElementCountPart1 = leastCommonElementArrayPart1.Length
let mostCommonElementCountPart1 = mostCommonElementArrayPart1.Length
let answerPart1 = mostCommonElementCountPart1 - leastCommonElementCountPart1


let addToCountInPairInsertion (pairsCount:Dictionary<string,int64>) (pair:string) count =
    if (pairsCount.ContainsKey(pair))
    then pairsCount[pair] <- pairsCount[pair] + count
    else pairsCount.Add(pair, count)

let countsPolymerPart2a = 
    let mutable resultPairs = new Dictionary<string,int64>()
    for i in 0..polymerTemplate.Length - 2 do
        let pair = (string polymerTemplate[i]) + (string polymerTemplate[i+1])
        addToCountInPairInsertion resultPairs pair 1
    for i in 0..39 do
        let mutable newPairs = Dictionary<string,int64>()
        for KeyValue(pair, count) in resultPairs do
            let pairInsertion = pairInsertions |> Array.find (fun pairInsertion -> pairInsertion.Pair = pair)
            let pairLeft = (string pair[0]) + (string pairInsertion.Insert)
            let pairRight = (string pairInsertion.Insert) + (string pair[1])
            addToCountInPairInsertion newPairs pairLeft count
            addToCountInPairInsertion newPairs pairRight count
        resultPairs <- newPairs
    let countResult = 
        resultPairs
        |> Seq.toList
        |> List.map (fun (KeyValue(pair,count)) -> (pair[0], count))
        |> List.append [polymerTemplate[polymerTemplate.Length - 1],1]
        |> List.groupBy (fun (character,count) -> character)
        |> List.map (fun (character, group) -> (character, group |> Seq.sumBy (fun (char, c) -> c)))
        |> List.sortBy( fun (character, count) -> count)
        |> List.map (fun (character, count) -> count)
    (List.last countResult) - countResult.Head

let answer = countsPolymerPart2a

