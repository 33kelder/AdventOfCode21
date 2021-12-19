module Day8

open System.IO
open System
open System.Collections.Generic

let allInput = 
    File.ReadLines(@"Day8\\Day8Input.txt")

type Entry = {
    Signals:string[]
    OutputValues:string[]
}

let orderString (s:string) =
    let orderedString = 
        s
        |> Seq.toArray
        |> Array.sort
    String.Join("", orderedString)

let sContains (s2:string) (s1:string) = 
    s2 |> Seq.forall (fun c -> s1.Contains(c))

let entries = 
    allInput
    |> Seq.map (fun s ->
            let strings = s.Split(" | ")
            let signals = strings[0].Split(" ") |> Array.sort
            let outputValues = strings[1].Split(" ")
            {Entry.Signals = signals; OutputValues = outputValues}
        )

let getNumber (numbers:string[]) (s:string) = 
    let os = orderString s
    match os with
    | s when s = numbers[0] -> 0
    | s when s = numbers[1] -> 1
    | s when s = numbers[2] -> 2
    | s when s = numbers[3] -> 3
    | s when s = numbers[4] -> 4
    | s when s = numbers[5] -> 5
    | s when s = numbers[6] -> 6
    | s when s = numbers[7] -> 7
    | s when s = numbers[8] -> 8
    | s when s = numbers[9] -> 9
    | _ -> 0

let getOutputValue (outputValueString:string[]) (numbers:string[]) = 
    let numberArray =
        outputValueString
        |> Array.map (getNumber numbers)
        |> Array.map string
    let numberString = String.Join("", numberArray)
    int numberString

let is1 (signal:string) =
    signal.Length = 2
    
let is7 (signal:string) =
    signal.Length = 3
    
let is4 (signal:string) =
    signal.Length = 4
    
let is8 (signal:string) =
    signal.Length = 7

let is0 (one:string, four:string, seven:string) (signal:string) =
    signal.Length = 6 &&
    signal |> sContains four = false &&
    signal |> sContains seven = true

let is6 (one:string, four:string, seven:string) (signal:string) =
    signal.Length = 6 &&
    signal |> is0 (one, four, seven) = false &&
    signal |> sContains four = false &&
    signal |> sContains seven = false

let is9 (one:string, four:string, seven:string) (signal:string) =
    signal.Length = 6 &&
    signal |> is0 (one, four, seven) = false &&
    signal |> is6 (one, four, seven) = false

let is3 (one:string, four:string, seven:string, zero:string, six:string, nine:string) (signal:string) =
    signal.Length = 5 &&
    signal |> sContains one = true

let is5 (one:string, four:string, seven:string, zero:string, six:string, nine:string) (signal:string) =
    signal.Length = 5 &&
    signal |> is3 (one, four, seven, zero, six, nine) = false &&
    nine |> sContains signal = true

let is2 (one:string, four:string, seven:string, zero:string, six:string, nine:string) (signal:string) =
    signal.Length = 5 &&
    signal |> is3 (one, four, seven, zero, six, nine) = false &&
    nine |> sContains signal = false

let get1478 (signals:string[]) =
    let one = signals |> Array.filter is1
    let four = signals |> Array.filter is4
    let seven = signals |> Array.filter is7
    let eight = signals |> Array.filter is8
    (one[0], four[0], seven[0], eight[0])

let get069 (signals:string[]) (one:string, four:string, seven:string) =
    let zero = signals |> Array.filter (fun signal -> is0 (one, four, seven) signal)
    let six = signals |> Array.filter (fun signal -> is6 (one, four, seven) signal)
    let nine = signals |> Array.filter (fun signal -> is9 (one, four, seven) signal)
    (zero[0], six[0], nine[0])

let get235 (signals:string[]) (one:string, four:string, seven:string, zero:string, six:string, nine:string) =
    let two = signals |> Array.filter (fun signal -> is2 (one, four, seven, zero, six, nine) signal)
    let three = signals |> Array.filter (fun signal -> is3 (one, four, seven, zero, six, nine) signal)
    let five = signals |> Array.filter (fun signal -> is5 (one, four, seven, zero, six, nine) signal)
    (two[0], three[0], five[0])


let sumation =
    entries
    |> Seq.map (fun entry -> 
            let (one, four, seven, eight) = get1478 entry.Signals
            let (zero, six, nine) = get069 entry.Signals (one, four, seven)
            let (two, three, five) = get235 entry.Signals (one, four, seven, zero, six, nine)
            let numbers = [|
                    orderString zero
                    orderString one
                    orderString two
                    orderString three
                    orderString four
                    orderString five
                    orderString six
                    orderString seven
                    orderString eight
                    orderString nine
                |]
            getOutputValue entry.OutputValues numbers
        )
    |> Seq.sum

let answer = sumation

