module Day1

open System.IO

let allNumbers = 
    File.ReadLines(@"Day1\\Day1Input.txt")
    |> Seq.map int
    |> Seq.toList

let firstNumber = allNumbers[0]
let numbers = allNumbers |> List.skip 1

let rec countIncrease lst prev1 count =
    if (List.length lst) > 2 
    then
        let prevMeasurement = prev1 + lst[0] + lst[1]
        let nextMeasurement = lst[0] + lst[1] + lst[2]
        if prevMeasurement < nextMeasurement
        then 
            countIncrease lst.Tail lst[0] (count + 1)
        else
            countIncrease lst.Tail lst[0] count
    else  
        count
let answer = countIncrease numbers firstNumber 0

