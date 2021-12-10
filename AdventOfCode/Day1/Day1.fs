module Day1

open System.IO

let allNumbers = 
    File.ReadAllLines(@"Day1Input.txt")
    |> Array.map int
    |> Array.toList

let (startNumber, numbers) = 
    match allNumbers with
    | h1::tl -> (h1, tl)
    | _ -> (0, allNumbers)

let rec countIncrease lst prev count =
    match lst with
    | h1::tl when h1 <= prev -> countIncrease tl h1 count
    | h1::tl when h1 > prev -> countIncrease tl h1 (count + 1)
    | _ -> count

let answer = countIncrease numbers startNumber 0

