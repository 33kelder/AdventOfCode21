module Day2

open System.IO

//forward 5
//down 5
//forward 8
//up 3
//down 8
//forward 2

type Direction =
| Forward
| Down
| Up
| Unknown

type Velocity = {
    Direction:Direction
    X: int
}

let parseDirection str =
    match str with
    | "forward" -> Direction.Forward
    | "down" -> Direction.Down
    | "up" -> Direction.Up
    | _ -> Direction.Unknown

let parseInstruction (line:string) =
    let strings = line.Split(" ")
    let direction = parseDirection strings[0]
    let x = strings[1] |> int
    { Velocity.Direction = direction; X = x }

let allInstructions = 
    File.ReadLines(@"Day2\\Day2Input.txt")
    |> Seq.toList
    |> List.map parseInstruction

let rec procesInstructions instructions horizontalPosition depth aim =
    match instructions with 
    | instruction::restInstructions ->
        match instruction.Direction with
        | Direction.Down -> procesInstructions restInstructions horizontalPosition depth (aim + instruction.X)
        | Direction.Up -> procesInstructions restInstructions horizontalPosition depth (aim - instruction.X)
        | Direction.Forward -> procesInstructions restInstructions (horizontalPosition + instruction.X) (depth + aim * instruction.X) aim
        | _ -> procesInstructions restInstructions horizontalPosition depth aim
    | _ -> horizontalPosition * depth

let answer = procesInstructions allInstructions 0 0 0

