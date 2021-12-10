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
    Speed: int
}

let splitIn2 (str:string) =
    let words = str.Split(" ")
    (words[0], words[1])

let parseDirection str =
    match str with
    | "forward" -> Direction.Forward
    | "down" -> Direction.Down
    | "up" -> Direction.Up
    | _ -> Direction.Unknown

let parseVelocity (directionString:string, speedString:string) =
    let direction = parseDirection directionString
    let speed = speedString |> int
    { 
        Velocity.Direction = direction; 
        Speed = speed
    }

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}
let allVelocities = 
    File.ReadAllLines(@"Day2Input.txt")
    |> Array.toList
    |> List.map splitIn2
    |> List.map parseVelocity

let rec procesVelocities velocities horizontalPosition depth =
    match velocities with 
    | h1::t1 ->
        match h1.Direction with
        | Direction.Forward -> procesVelocities t1 (horizontalPosition + h1.Speed) depth
        | Direction.Down -> procesVelocities t1 horizontalPosition (depth + h1.Speed)
        | Direction.Up -> procesVelocities t1 horizontalPosition (depth - h1.Speed)
        | _ -> procesVelocities t1 horizontalPosition depth
    | _ -> horizontalPosition * depth

let answer = procesVelocities allVelocities 0 0

