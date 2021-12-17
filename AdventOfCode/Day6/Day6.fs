module Day6

open System.IO
open System

let allInput = File.ReadLines(@"Day6\\Day6Input.txt")

let answer = allInput |> Seq.toList |> List.length

