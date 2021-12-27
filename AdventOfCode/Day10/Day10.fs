module Day10

open System.IO
open System.Collections
open System

let sampleSeq = 
    seq {
        yield "[({(<(())[]>[[{[]{<()<>>"
        yield "[(()[<>])]({[<{<<[]>>("
        yield "{([(<{}[<>[]}>{[]{[(<()>"
        yield "(((({<>}<{<{<>}{[]{[]{}"
        yield "[[<[([]))<([[{}[[()]]]"
        yield "[{[{({}]{}}([{[{{{}}([]"
        yield "{<[[]]>}<{[{[{[]{()[[[]"
        yield "[<(<(<(<{}))><([]([]()"
        yield "<{([([[(<>()){}]>(<<{{"
        yield "<{([{{}}[<[[[<>{}]]]>[]]"
    }

let getPushCharacter character = 
    let pushCharacter = 
        match character with
        | '(' -> Some ')'
        | '[' -> Some ']'
        | '{' -> Some '}'
        | '<' -> Some '>'
        | _ -> None
    if pushCharacter.IsSome 
    then pushCharacter.Value
    else failwith ""

let rec getIllegalCharacter (chars:char list) (chunks:Stack) = 
    match chars with
    | character::restChars when chunks.Count = 0 -> 
        match character with
        | ')' | ']' | '}' | '>' -> 0
        | _ -> 
            chunks.Push(getPushCharacter character)
            getIllegalCharacter restChars chunks
    | character::restChars -> 
        let lastChunkChar = chunks.Peek() :?> char
        match character with
        | ')' | ']' | '}' | '>' when lastChunkChar = character -> 
            chunks.Pop() |> ignore
            getIllegalCharacter restChars chunks
        | ')' -> 3
        | ']' -> 57
        | '}' -> 1197
        | '>' -> 25137
        | _ -> 
            chunks.Push(getPushCharacter character) |> ignore
            getIllegalCharacter restChars chunks
    | [] -> 0

let doesNotContainIllegalCharacter (line:string) = getIllegalCharacter (Seq.toList line) (new Stack()) = 0
    
let totalSyntaxErrorScore = 
    File.ReadLines(@"Day10\\Day10Input.txt")
    //sampleSeq
    |> Seq.map (fun line -> Seq.toList line)
    |> Seq.map (fun charList -> getIllegalCharacter charList (new Stack()))
    |> Seq.toList
    |> Seq.reduce (+)

let rec getIncompleteChunks (chars:char list) (chunks:Stack) = 
    match chars with
    | character::restChars when chunks.Count = 0 -> 
        match character with
        | ')' | ']' | '}' | '>' -> [||]
        | _ -> 
            chunks.Push(getPushCharacter character)
            getIncompleteChunks restChars chunks
    | character::restChars -> 
        let lastChunkChar = chunks.Peek() :?> char
        match character with
        | ')' | ']' | '}' | '>' when lastChunkChar = character -> 
            chunks.Pop() |> ignore
            getIncompleteChunks restChars chunks
        | ')' | ']' | '}' | '>' -> [||]
        | _ -> 
            chunks.Push(getPushCharacter character)
            getIncompleteChunks restChars chunks
    | [] -> 
        [| for i in 0 .. chunks.Count - 1 -> chunks.Pop() :?> char |]
        
let getScore (characters:char[]) =
    characters 
    |> Array.fold 
        ( fun (score:int64) c ->
            match c with
            | ')' -> score * 5L + 1L
            | ']' -> score * 5L + 2L
            | '}' -> score * 5L + 3L
            | '>' -> score * 5L + 4L
            | _ -> score )
        0


let scores = 
    File.ReadLines(@"Day10\\Day10Input.txt")
    //sampleSeq
    |> Seq.filter doesNotContainIllegalCharacter
    |> Seq.map (fun line -> Seq.toList line)
    |> Seq.map (fun charList -> getIncompleteChunks charList (new Stack()))
    |> Seq.map (fun characters -> getScore characters)
    |> Seq.toList
    |> List.sortBy (fun score -> score)

let mediumScore = scores[scores.Length/2]
    
let answer =  mediumScore

