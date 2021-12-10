module Day4

open System.IO
open System

type Field = {
    Number:int
    Drawn:bool
}

// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/arrays
type Board(N: int, nrs: int[][]) =
    let internalArray = Array2D.zeroCreate<Field> N N
    let mutable bingoNumber = None
    do 
        for i in 0..N-1 do
            for j in 0..N-1 do
                internalArray[i, j] <- {Field.Number = nrs[i][j]; Drawn = false}

    let getRow(row: int) =
        internalArray[row, 0..internalArray.GetLength(1) - 1]

    let getCol(col: int) =
        internalArray[0..internalArray.GetLength(0) - 1, col]
    
    member this.DrawNumber(nr:int) =
        for i in 0..N-1 do
            for j in 0..N-1 do
                if (internalArray[i, j].Number = nr) then internalArray[i, j] <- {internalArray[i, j] with Drawn = true}

        let getColOrRowBingo getColOrRow = 
            [|0..N-1|]
            |> Array.map (fun colnr -> 
                let colOrRow = getColOrRow colnr 
                let noBingo = colOrRow |> Array.tryFind (fun f -> f.Drawn = false) 
                match noBingo with
                | Some _ -> false
                | _ -> true
                )
            |> Array.contains true
            
        let colBingo = getColOrRowBingo getCol
        let rowBingo = getColOrRowBingo getRow

        let bingo = colBingo || rowBingo
        if bingo then bingoNumber <- Some nr
        bingo

    member this.GetBingoNumber() = 
        match bingoNumber with 
        | Some bingoNr ->
            let mutable sumAllUnMatchNumbers = 0
            for i in 0..N-1 do
                for j in 0..N-1 do
                    if (internalArray[i, j].Drawn = false) then sumAllUnMatchNumbers <- sumAllUnMatchNumbers + internalArray[i, j].Number
            sumAllUnMatchNumbers * bingoNr
        | _ -> 0


let boardSize = 5
let allInput = File.ReadAllLines(@"Day4\\Day4Input.txt")
let numbersDrawn = allInput[0].Split(",") |> Array.map int |> Array.toList

let getRows (rows:string[]) =
    rows
    |> Array.map (fun row -> 
        let row1 = row.Trim().Replace("  ", " ")
        let nrStrings = row1.Split(" ")
        Array.map int nrStrings
        )

let getBoard (nrs:int[][]) =
    new Board(boardSize, nrs)

let boardsInput = Array.tail allInput
let boardInputArray = Array.chunkBySize (boardSize+1) boardsInput 

let boards = 
    boardInputArray
    |> Array.map (fun rows -> Array.tail rows)
    |> Array.map getRows
    |> Array.map getBoard

let rec DrawNumbers (boards:Board[]) (numbersDrawn:int list) =
    match numbersDrawn with
    |head::tail -> 
        let mutable bingoBoard = None
        for i in 0..boards.Length - 1 do
            let b = boards[i]
            if (b.DrawNumber head)
            then bingoBoard <- Some b
        match bingoBoard with
        | Some board -> Some board
        | _ -> DrawNumbers boards tail
    | [] -> None

let bingoBoard = DrawNumbers boards numbersDrawn

let answer = 
    match bingoBoard with
    | Some board -> board.GetBingoNumber()
    | None -> 0

