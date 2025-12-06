module AdventOfCode2025.Day06

open System
open AdventOfCode2025.Common

type Sign = Plus | Times

type Problem = Sign * int64 seq

let (|Sign|_|) (str: string) =
    match str with
    | "+" -> Some Plus
    | "*" -> Some Times
    | _ -> None


let parseProblem (column: string seq): Problem =
    match column |> Seq.rev |> Seq.toList with
    | Sign sign :: numbers -> 
        let nums = numbers |> Seq.map int64 |> Seq.rev
        (sign, nums)
        | _ -> failwithf $"""Invalid column: {column |> String.concat " "}"""


let parseInput (lines: string seq) =
    lines
    |> Seq.map (splitBy " ")
    |> Seq.transpose
    |> Seq.map parseProblem

let solveProblem (problem: Problem) =
    match problem with
    | Plus, numbers -> numbers |> Seq.sum
    | Times, numbers -> numbers |> Seq.fold (*) 1L

let splitByEmptyColumn (columns: char seq seq) =
    seq {
        let mutable currentGroup = List.empty
        for col in columns do
            if Seq.forall ((=) ' ') col then
                yield List.rev currentGroup
                currentGroup <- List.empty
            else
                currentGroup <- col :: currentGroup
        if not (List.isEmpty currentGroup) then
            yield List.rev currentGroup
    }

let splitSignRowAndNumbersInCol (column: char seq): char * char seq =
    match column |> Seq.rev |> Seq.toList with
    | signChar :: numberChars ->
        (signChar, Seq.rev numberChars)
    | _ -> failwithf $"Invalid column: {column}"
    
let charSeqToInt64 chars=
    chars |> Seq.map Char.ToString |> String.concat "" |> int64

let parsePart2Problem (columns: char seq list): Problem =
    let signChars, numberCols =
        columns
        |> List.map splitSignRowAndNumbersInCol
        |> List.unzip

    let sign =
        match signChars |> List.where ((<>) ' ') with
        | ['+'] -> Plus
        | ['*'] -> Times
        | _ -> failwithf $"Invalid characters in column: {signChars}"
    
    let numbers = numberCols |> Seq.map charSeqToInt64

    (sign, numbers)
    

let parsePart2 (lines: string seq) =
    lines |> Seq.transpose |> splitByEmptyColumn |> Seq.map parsePart2Problem
    

let solve () =
    let inputLines = readLines "06"
    let inputPart1 = inputLines |> parseInput
    let part1 = inputPart1 |> Seq.sumBy solveProblem
    printfn $"Day 06 - Part 1: {part1}"
    
    let inputPart2 = inputLines |> parsePart2
    let part2 = inputPart2 |> Seq.sumBy solveProblem
    printfn $"Day 06 - Part 2: {part2}"
