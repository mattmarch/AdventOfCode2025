module AdventOfCode2025.Day01

open Common

type Rotation = Right of int64 | Left of int64

let parseLine (line: string) : Rotation =
    match line with
    | Prefix "R" (Int64 amount) -> Right amount
    | Prefix "L" (Int64 amount) -> Left amount
    | _ -> failwithf $"Invalid line: %s{line}"
    

let getTurnN position =
    if position >= 0L then
        position / 100L
    else
        (position - 99L) / 100L

let zeroCrossings startP endP =
    if endP > startP then
        (getTurnN endP) - (getTurnN startP)
    else
        (getTurnN (startP - 1L)) - (getTurnN (endP - 1L))
    

let solve () =
    let rotations =
        readLines "01"
        |> Seq.map parseLine
        |> Seq.toList

    let allPositions =
        rotations
        |> Seq.scan (fun v rot ->
            match rot with
            | Right n -> v + n
            | Left n -> v - n)
            50L
            
    let part1 =
        allPositions
        |> Seq.where (fun x -> x % 100L = 0L)
        |> Seq.length
        
    printfn $"Day 01 - Part 1: %d{part1}"
    
    let part2 =
        allPositions
        |> Seq.pairwise
        |> Seq.sumBy (fun (a, b) -> zeroCrossings a b)
        

    printfn $"Day 01 - Part 2: %d{part2}"