module AdventOfCode2025.Day09

open AdventOfCode2025.Common

let parseLine (line: string) =
    line |> splitBy "," |> Seq.map int64 |> unpack2

let getArea ((x1, y1), (x2, y2)) =
    (abs (x2 - x1) + 1L) * (abs (y2 - y1) + 1L)

let solve () =
    let input = readLines "09" |> Seq.map parseLine |> Seq.toList
    
    let part1 =
        input
        |> allCombinations
        |> Seq.map getArea
        |> Seq.max
    printfn $"Day 09 - Part 1: {part1}"
    printfn "Day 09 - Part 2: Not implemented"