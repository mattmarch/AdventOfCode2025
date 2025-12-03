module AdventOfCode2025.Day02

open System.Text.RegularExpressions
open AdventOfCode2025.Common

let part1Regex = Regex("^([0-9]+)\1$")
let part2Regex = Regex("^([0-9]+)\1+$")

let parseRange = splitBy "-" >> Seq.map int64 >> unpack2

let expandRange (startN, endN) =
    let count = (endN - startN + 1L) |> int
    Seq.init count (fun i -> startN + int64 i)

let invalidIdPicker (invalidRegex: Regex) id =
    let idStr = id |> string
    if invalidRegex.IsMatch idStr then Some id else None

let solve () =
    let input = readAllText "02" |> splitBy "," |> Seq.map parseRange
    
    let part1InvalidIdSum =
        input
        |> Seq.collect expandRange
        |> Seq.choose (invalidIdPicker part1Regex)
        |> Seq.sum
    
    printfn $"Day 02 - Part 1: %d{part1InvalidIdSum}"
    
    let part2InvalidIdSum =
        input
        |> Seq.collect expandRange
        |> Seq.choose (invalidIdPicker part2Regex)
        |> Seq.sum

    printfn $"Day 02 - Part 2: %d{part2InvalidIdSum}"
