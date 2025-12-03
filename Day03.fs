module AdventOfCode2025.Day03

open AdventOfCode2025.Common

let parseBank (line: string) = line |> Seq.map (charToInt >> int64) |> Seq.toList

let rec maxJoltage digitsToGo totalSoFar bank =
    let nextDigit =
        bank
        |> List.rev
        |> List.skip (digitsToGo - 1)
        |> List.max
    let nextIndex = List.findIndex (fun x -> x = nextDigit) bank
    let remainingBank = bank.[nextIndex + 1 ..]
    let newTotal = totalSoFar * 10L + nextDigit
    if digitsToGo = 1 then
        newTotal
    else
        maxJoltage (digitsToGo - 1) newTotal remainingBank

let solve () =
    let input = readLines "03" |> Seq.map parseBank
    
    let part1MaxJoltage =
        input
        |> Seq.sumBy (maxJoltage 2 0L)

    printfn $"Day 03 - Part 1: %d{part1MaxJoltage}"
    
    let part2MaxJoltage = 
        input
        |> Seq.sumBy (maxJoltage 12 0L)
        
    printfn $"Day 03 - Part 2: %d{part2MaxJoltage}"
