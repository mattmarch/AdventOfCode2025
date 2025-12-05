module AdventOfCode2025.Day05

open AdventOfCode2025.Common

type Range = { Start: int64; End: int64 }

let parseRange (line: string) =
    let rStart, rEnd = line |> splitBy "-" |> Seq.map int64 |> unpack2
    { Start = rStart; End = rEnd }

let parseInput (input: string) =
    let freshRangesSection, availableIngredientsSection =
        input |> splitBy "\n\n" |> unpack2

    let freshRanges = freshRangesSection |> splitBy "\n" |> Seq.map parseRange

    let availableIngredients =
        availableIngredientsSection |> splitBy "\n" |> Seq.map int64

    freshRanges, availableIngredients

let isIngredientFresh (freshRanges: Range seq) ingredient =
    freshRanges
    |> Seq.exists (fun r -> ingredient >= r.Start && ingredient <= r.End)

let tryMergeRanges (r1: Range) (r2: Range) =
    if r1.End + 1L >= r2.Start && r2.End + 1L >= r1.Start then
        Some
            { Start = min r1.Start r2.Start
              End = max r1.End r2.End }
    else
        None

let mergeRangeFolder (mergedRanges: Range list) (nextRange: Range) =
    let nonMatching, current =
        mergedRanges
        |> List.fold
            (fun (nonMatching, current) r ->
                match tryMergeRanges r current with
                | Some merged -> (nonMatching, merged)
                | None -> (r :: nonMatching, current))
            ([], nextRange)

    current :: nonMatching

let solve () =
    let freshRanges, availableIngredients = readAllText "05" |> parseInput

    let part1 =
        availableIngredients |> Seq.where (isIngredientFresh freshRanges) |> Seq.length

    printfn $"Day 05 - Part 1: {part1}"

    let part2 =
        freshRanges
        |> Seq.fold mergeRangeFolder []
        |> Seq.sumBy (fun r -> r.End - r.Start + 1L)

    printfn $"Day 05 - Part 2: {part2}"
