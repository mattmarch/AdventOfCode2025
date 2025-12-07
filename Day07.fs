module AdventOfCode2025.Day07

open AdventOfCode2025.Common

let getStartIndex (startLine: string) = startLine.IndexOf('S')

let getSplitterIndices (line: string) =
    line
    |> Seq.indexed
    |> Seq.choose (fun (i, c) ->
        match c with
        | '^' -> Some i
        | _ -> None)
    |> Set.ofSeq

let parseInput (lines: string seq) =
    match lines |> Seq.toList with
    | startLine :: splitterLines -> getStartIndex startLine, splitterLines |> Seq.map getSplitterIndices
    | _ -> failwith "No input lines"

let getBeamsAfterSplitters (beamPositions: int seq) (splitterIndices: Set<int>) =
    beamPositions
    |> Seq.collect (fun pos ->
        if Set.contains pos splitterIndices then
            [ pos - 1; pos + 1 ]
        else
            [ pos ])
    |> Seq.distinct

let countSplits (line1Positions: Set<int>) (line2Positions: Set<int>) =
    let unchangedPositions = Set.intersect line1Positions line2Positions |> Set.count
    Set.count line1Positions - unchangedPositions

let getBeamCountsAfterSplitters (beamPositions: (int * int64) seq) (splitterIndices: Set<int>) =
    beamPositions
    |> Seq.collect (fun (pos, count) ->
        if Set.contains pos splitterIndices then
            [ (pos - 1, count); (pos + 1, count) ]
        else
            [ (pos, count) ])
    |> Seq.groupBy fst
    |> Seq.map (fun (pos, grouped) -> (pos, grouped |> Seq.sumBy snd))

let solve () =
    let startIndex, splitterLines = readLines "07" |> parseInput

    let beamPositions =
        splitterLines
        |> Seq.scan getBeamsAfterSplitters [ startIndex ]
        |> Seq.map Set.ofSeq

    let part1 =
        beamPositions
        |> Seq.pairwise
        |> Seq.sumBy (fun (line1, line2) -> countSplits line1 line2)

    printfn $"Day 07 - Part 1: {part1}"

    let part2 =
        splitterLines
        |> Seq.fold getBeamCountsAfterSplitters [ (startIndex, 1L) ]
        |> Seq.sumBy snd

    printfn $"Day 07 - Part 2: {part2}"
