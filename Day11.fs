module AdventOfCode2025.Day11

open System.Collections.Generic
open AdventOfCode2025.Common

type Device = {
    Name: string
    Outputs: string list
}

let parseLine (line: string) =
    let name, connections = line |> splitBy ": " |> unpack2
    { Name = name; Outputs = connections |> splitBy " " |> Seq.toList }

let countPaths (devices: Map<string, Device>) (start: string) =
    let cache = Dictionary<string, int>()
    let rec countPathsFrom deviceName =
        match deviceName with
        | "out" -> 1
        | _ ->
            let device = Map.find deviceName devices
            device.Outputs |> List.sumBy (memoize cache countPathsFrom)
    countPathsFrom start

let countPathsPart2 (devices: Map<string, Device>) (start: string) =
    let cache = Dictionary<(bool * bool * string), int64>()
    let rec countPathsFrom (visitedDac, visitedFft, deviceName) =
        match deviceName with
        | "out" -> if visitedDac && visitedFft then 1L else 0L
        | _ ->
            let device = Map.find deviceName devices
            let updatedVisitedDac = visitedDac || (deviceName = "dac")
            let updatedVisitedFft = visitedFft || (deviceName = "fft")
            device.Outputs
            |> List.sumBy (fun nextDevice -> memoize cache countPathsFrom (updatedVisitedDac, updatedVisitedFft, nextDevice))
    countPathsFrom (false, false, start)

let solve () =
    let deviceMap = 
        readLines "11"
        |> Seq.map parseLine
        |> Seq.map (fun d -> (d.Name, d))
        |> Map.ofSeq
    let part1 = countPaths deviceMap "you"
    printfn $"Day 11 - Part 1: {part1}"
    let part2 = countPathsPart2 deviceMap "svr"
    printfn $"Day 11 - Part 2: {part2}"