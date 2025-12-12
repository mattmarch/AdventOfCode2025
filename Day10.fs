module AdventOfCode2025.Day10

open System.Linq
open AdventOfCode2025.Common
open Microsoft.Z3

type Machine =
    { IndicatorLights: bool list
      WiringSchematics: int list list
      JoltageRequirements: int list }

let parseIndicatorLights (lightsStr: string) =
    lightsStr
    |> Seq.map (function
        | '#' -> true
        | '.' -> false
        | _ -> failwithf $"Invalid light character: {lightsStr}")
    |> Seq.toList

let parseIntList (schematicStr: string) =
    schematicStr |> splitBy "," |> List.map int

let parseLine (line: string) =
    let indicatorLights =
        match line with
        | ParseRegex @"\[([#.]+)\]" [ [ lightsStr ] ] -> parseIndicatorLights lightsStr
        | _ -> failwithf $"Invalid indicator lights in line: {line}"

    let wiringSchematics =
        match line with
        | ParseRegex @"\(([\d,]+)\)" schematicStrs -> schematicStrs |> List.map (List.exactlyOne >> parseIntList)
        | _ -> failwithf $"Invalid wiring schematics in line: {line}"

    let joltageRequirements =
        match line with
        | ParseRegex @"\{([\d,]+)\}" [ [ requirementsStr ] ] -> parseIntList requirementsStr
        | _ -> failwithf $"Invalid joltage requirements in line: {line}"

    { IndicatorLights = indicatorLights
      WiringSchematics = wiringSchematics
      JoltageRequirements = joltageRequirements }

let indicatorLightsToInt (lights: bool list) : int =
    lights |> List.mapi (fun i light -> if light then 1 <<< i else 0) |> List.sum

let schematicsToInt (schematics: int list) : int =
    schematics |> List.sumBy (fun v -> 1 <<< v)

let countBits (n: int) : int =
    let mutable count = 0
    let mutable value = n

    while value <> 0 do
        count <- count + (value &&& 1)
        value <- value >>> 1

    count

let solutionNResult (schematicsInts: int list) (solutionN: int) : int =
    schematicsInts
    |> List.mapi (fun i schematicInt -> if (solutionN &&& (1 <<< i)) <> 0 then schematicInt else 0)
    |> List.reduce (^^^)

let fewestButtonPresses (machine: Machine) : int =
    let lightsInt = indicatorLightsToInt machine.IndicatorLights
    let schematicsInts = machine.WiringSchematics |> List.map schematicsToInt
    let nPossibleSolutions = pown 2 (schematicsInts.Count())
    let possibleSolutions = Seq.init nPossibleSolutions id |> Seq.sortBy countBits

    let fewestPressSolution =
        possibleSolutions |> Seq.find (solutionNResult schematicsInts >> (=) lightsInt)

    countBits fewestPressSolution

let getButtonIndices (buttons: int list list) (counterIndex: int) : int list =
    buttons
    |> List.indexed
    |> List.choose (fun (i, counters) ->
        if counters |> List.contains counterIndex then
            Some i
        else
            None)

let fewestPressesPart2 (machine: Machine) : int =
    use ctx = new Context()
    let optimizer = ctx.MkOptimize()

    let buttons =
        machine.WiringSchematics |> List.mapi (fun i _ -> ctx.MkIntConst($"b{i}"))

    buttons |> List.iter (fun b -> optimizer.Add(ctx.MkGe(b, ctx.MkInt(0))))

    machine.JoltageRequirements
    |> List.indexed
    |> List.iter (fun (i, req) ->
        let buttonIndices = getButtonIndices machine.WiringSchematics i

        let includedButtons =
            buttonIndices |> List.map (fun bi -> buttons.[bi] :> ArithExpr)

        optimizer.Add(ctx.MkEq(ctx.MkAdd(List.toArray includedButtons), ctx.MkInt(req))))

    let total = ctx.MkAdd(buttons |> List.map (fun b -> b :> ArithExpr) |> List.toArray)
    optimizer.MkMinimize(total) |> ignore

    match optimizer.Check() with
    | Status.SATISFIABLE ->
        let value = optimizer.Model.Evaluate(total, true) :?> IntNum
        value.Int
    | _ -> failwith "No solution found."

let solve () =
    let input = readLines "10" |> Seq.map parseLine |> Seq.toList
    let part1 = input |> List.sumBy fewestButtonPresses
    printfn $"Day 10 - Part 1: {part1}"
    let part2 = input |> List.sumBy fewestPressesPart2
    printfn $"Day 10 - Part 2: {part2}"
