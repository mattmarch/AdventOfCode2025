module AdventOfCode2025.Day12

open AdventOfCode2025.Common

type TreeRegion = { Size: int * int; Shapes: int list }

let parseShapeLine line =
    line
    |> Seq.map (function
        | '#' -> true
        | '.' -> false
        | _ -> failwithf $"Invalid shape character: {line}")
    |> Seq.toList

let parseShape i (shapeLines: string) =
    match shapeLines |> splitBy "\n" with
    | numLine :: lines when numLine = $"{i}:" -> lines |> List.map parseShapeLine
    | _ -> failwithf $"Invalid shape block for shape {i}: {shapeLines}"

let parseRegionLine line =
    let sizeStr, shapesStr =
        line |> splitBy ": " |> unpack2
    let size = sizeStr |> splitBy "x" |> List.map int |> unpack2
    let shapes = shapesStr |> splitBy " " |> List.map int
    { Size = size; Shapes = shapes }

let parseInput (input: string) =
    let sections = input |> splitBy "\n\n"

    match sections |> List.rev with
    | regionSection :: shapeSections ->
        (regionSection |> splitBy "\n" |> List.map parseRegionLine, shapeSections |> List.rev |> List.mapi parseShape)
    | _ -> failwith "Invalid input format"
    
let shapeMinSize shape =
    let filledSquares =
        shape
        |> List.collect (List.where id)
        |> List.length
    filledSquares

let boxSize = 3
let boxesFitInArea width height count =
    let boxesPerRow = width / boxSize
    let boxesPerCol = height / boxSize
    let totalBoxes = boxesPerRow * boxesPerCol
    totalBoxes >= count 

type BoundResult =
    | DefinitelyFits
    | DefinitelyTooLarge
    | PossiblyFits

let checkRegionBounds (shapeMinSizes: int list) region =
    let width, height = region.Size
    let area = width * height
    let minShapeArea =
        region.Shapes
        |> List.mapi (fun i shapeCount -> shapeMinSizes.[i] * shapeCount)
        |> List.sum
    
    let totalBoxes =
        region.Shapes |> List.sum
    let boxesFit = boxesFitInArea width height totalBoxes
    match boxesFit, area < minShapeArea with
    | true, _ -> DefinitelyFits
    | _, true -> DefinitelyTooLarge
    | _ -> PossiblyFits


let solve () =
    let regions, shapes = readAllText "12" |> parseInput
    
    let shapeMinSizes =
        shapes |> List.map shapeMinSize
        
    let boundResults =
        regions
        |> List.map (checkRegionBounds shapeMinSizes)
        
    let lowerBound =
        boundResults
        |> List.filter (function DefinitelyFits -> true | _ -> false)
        |> List.length
    let upperBound =
        boundResults
        |> List.filter (function DefinitelyTooLarge -> false | _ -> true)
        |> List.length
    
    printfn $"Day 12 - Part 1: {lowerBound} to {upperBound}"
