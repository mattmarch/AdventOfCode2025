module AdventOfCode2025.Day04

open AdventOfCode2025.Common

type Square =
    | Paper
    | Empty

let parseSquare square =
    match square with
    | '@' -> Paper
    | '.' -> Empty
    | _ -> failwithf $"Invalid square character: %c{square}"

let parseLine y line =
    line
    |> Seq.map parseSquare
    |> Seq.indexed
    |> Seq.map (fun (x, square) -> (x, y), square)

let parseInput (lines: string seq) =
    lines
    |> Seq.indexed
    |> Seq.collect (fun (y, line) -> parseLine y line)
    |> Map.ofSeq

let coordEmpty map coord =
    match Map.tryFind coord map with
    | Some Paper -> false
    | _ -> true

let isPaperAccessible map (x, y) =
    let adjacent =
        [ (x - 1, y - 1)
          (x, y - 1)
          (x + 1, y - 1)
          (x - 1, y)
          (x + 1, y)
          (x - 1, y + 1)
          (x, y + 1)
          (x + 1, y + 1) ]

    let emptySquares = adjacent |> List.where (coordEmpty map) |> List.length
    emptySquares > 4

let rec findAllRemovablePaper removedCount map =
    let removablePaper =
        map
        |> Map.keys
        |> Seq.where (coordEmpty map >> not)
        |> Seq.where (isPaperAccessible map)
        |> Seq.toList

    if List.isEmpty removablePaper then
        removedCount
    else
        let newMap =
            removablePaper |> List.fold (fun acc coord -> Map.add coord Empty acc) map

        let newRemovedCount = removedCount + List.length removablePaper

        findAllRemovablePaper newRemovedCount newMap

let solve () =
    let input = readLines "04" |> parseInput
    let allCoords = Map.keys input

    let part1 =
        allCoords
        |> Seq.where (coordEmpty input >> not)
        |> Seq.where (isPaperAccessible input)
        |> Seq.length

    printfn $"Day 04 - Part 1: {part1}"

    let part2 = findAllRemovablePaper 0 input

    printfn $"Day 04 - Part 2: {part2}"
