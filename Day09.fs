module AdventOfCode2025.Day09

open AdventOfCode2025.Common

let parseLine (line: string) =
    line |> splitBy "," |> Seq.map int64 |> unpack2

let getArea ((x1, y1), (x2, y2)) =
    (abs (x2 - x1) + 1L) * (abs (y2 - y1) + 1L)

let withinRangeExc (r1, r2) v =
    let minV = min r1 r2
    let maxV = max r1 r2
    (v > minV) && (v < maxV)
    
let pointInside ((x, y): int64 * int64) ((x1, y1), (x2, y2)) =
    (withinRangeExc (x1, x2) x) && (withinRangeExc (y1, y2) y)

let anyPointInside allPoints rect =
    allPoints
    |> Seq.exists (fun p -> pointInside p rect)

let getEdgeCoords startP endP =
    match startP, endP with
    | (x1, y1), (x2, y2) when x1 = x2 ->
        let minY = min y1 y2
        let maxY = max y1 y2
        seq { for y in minY .. maxY - 1L -> (x1, y) }
    | (x1, y1), (x2, y2) when y1 = y2 ->
        let minX = min x1 x2
        let maxX = max x1 x2
        seq { for x in minX .. maxX - 1L -> (x, y1) }
    | _ -> failwith "Diagonal line in getEdgeCoords"

let solve () =
    let input = readLines "09" |> Seq.map parseLine |> Seq.toList
    
    let part1 =
        input
        |> allCombinations
        |> Seq.map getArea
        |> Seq.max
    printfn $"Day 09 - Part 1: {part1}"
    
    let edgeCoords =
        (Seq.last input) :: input
        |> Seq.pairwise
        |> Seq.collect (fun (startP, endP) -> getEdgeCoords startP endP)
        |> Seq.toList
        
    // doesn't handle concave shapes with larger concave rectangles than convex ones but works
    // on my input
    let part2 =
        input
        |> allCombinations
        |> Seq.sortByDescending getArea
        |> Seq.find (anyPointInside edgeCoords >> not)
        |> getArea
    
    printfn $"Day 09 - Part 2: {part2}"