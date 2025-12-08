module AdventOfCode2025.Day08

open AdventOfCode2025.Common

type Coord3D = int64 * int64 * int64

let parseCoord: string -> Coord3D = splitBy "," >> Seq.map int64 >> unpack3

let distSq ((x1, y1, z1): Coord3D) ((x2, y2, z2): Coord3D) =
    let dx = x2 - x1
    let dy = y2 - y1
    let dz = z2 - z1
    dx * dx + dy * dy + dz * dz

let expandCircuit (allConnections: (Coord3D * Coord3D) list) (coord: Coord3D) =
    let rec expandNext (toVisit: Coord3D list) (visited: Set<Coord3D>) =
        match toVisit with
        | [] -> visited
        | current :: rest ->
            let newConnections =
                allConnections
                |> List.choose (function
                    | c1, c2 when c1 = current -> Some c2
                    | c1, c2 when c2 = current -> Some c1
                    | _ -> None)
                |> List.filter (fun c -> not (visited.Contains c))

            expandNext (rest @ newConnections) (visited.Add current)
    
    expandNext [ coord ] Set.empty
    
let applyConnection (connectedGroups: Set<Coord3D> list) (c1: Coord3D, c2: Coord3D) =
    let c1Group = connectedGroups |> List.find (fun group -> group.Contains c1)
    let c2Group = connectedGroups |> List.find (fun group -> group.Contains c2)
    if c1Group = c2Group then
        connectedGroups
    else
        let mergedGroup = Set.union c1Group c2Group
        connectedGroups
        |> List.filter (fun group -> group <> c1Group && group <> c2Group)
        |> List.append [ mergedGroup ]


let solve () =
    let isTest = false
    let inputName, pairs = if isTest then "test08", 10 else "08", 1000
    
    let input = readLines inputName |> Seq.map parseCoord |> Seq.toList
    
    let allCombinations =
        [ for c1 in input do for c2 in input do if c1 <> c2 then yield (c1, c2) ]
        |> Seq.distinctBy (fun (a, b) -> Set.ofList [ a; b ])
        
    let orderedConnections =
        allCombinations
        |> Seq.map (fun pair -> pair, pair ||> distSq)
        |> Seq.sortBy snd
        |> Seq.map fst
        
    let closestConnections =
        orderedConnections
        |> Seq.take pairs
        |> Seq.toList
    
    let allCircuits =
        input
        |> Seq.map (expandCircuit closestConnections)
        |> Seq.distinct
    
    let part1 =
        allCircuits
        |> Seq.map Set.count
        |> Seq.sortByDescending id
        |> Seq.take 3
        |> Seq.reduce (*)
        
    
    printfn $"Day 08 - Part 1: {part1}"
    
    let finalConnection =
        orderedConnections
        |> Seq.scan applyConnection (input |> Seq.map Set.singleton |> Seq.toList)
        |> Seq.findIndex (fun groups -> List.length groups = 1)
        
    let (x1, _, _), (x2, _, _) =
        orderedConnections
        |> Seq.item (finalConnection - 1)
    
    let part2 = x1 * x2    
    
    printfn $"Day 08 - Part 2: {part2}"