open System.IO

let move (x, y) (direction, size) =
  match direction with
  | 'R' -> [ x .. x + size ] |> List.map (fun x' -> x', y) |> List.rev
  | 'L' -> [ x - size .. x ] |> List.map (fun x' -> x', y)
  | 'U' -> [ y .. y + size] |> List.map (fun y' -> x, y') |> List.rev
  | 'D' -> [ y - size .. y ] |> List.map (fun y' -> x, y')
  | _ -> printfn "Something went wrong"; [x, y]

let initial = (0,0)

let traceWire wire = 
  ([initial], wire)
  ||> Array.fold (fun (position::trace) step ->
      let trace' = move position step
      List.append trace' (position::trace))

let distance (x1, y1) (x2, y2) =
  abs (x1 - x2) + abs (y1 - y2)

let findPositions wires =
  wires
  |> Array.map (traceWire >> set)

let wires = 
  File.ReadAllLines "inputs/aoc03.txt"
  //[| "R75,D30,R83,U83,L12,D49,R71,U7,L72";"U62,R66,U55,R34,D71,R55,D58,R83" |]
  |> Array.map (fun line -> line.Split ',')
  |> Array.map (fun wire -> 
      wire |> Array.map (fun step -> step.[0], int step.[1..])) 
      
let positions = findPositions wires
let intersections = Set.intersect positions.[0] positions.[1]
let closest =
  intersections
  |> Seq.map (fun x -> distance x initial)
  |> Seq.filter (fun dist -> dist > 0)
  |> Seq.min

// part 2

let moveWithSteps (x, y) (direction, size) steps =
  match direction with
  | 'R' -> [ 0 .. size ] |> List.map (fun i -> (x + i, y), steps + i) |> List.rev
  | 'L' -> [ 0 .. size ] |> List.map (fun i -> (x - i, y), steps + i) |> List.rev
  | 'U' -> [ 0 .. size ] |> List.map (fun i -> (x, y + i), steps + i) |> List.rev
  | 'D' -> [ 0 .. size ] |> List.map (fun i -> (x, y - i), steps + i) |> List.rev
  | _ -> printfn "Something went wrong"; [(x, y), steps]

let traceWireWithSteps wire =
  (([initial, 0]), wire)
  ||> Array.fold (fun ((position, steps)::trace) move ->
      let trace' = moveWithSteps position move steps
      List.append trace' trace)

let findPositionsWithSteps wires =
  wires
  |> Array.map traceWireWithSteps

let allPositionsWithSteps = findPositionsWithSteps wires

let findFastest position (positions: ((int * int) * int) list) =
  positions
  |> List.choose (fun (p, steps) -> if p = position then Some steps else None)
  |> List.min

let commonPositions =
  Set.intersect 
    (allPositionsWithSteps.[0] |> List.map fst |> set)
    (allPositionsWithSteps.[1] |> List.map fst |> set)
  |> Set.remove (0,0)

let earliest = 
  commonPositions
  |> Seq.map (fun pos ->
      (allPositionsWithSteps.[0] |> findFastest pos)
      + (allPositionsWithSteps.[1] |> findFastest pos) )
  |> Seq.min