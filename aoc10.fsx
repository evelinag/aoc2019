open System.IO
open System.Collections.Generic

let input = 
  File.ReadAllLines("inputs/aoc10.txt")
//   ".#..##.###...#######
// ##.############..##.
// .#.######.########.#
// .###.#######.####.#.
// #####.##.#.##.###.##
// ..#####..#.#########
// ####################
// #.####....###.#.#.##
// ##.#################
// #####.##.###..####..
// ..######..##.#######
// ####.##.####...##..#
// .#####..#.######.###
// ##...#.##########...
// #.##########.#######
// .####.#.###.###.#.##
// ....##.##.###..#####
// .#.#.###########.###
// #.#.#.#####.####.###
// ###.##.####.##.#..##".Split '\n'
  |> Array.mapi (fun i line -> 
      line 
      |> Seq.toArray 
      |> Array.mapi (fun j x -> j, x)
      |> Array.choose (fun (j,x) -> if x = '#' then Some (float j, float i) else None))
  |> Array.concat

// part 1 
// compute angle and distance of each asteroid

let angle (x1, y1) (x2, y2) =
  let x, y = x2 - x1, y2 - y1
  System.Math.Atan2(y, x)

let distance (x1, y1) (x2, y2) =
  (x1 - x2)**2. + (y1 - y2)**2.

let laserBase, visibleAsteroids =
  input
  |> Array.map (fun potentialBase ->
      potentialBase, 
      input
      |> Array.sumBy (fun asteroid ->
            let asteroidAngle = angle potentialBase asteroid
            let asteroidDistance = distance potentialBase asteroid
            if asteroid = potentialBase then 0
            else 
              input 
              |> Array.filter (fun a -> a <> asteroid && a <> potentialBase)
              |> Array.filter (fun a -> angle potentialBase a = asteroidAngle && distance potentialBase a < asteroidDistance)
              |> fun isBlocked -> if isBlocked.Length > 0 then 0 else 1
              ))
  |> Array.maxBy snd              

let vaporizationQueue =
  let laserOrderOrig =
    input
    |> Array.filter (fun asteroid -> asteroid <> laserBase)
    |> Array.map (fun asteroid -> angle laserBase asteroid, (distance laserBase asteroid, asteroid))
    |> Array.groupBy fst
    |> Array.map (fun (angles, asteroidData) -> angles, asteroidData |> Array.map snd |> Array.sortBy fst |> List.ofArray)
    |> Array.sortBy fst
  // shift to start at 90 degrees
  let startIdx = laserOrderOrig |> Array.findIndex (fun (x,_) -> x >= - System.Math.PI/2.)
  let queue = new Queue<(float*(float*float)) list>()
  for i in 0..laserOrderOrig.Length-1 do
    queue.Enqueue(laserOrderOrig.[(startIdx + i) % laserOrderOrig.Length ] |> snd)
  queue

let rec vaporize idx =
  let next = vaporizationQueue.Dequeue()
  if idx = 1 then
    next.Head |> fun (_, (x, y)) -> int x * 100 + int y
  else
    match next.Tail with
    | [] -> vaporize (idx - 1)
    | x::xs -> 
      vaporizationQueue.Enqueue(next.Tail)
      vaporize (idx - 1)

vaporize 200
