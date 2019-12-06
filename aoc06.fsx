
open System.IO

type OrbitSystem =
  | Leaf of string
  | Subsystem of string * OrbitSystem []

let orbits = 
  //[| "COM)B"; "B)C";  "C)D";  "D)E";  "E)F";  "B)G";  "G)H";  "D)I";  "E)J";  "J)K"; "K)L"; "K)YOU"; "I)SAN" |]
  File.ReadAllLines "inputs/aoc06.txt"
  |> Array.map (fun line -> 
      line.Split ')'
      |> fun a -> a.[0], a.[1])

let rec constructGraph centre =
  let orbitting = 
    orbits
    |> Array.filter (fun (x, _) -> x = centre)
  if orbitting.Length = 0 then
    Leaf centre
  else
    Subsystem(centre, 
      orbitting
      |> Array.map (fun (_, object) -> constructGraph object) )

let graph = constructGraph "COM"

let rec countIndirectOrbits g depth =
  let currentNode = 
    // number of indirect orbits for the current node, don't count the direct orbit around the parent
    if depth > 0 then 
      depth - 1
    else 
      0
  match g with 
  | Leaf _ -> currentNode 
  | Subsystem (_, subgraphs) ->
    currentNode + 
      (subgraphs |> Array.sumBy (fun sg -> countIndirectOrbits sg (depth + 1)))

let graphChecksum =
  let directOrbits = orbits.Length
  let indirectOrbits = countIndirectOrbits graph 0
  directOrbits + indirectOrbits

// part 2
// find closest common parent in DAG between "YOU" and "SAN"

// for a specific node, get list of all its ancestors in the graph and number of transfers (steps)
// needed to reach them
let rec getAncestorsDepth graph node =
  match graph with
  | Leaf x when x = node -> true, [node, -1] // don't count the direct parents
  | Leaf _ -> false, []
  | Subsystem (x, children) ->
    let subresults = 
      children
      |> Array.map (fun n -> getAncestorsDepth n node)
      |> Array.filter (fun (containsNode, ancestors) -> containsNode)
    if subresults.Length = 0 then
      false, []
    else
      let _, ancestors = subresults |> Array.exactlyOne
      let depth = ancestors.Head |> snd
      true, (x, depth + 1) :: ancestors

let youPath = getAncestorsDepth graph "YOU" |> snd
let sanPath = getAncestorsDepth graph "SAN" |> snd

// find the lowest common ancestor and sum the steps
let lowestCommon = 
  youPath
  |> List.filter (fun (object, steps) -> 
      sanPath 
      |> List.tryFind (fun (object', steps') -> object = object')
      |> Option.isSome)
  |> List.minBy snd

let youSteps = snd lowestCommon
let sanSteps = 
  sanPath 
  |> List.find (fun (object, steps) -> object = fst lowestCommon)
  |> snd
let transfers = youSteps + sanSteps

