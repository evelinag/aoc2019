#load "intcode.fs"

// day 11
open System.IO
open Intcode

type Direction =
  | Up | Right | Left | Down

let turnLeft direction x y =
  match direction with
  | Up -> x - 1, y, Left
  | Right -> x, y + 1, Up
  | Left -> x, y - 1, Down
  | Down -> x + 1, y, Right

let turnRight direction x y =
  match direction with
  | Up -> x + 1, y, Right
  | Right -> x, y - 1, Down
  | Left -> x, y + 1, Up
  | Down -> x - 1, y, Left

let code = 
  File.ReadAllText("inputs/aoc11.txt").Split(',') 
  |> Array.map int64


// 0 black
// 1 white

let rec robot computer (hull: int64 [,]) x y direction painted =
  let outputs, computer' =
    executeIntcode [hull.[x,y]] code computer
  match computer' with
  | None -> painted
  | Some c ->
    let colour, turn = int outputs.[1], int outputs.[0]
    // 0 means it should turn left 90 degrees, and 1 means it should turn right 90 degrees.
    let x', y', direction' =
      match turn with
      | 0 -> turnLeft direction x y
      | 1 -> turnRight direction x y
      | _ -> printfn "Unknown direction."; x, y, direction
    match colour with
    | 0 -> hull.[x,y] <- 0L
    | 1 -> hull.[x,y] <- 1L
    | _ -> printfn "Unknown colour"
    robot computer' hull x' y' direction' ((x, y)::painted)

let hull : int64 [,] = Array2D.init 1000 1000 (fun i j -> 0L)
let totalPositions = robot None hull 500 500 Up []

let count = totalPositions |> List.distinct |> List.length

// part 2

let hull' = Array2D.init 100 100 (fun i j -> 0L)
hull'.[50, 50] <- 1L
let allPositions = robot None hull' 50 50 Up []

let xMin = allPositions |> List.minBy fst |> fst
let xMax = allPositions |> List.maxBy fst |> fst
let yMin = allPositions |> List.minBy snd |> snd
let yMax = allPositions |> List.maxBy snd |> snd

printfn "\n"
for y in yMax .. -1 .. yMin do
   [ for x in xMin .. xMax ->
       if hull'.[x,y] = 1L then "X" else " " ]
   |> String.concat ""
   |> printfn "%s"
printfn "\n"      

