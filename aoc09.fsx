open System.IO
#load "intcode.fs"

open Intcode

let code = 
  File.ReadAllText("inputs/aoc09.txt").Split(',') 
  |> Array.map int64

executeIntcode [1L] code None |> fst
executeIntcode [2L] code None |> fst
