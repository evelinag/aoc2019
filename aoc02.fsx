open System.IO

// part 1

let rec runIntcode instructionPointer (memory: int[]) =
  match memory.[instructionPointer] with
  | 1 -> 
    memory.[memory.[instructionPointer + 3]] <- memory.[memory.[instructionPointer + 1]] + memory.[memory.[instructionPointer + 2]]
    runIntcode (instructionPointer + 4) memory
  | 2 ->
    memory.[memory.[instructionPointer + 3]] <- memory.[memory.[instructionPointer + 1]] * memory.[memory.[instructionPointer + 2]]
    runIntcode (instructionPointer + 4) memory
  | 99 -> memory.[0]
  | _ -> 
      printfn "Something went wrong"
      0

let input = File.ReadAllText("inputs/aoc02.txt").Split(',') |> Array.map int
let memory = input |> Array.copy
memory.[1] <- 12
memory.[2] <- 2

memory |> runIntcode 0  

// part 2

let targetOutput = 19690720

let changeInput (input: int[]) noun verb = 
  let modified = input |> Array.copy
  modified.[1] <- noun
  modified.[2] <- verb
  modified  

let nouns = [|0..99|]
let verbs = [|0..99|]

(nouns, verbs)
||> Array.allPairs 
|> Array.choose (fun (noun, verb) ->
    let memory = changeInput input noun verb
    if runIntcode 0 memory = targetOutput then 
      Some(100 * noun + verb)
    else
      None
    )

