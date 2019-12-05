open System.IO

type ParameterMode =
  | Position   // position in memory
  | Immediate  // value

let getParameterMode x = if x = '0' then Position else Immediate

let parseInstruction instruction =
  let fullInstruction = sprintf "%05d" instruction // assuming maximum of 3 arguments
  let opcode = fullInstruction.[3..4] |> int
  let parameters = fullInstruction.[0..2] |> Seq.map getParameterMode |> Array.ofSeq |> Array.rev
  opcode, parameters

let getParamValue (memory: int[]) paramMode idx =
  match paramMode with
  | Position -> memory.[memory.[idx]]
  | Immediate -> memory.[idx]

let saveResult (memory: int[]) paramMode idx value =
  match paramMode with
  | Position -> memory.[memory.[idx]] <- value
  | Immediate -> printfn "This should not happen." // Parameters that an instruction writes to will never be in immediate mode
  memory

let rec runIntcode (input: int) instructionPointer (memory: int[])  =
  let opcode, paramModes = parseInstruction memory.[instructionPointer]
  match opcode with

  | 1 -> 
    let param1 = getParamValue memory paramModes.[0] (instructionPointer + 1)
    let param2 = getParamValue memory paramModes.[1] (instructionPointer + 2)
    param1 + param2 
    |> saveResult memory paramModes.[2] (instructionPointer+3) 
    |> runIntcode input (instructionPointer + 4)

  | 2 ->
    let param1 = getParamValue memory paramModes.[0] (instructionPointer + 1)
    let param2 = getParamValue memory paramModes.[1] (instructionPointer + 2)
    param1 * param2 
    |> saveResult memory paramModes.[2] (instructionPointer+3) 
    |> runIntcode input (instructionPointer + 4)

  | 3 -> 
    // write input
    input
    |> saveResult memory paramModes.[0] (instructionPointer + 1) 
    |> runIntcode input (instructionPointer + 2)

  | 4 -> 
    // print value
    let value = getParamValue memory paramModes.[0] (instructionPointer + 1)
    printfn "%d" value
    runIntcode input (instructionPointer + 2) memory

  | 5 ->
    // jump if true
    let param1 = getParamValue memory paramModes.[0] (instructionPointer + 1)
    if param1 <> 0 then
      let instructionPointer' = getParamValue memory paramModes.[1] (instructionPointer + 2)
      runIntcode input instructionPointer' memory
    else
      runIntcode input (instructionPointer + 3) memory
  
  | 6 ->
    // jump if false
    let param1 = getParamValue memory paramModes.[0] (instructionPointer + 1)
    if param1 = 0 then
      let instructionPointer' = getParamValue memory paramModes.[1] (instructionPointer + 2)
      runIntcode input instructionPointer' memory
    else
      runIntcode input (instructionPointer + 3) memory

  | 7 ->
    // less than
    let param1 = getParamValue memory paramModes.[0] (instructionPointer + 1)
    let param2 = getParamValue memory paramModes.[1] (instructionPointer + 2)
    
    if param1 < param2 then 1 else 0
    |> saveResult memory paramModes.[2] (instructionPointer + 3) 
    |> runIntcode input (instructionPointer + 4)

  | 8 ->
    // equals
    let param1 = getParamValue memory paramModes.[0] (instructionPointer + 1)
    let param2 = getParamValue memory paramModes.[1] (instructionPointer + 2)
    
    if param1 = param2 then 1 else 0
    |> saveResult memory paramModes.[2] (instructionPointer + 3) 
    |> runIntcode input (instructionPointer + 4)    

  | 99 -> printfn "Halt"; 0
  | _ -> 
      printfn "Something went wrong"
      0

let input = File.ReadAllText("inputs/aoc05.txt").Split(',') |> Array.map int

// part 1
let memory1 = input |> Array.copy
memory1 |> runIntcode 1 0  

// part 2
let memory2 = input |> Array.copy
memory2 |> runIntcode 5 0
