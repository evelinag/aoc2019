
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

let rec runIntcode (inputs: int list) instructionPointer (outputs: int list) (memory: int[])  =
  let opcode, paramModes = parseInstruction memory.[instructionPointer]
  match opcode with

  | 1 -> 
    let param1 = getParamValue memory paramModes.[0] (instructionPointer + 1)
    let param2 = getParamValue memory paramModes.[1] (instructionPointer + 2)
    param1 + param2 
    |> saveResult memory paramModes.[2] (instructionPointer+3) 
    |> runIntcode inputs (instructionPointer + 4) outputs

  | 2 ->
    let param1 = getParamValue memory paramModes.[0] (instructionPointer + 1)
    let param2 = getParamValue memory paramModes.[1] (instructionPointer + 2)
    param1 * param2 
    |> saveResult memory paramModes.[2] (instructionPointer+3) 
    |> runIntcode inputs (instructionPointer + 4) outputs

  | 3 -> 
    // write input
    if inputs.Length = 0 then
      printfn "Waiting for input..."
      // return memory state and "wait" for next input
      outputs, memory, Some(instructionPointer)
    else
      inputs.Head
      |> saveResult memory paramModes.[0] (instructionPointer + 1) 
      |> runIntcode inputs.Tail (instructionPointer + 2) outputs

  | 4 -> 
    // output value
    let value = getParamValue memory paramModes.[0] (instructionPointer + 1)
    runIntcode inputs (instructionPointer + 2) (value::outputs) memory 

  | 5 ->
    // jump if true
    let param1 = getParamValue memory paramModes.[0] (instructionPointer + 1)
    if param1 <> 0 then
      let instructionPointer' = getParamValue memory paramModes.[1] (instructionPointer + 2)
      runIntcode inputs instructionPointer' outputs memory
    else
      runIntcode inputs (instructionPointer + 3) outputs memory
  
  | 6 ->
    // jump if false
    let param1 = getParamValue memory paramModes.[0] (instructionPointer + 1)
    if param1 = 0 then
      let instructionPointer' = getParamValue memory paramModes.[1] (instructionPointer + 2)
      runIntcode inputs instructionPointer' outputs memory
    else
      runIntcode inputs (instructionPointer + 3) outputs memory

  | 7 ->
    // less than
    let param1 = getParamValue memory paramModes.[0] (instructionPointer + 1)
    let param2 = getParamValue memory paramModes.[1] (instructionPointer + 2)
    
    if param1 < param2 then 1 else 0
    |> saveResult memory paramModes.[2] (instructionPointer + 3) 
    |> runIntcode inputs (instructionPointer + 4) outputs

  | 8 ->
    // equals
    let param1 = getParamValue memory paramModes.[0] (instructionPointer + 1)
    let param2 = getParamValue memory paramModes.[1] (instructionPointer + 2)
    
    if param1 = param2 then 1 else 0
    |> saveResult memory paramModes.[2] (instructionPointer + 3) 
    |> runIntcode inputs (instructionPointer + 4) outputs

  | 99 -> 
      // return output and current state of program/memory
      outputs, memory, None

  | _ -> 
      printfn "Something went wrong"
      [], memory, None

let executeIntcode inputs (code, instructionPointer) = 
  // copy code into memory
  let memory = code |> Array.copy
  // run the program with the given input
  match instructionPointer with
  | None ->
    memory |> runIntcode inputs 0 []
  | Some p ->
    memory |> runIntcode inputs p []

// ==========================================
// Part 1

let thrust input code settings =
  (input, settings)
  ||> List.fold (fun output setting ->
      executeIntcode [setting; output] code
      |> fun (value, memory, finished) -> value  
      |> List.exactlyOne)

let rec permutations list taken = 
  [ if Set.count taken = List.length list then yield [] else
        for l in list do
          if not (Set.contains l taken) then 
            for perm in permutations list (Set.add l taken)  do
              yield l::perm ]


let input = 0

let code = 
  File.ReadAllText("inputs/aoc07.txt").Split(',') 
  |> Array.map int

let maxThrust =
  // all possible permutations of allowed settings, i.e. 0..4
  permutations [ 0 .. 4 ] Set.empty
  |> List.map (fun settings -> settings, thrust input (code, None) settings)
  |> List.maxBy snd

// Part 2 - feedback loop
// This all should be much nicer with agents (mailboxprocessor)

let feedbackThrust input code settings =

  // run first iteration
  let signal, memories =
    ((input, []), settings)
    ||> List.fold (fun (signal, memories) setting ->
        let output, memory, instructionPointer = executeIntcode [setting; signal] code
        output.Head, (memory, instructionPointer)::memories
        )
  let memoryStates = memories |> List.rev

  let rec feedbackThrustIteration signal memoryStates =
    let result, memoryStates', finished = 
      ((signal, [], false), memoryStates)
      ||> List.fold (fun (inp, memories, _) previousMemory ->
          let output, memory, instructionPointer = executeIntcode [inp] previousMemory
          output.Head, (memory, instructionPointer)::memories, instructionPointer.IsNone
          )    
    if finished then
      result
    else
      feedbackThrustIteration result (memoryStates' |> List.rev)
  
  feedbackThrustIteration signal memoryStates


let maxFeedbackThrust =
  permutations [ 5 .. 9 ] Set.empty
  |> List.map (fun settings -> settings, feedbackThrust input (code, None) settings)
  |> List.maxBy snd
