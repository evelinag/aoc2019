open System.IO

type ParameterMode =
  | Position   // position in memory
  | Immediate  // value

let getParameterMode x = if x = '0' then Position else Immediate

let (|Instruction|) (memory:_[], instructionPointer) =
  let instruction = memory.[instructionPointer]
  let fullInstruction = sprintf "%05d" instruction // assuming maximum of 3 arguments
  let opcode = fullInstruction.[3..4] |> int
  let parameters = fullInstruction.[0..2] |> Seq.map getParameterMode |> Array.ofSeq |> Array.rev
  opcode, (memory, instructionPointer, parameters)

let getParamValue (memory: int[]) paramMode idx =
  match paramMode with
  | Position -> memory.[memory.[idx]]
  | Immediate -> memory.[idx]

let (|Param|) idx (memory, instructionPointer, parameters:_[]) =
  getParamValue memory parameters.[idx] (instructionPointer + 1 + idx)

let (|ParamMode|) idx (_, _, parameters:_[]) = 
  parameters.[idx]

let saveResult (memory: int[]) paramMode idx value =
  match paramMode with
  | Position -> memory.[memory.[idx]] <- value
  | Immediate -> printfn "This should not happen." // Parameters that an instruction writes to will never be in immediate mode
  memory

let rec runIntcode (input: int) instructionPointer (memory: int[])  =
  match memory, instructionPointer with 
  | Instruction(1, Param 0 param1 & Param 1 param2 & ParamMode 2 outMode) -> 
    param1 + param2 
    |> saveResult memory outMode (instructionPointer+3) 
    |> runIntcode input (instructionPointer + 4)

  | Instruction(2, Param 0 param1 & Param 1 param2 & ParamMode 2 outMode) -> 
    param1 * param2 
    |> saveResult memory outMode (instructionPointer+3) 
    |> runIntcode input (instructionPointer + 4)

  | Instruction(3, ParamMode 0 outMode) -> 
    // write input
    input
    |> saveResult memory outMode (instructionPointer + 1) 
    |> runIntcode input (instructionPointer + 2)

  | Instruction(4, Param 0 value) -> 
    printfn "%d" value
    runIntcode input (instructionPointer + 2) memory

  | Instruction(5, Param 0 0) -> 
    // jump if true
    runIntcode input (instructionPointer + 3) memory
  | Instruction(5, Param 0 _ & ParamMode 1 outMode) -> 
    let instructionPointer' = getParamValue memory outMode (instructionPointer + 2)
    runIntcode input instructionPointer' memory
  
  | Instruction(6, Param 0 0 & ParamMode 1 outMode) -> 
    // jump if false
    let instructionPointer' = getParamValue memory outMode (instructionPointer + 2)
    runIntcode input instructionPointer' memory
  | Instruction(6, Param 0 _) -> 
    runIntcode input (instructionPointer + 3) memory

  | Instruction(7, Param 0 param1 & Param 1 param2 & ParamMode 2 outMode) -> 
    // less than
    if param1 < param2 then 1 else 0
    |> saveResult memory outMode (instructionPointer + 3) 
    |> runIntcode input (instructionPointer + 4)

  | Instruction(8, Param 0 param1 & Param 1 param2 & ParamMode 2 outMode) -> 
    // equals
    if param1 = param2 then 1 else 0
    |> saveResult memory outMode (instructionPointer + 3) 
    |> runIntcode input (instructionPointer + 4)    

  | Instruction(99, _) -> printfn "Halt"; 0
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
