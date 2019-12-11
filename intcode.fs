module Intcode

open System.Collections.Generic

type IntCodeMachine =
  { 
    Memory : Dictionary<int64, int64>
    InstructionPointer : int64
    RelativeBase : int64
    OnHold : bool
  }

type ParameterMode =
  | Position   // position in memory
  | Immediate  // value
  | Relative

let getParameterMode x = 
  match x with
  | '0' -> Position
  | '1' -> Immediate
  | '2' -> Relative
  | _ -> 
    printfn "Unknown parameter mode %A" x
    Position // default

let parseInstruction computer =
  let fullInstruction = 
    if computer.Memory.ContainsKey computer.InstructionPointer then
      let instruction = computer.Memory.[computer.InstructionPointer]
      sprintf "%05d" instruction // assuming maximum of 3 arguments
    else
      "0L0L0L0L0L"
  let opcode = fullInstruction.[3..4] |> int
  let parameters = fullInstruction.[0..2] |> Seq.map getParameterMode |> Array.ofSeq |> Array.rev
  opcode, parameters

let getParamValue (computer : IntCodeMachine) idx paramMode =
  let address =
    match paramMode with
    | Position -> computer.Memory.[idx]
    | Immediate -> idx
    | Relative -> computer.Memory.[idx] + computer.RelativeBase
  if computer.Memory.ContainsKey address then
    computer.Memory.[address]
  else
    0L

let saveResult (computer : IntCodeMachine) idx paramMode value =
  let address =
    match paramMode with
    | Position -> computer.Memory.[idx]
    | Immediate -> printfn "This should not happen."; 0L // Parameters that an instruction writes to will never be in immediate mode
    | Relative -> computer.Memory.[idx] + computer.RelativeBase
  if computer.Memory.ContainsKey address then
    computer.Memory.[address] <- value
  else
    computer.Memory.Add(address, value)
  computer

let rec runIntcode (inputs: int64 list) (outputs: int64 list) (computer : IntCodeMachine)  =
  let opcode, paramModes = parseInstruction computer
  match opcode with

  | 1 -> 
    let param1 = getParamValue computer (computer.InstructionPointer + 1L) paramModes.[0]
    let param2 = getParamValue computer (computer.InstructionPointer + 2L) paramModes.[1]
    param1 + param2 
    |> saveResult computer (computer.InstructionPointer + 3L) paramModes.[2]  
    |> fun computer' -> { computer' with InstructionPointer = computer'.InstructionPointer + 4L }
    |> runIntcode inputs outputs 

  | 2 ->
    let param1 = getParamValue computer (computer.InstructionPointer + 1L) paramModes.[0]
    let param2 = getParamValue computer (computer.InstructionPointer + 2L) paramModes.[1]
    param1 * param2 
    |> saveResult computer (computer.InstructionPointer + 3L) paramModes.[2]  
    |> fun computer' -> { computer' with InstructionPointer = computer'.InstructionPointer + 4L }
    |> runIntcode inputs outputs 

  | 3 -> 
    // write input
    if inputs.Length = 0 then
      // return memory state and "wait" for next input
      outputs, Some { computer with OnHold = true }
    else
      inputs.Head
      |> saveResult computer (computer.InstructionPointer + 1L) paramModes.[0]  
      |> fun computer' -> { computer' with InstructionPointer = computer'.InstructionPointer + 2L }
      |> runIntcode inputs.Tail outputs 

  | 4 -> 
    // output value
    let value = getParamValue computer (computer.InstructionPointer + 1L) paramModes.[0]
    { computer with InstructionPointer = computer.InstructionPointer + 2L }
    |> runIntcode inputs (value::outputs)

  | 5 ->
    // jump if true
    let param1 = getParamValue computer (computer.InstructionPointer + 1L) paramModes.[0]
    if param1 <> 0L then
      let instructionPointer' = getParamValue computer (computer.InstructionPointer + 2L) paramModes.[1]
      { computer with InstructionPointer = instructionPointer'}
      |> runIntcode inputs outputs 
    else
      { computer with InstructionPointer = computer.InstructionPointer + 3L }
      |> runIntcode inputs outputs
  
  | 6 ->
    // jump if false
    let param1 = getParamValue computer (computer.InstructionPointer + 1L) paramModes.[0]
    if param1 = 0L then
      let instructionPointer' = getParamValue computer (computer.InstructionPointer + 2L) paramModes.[1]
      { computer with InstructionPointer = instructionPointer'}
      |> runIntcode inputs outputs 
    else
      { computer with InstructionPointer = computer.InstructionPointer + 3L }
      |> runIntcode inputs outputs

  | 7 ->
    // less than
    let param1 = getParamValue computer (computer.InstructionPointer + 1L) paramModes.[0]
    let param2 = getParamValue computer (computer.InstructionPointer + 2L) paramModes.[1]
    
    if param1 < param2 then 1L else 0L
    |> saveResult computer (computer.InstructionPointer + 3L) paramModes.[2]  
    |> fun computer' -> { computer' with InstructionPointer = computer'.InstructionPointer + 4L }
    |> runIntcode inputs outputs 

  | 8 ->
    // equals
    let param1 = getParamValue computer (computer.InstructionPointer + 1L) paramModes.[0]
    let param2 = getParamValue computer (computer.InstructionPointer + 2L) paramModes.[1]
    
    if param1 = param2 then 1L else 0L
    |> saveResult computer (computer.InstructionPointer + 3L) paramModes.[2]  
    |> fun computer' -> { computer' with InstructionPointer = computer'.InstructionPointer + 4L }
    |> runIntcode inputs outputs 
  
  | 9 ->
    // increment relative base
    let value = getParamValue computer (computer.InstructionPointer + 1L) paramModes.[0]
    { computer with RelativeBase = computer.RelativeBase + value}
    |> fun computer' -> { computer' with InstructionPointer = computer'.InstructionPointer + 2L }
    |> runIntcode inputs outputs 

  | 99 -> 
      // return output and current state of program/memory
      outputs, None

  | _ -> 
      printfn "Something went wrong"
      [], None

let executeIntcode inputs (code: int64 []) (computer: IntCodeMachine option) =
  match computer with
  | Some comp when comp.OnHold ->
    // existing machine
    { comp with OnHold = false } |> runIntcode inputs []
  | _ ->
    // copy code into memory
    let memory: Dictionary<int64,int64> = Dictionary<int64,int64>(code |> Array.mapi (fun i x -> KeyValuePair(int64 i, x)))
    {
      Memory = memory
      InstructionPointer = 0L
      RelativeBase = 0L
      OnHold = false
    }
    |> runIntcode inputs []
