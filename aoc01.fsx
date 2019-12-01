open System.IO

// part 1

let calculateFuel mass =
  float mass / 3. 
  |> floor 
  |> fun x -> int x - 2

let input = 
  File.ReadAllLines "inputs/aoc01.txt" 
  |> Array.map int

let fuel1 = 
  input |> Array.sumBy calculateFuel  

printfn "Total fuel: %d" fuel1

// part 2  

let rec calculateTotalFuel totalFuel mass =
  let moreFuel = calculateFuel mass
  if moreFuel <= 0 then
    totalFuel
  else
    calculateTotalFuel (totalFuel + moreFuel) moreFuel

let fuel2 =
  input |> Array.sumBy (fun f -> calculateTotalFuel 0 f)

printfn "Total fuel taking into account fuel itself: %d" fuel2