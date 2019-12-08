open System.IO

let wide, tall = 25, 6
let signal = 
  //"123456789012" 
  (File.ReadAllText "inputs/aoc08.txt").TrimEnd()
  |> Seq.toArray
  |> Array.map (string >> int)

let flatLayers = 
  signal
  |> Array.chunkBySize (wide*tall)

let layers =
  flatLayers  
  |> Array.map (fun flatLayer -> 
      Array2D.init tall wide (fun i j -> flatLayer.[j + i*wide]))

let countDigits digit layer =
  layer 
  |> Array.sumBy (fun x -> if x = digit then 1 else 0)

let checkLayer =
  let minZeroLayer =
    flatLayers
    |> Array.minBy (countDigits 0)
  (countDigits 1 minZeroLayer) * (countDigits 2 minZeroLayer)

// part 2
// 0 is black, 1 is white, and 2 is transparent.

let image = 
  Array2D.init tall wide (fun i j ->
    let result = 
      layers |> Array.choose (fun layer -> if layer.[i,j] <> 2 then Some layer.[i,j] else None)
    result.[0])

// plot image
let plotImage (image:int[,]) =
  printfn "\n\n"
  [ for i in [0 .. 1 .. tall-1] ->
    [ for j in [0 .. 1 .. wide-1] ->
        if image.[i,j] = 1 then "X" else " " ]
    |> String.concat "" ]
  |> List.iter (fun line -> printf "%s\n" line)
  printfn "\n\n"
    
image |> plotImage

