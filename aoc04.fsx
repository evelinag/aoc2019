
let lowest, highest = 245182,790572

let checkPassword (password: string) =
  let repeats =
    password 
    |> Seq.pairwise    
    |> Seq.filter (fun (x,y) -> x = y)
    |> Seq.length
    |> (<) 0
  if repeats then
    let increasing =
      (true, [1..(Seq.length password)-1])
      ||> List.fold (fun isIncreasing i -> isIncreasing && (password.[i-1] <= password.[i]))
    increasing
  else
    false


[ lowest .. highest ]
|> List.filter (fun password ->
    checkPassword (string password)
    )
|> List.length

// part 2

let checkPassword2 (password: string) =
  let fancyRepeats =
    password 
    |> Seq.pairwise    
    |> Seq.filter (fun (x,y) -> x = y)
    |> Seq.groupBy fst
    |> Seq.filter (fun (_, group) -> Seq.length group = 1)
    |> Seq.length
    |> (<) 0
  if fancyRepeats then
    (true, [1..(Seq.length password)-1])
    ||> List.fold (fun isIncreasing i -> isIncreasing && (password.[i-1] <= password.[i]))
  else
    false


[ lowest .. highest ]
|> List.filter (fun password ->
    checkPassword2 (string password)
    )
|> List.length
