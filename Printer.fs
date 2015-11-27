namespace MAL

module Printer =
    open Types

    let rec PrStr value = 
        match value with 
        | Atom v -> v
        | Number v -> v.ToString()
        | List vs -> List.map PrStr vs |> String.concat " " |> (fun x -> "(" + x + ")")
        | _ -> "<not supported>"


