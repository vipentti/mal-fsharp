namespace MAL

module Printer =
    open Types

    let rec PrStr value = 
        match value with 
        | Atom v -> v
        | Number v -> v.ToString()
        | Bool v -> match v with | true -> "true" | false -> "false"
        | List vs -> List.map PrStr vs |> String.concat " " |> (fun x -> "(" + x + ")")
        | Nil -> "nil"
        | _ -> "<not supported>"


