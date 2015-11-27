namespace MAL

module Printer =
    open Types

    let printString (str : string) = 
        "\"" +  str.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\n", "\\n") + "\""

    let rec PrStr value = 
        match value with 
        | Symbol v -> v
        | Number v -> v.ToString()
        | String v -> printString v
        | Keyword v -> v.Substring(1)
        | Bool v -> match v with | true -> "true" | false -> "false"
        | List vs -> List.map PrStr vs |> String.concat " " |> (fun x -> "(" + x + ")")
        | Nil -> "nil"
        | _ -> "<not supported>"


