module Printer
    open Types

    let printString readably (str : string) = 
        if readably then
            "\"" +  str.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\n", "\\n") + "\""
        else
            str

    let rec PrStr readably value = 
        match value with 
        | Symbol v -> v
        | Number v -> v.ToString()
        | String v -> printString readably v 
        | Keyword v -> ":" + v.Substring(1)
        | Bool v -> match v with | true -> "true" | false -> "false"
        | List vs -> 
            List.map (PrStr readably) vs |> String.concat " " |> (fun x -> "(" + x + ")")

        | Vector vs -> 
            List.map (PrStr readably) vs |> String.concat " " |> (fun x -> "[" + x + "]")

        | Nil -> "nil"

        | Function _ -> "#<function>"

        | PrimitiveFunction _ -> "#<primitive_function>"

        | HashMap v ->
            v
            |> Map.toList
            |> List.map (fun (key, value) -> (PrStr readably key) + " " + (PrStr readably value))
            |> String.concat " "
            |> (fun x -> "{" + x + "}")

        | _ -> "<not supported>"


