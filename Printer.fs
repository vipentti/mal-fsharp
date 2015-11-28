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
        | List (_, vs) -> 
            List.map (PrStr readably) vs |> String.concat " " |> (fun x -> "(" + x + ")")

        | Vector (_, vs) -> 
            List.map (PrStr readably) vs |> String.concat " " |> (fun x -> "[" + x + "]")

        | Nil -> "nil"

        | Function _ -> "#<function>"

        | PrimitiveFunction _ -> "#<primitive_function>"

        | Macro _ -> "#<macro>"

        | HashMap (_, v) ->
            v
            |> Map.toList
            |> List.map (fun (key, value) -> (PrStr readably key) + " " + (PrStr readably value))
            |> String.concat " "
            |> (fun x -> "{" + x + "}")

        | Atom (_, v) -> 
            let value = !v
            sprintf "(atom %s)" (PrStr true value)