module Core
    open Types
    open System

    let rec unpackNumber value = 
        match value with 
        | Number x -> x
        | List [n] -> unpackNumber n
        | String x -> int(float(x))
        | _        -> raise (Exception("Typemismatch"))


    let singleMathOp op values =
        let numbers = values |> List.map unpackNumber
        let result = numbers |> List.reduce op

        Number result

    let isList args = 
        match args with 
        | List xs :: anything -> Bool true
        | _ -> Bool false

    let isEmpty args = 
        match args with 
        | List xs :: anything -> Bool (xs.Length = 0)
        | _ -> Bool false


    let count args = 
        match args with 
        | List xs :: anything -> Number xs.Length
        | _ -> Number 0

    let equal args = 
        match args with
        | a :: b :: rest -> Bool (a = b)
        | _ -> Bool false

    let boolBinop op values = 
        match values with
        | a :: b :: rest -> Bool (op a b)
        | _ -> Bool false