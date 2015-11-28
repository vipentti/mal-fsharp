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
        | ((List xs) | (Vector xs)) :: anything -> Bool (xs.Length = 0)
        | _ -> Bool false

    let count args = 
        match args with 
        | ((List xs) | (Vector xs)) :: anything -> Number xs.Length
        | _ -> Number 0

    let equal args = 
        match args with
        | a :: b :: rest -> Bool (a = b)
        | _ -> Bool false

    let boolBinop op values = 
        match values with
        | a :: b :: rest -> Bool (op a b)
        | _ -> Bool false


    let noop (args : list<MalType>) = Nil
        
    let prstr args = 
        args
        |> List.map (Printer.PrStr true)
        |> String.concat " "
        |> Types.String

    let str args = 
        args
        |> List.map (Printer.PrStr false)
        |> String.concat ""
        |> Types.String

    let prn args = 
        let str = 
            args
            |> List.map (Printer.PrStr true)
            |> String.concat " "

        //printfn "%s" (str.Replace("\r\r\n", "\r\n"))
        //printfn "%s" (str)

        //NOTE(ville): This is a bit of a hack
        Console.Write(str + "\n")
        Nil

    let println args = 
        let str = 
            args
            |> List.map (Printer.PrStr false)
            |> String.concat " "

        //NOTE(ville): This is a bit of a hack
        Console.Write(str + "\n")
        Nil
