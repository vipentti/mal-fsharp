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

    let readString args = 
        match args with
        | [String s] -> Reader.ReadStr s
        | _ -> raise(Exception("Invalid string type"))


    let slurp args = 
        match args with
        | [String fileName] -> 
            let text = System.IO.File.ReadAllText(fileName)
            Types.String text
        | _ -> raise(Exception("Invalid filename"))


    let cons args = 
        match args with
        | value :: ([List items] | [Vector items]) -> List (value :: items)
        | _ -> raise(Exception("Invalid cons arguments"))

    let concat args = 
        match args with
        | []-> List []
        | (List vs :: rest) | (Vector vs :: rest) ->
            let result = 
                args
                |> List.map (function | (List xs | Vector xs) -> xs | _ -> [])
                |> List.filter (fun x -> x.Length > 0)
                |> List.concat

            List result
        | _ -> raise(Exception("Invalid concat arguments"))

    
    let nth = function
        | [] -> Nil
        | ((List vs) | (Vector vs)) :: [Number n] -> 
            if n > vs.Length - 1 || n < 0 then
                raise(Exception("Index out of range " + n.ToString()))
            else
                vs.Item n

        | _ -> raise(Exception("Invalid nth arguments"))

    let first = function
        | [(List vs) | (Vector vs)]-> 
            match vs with
            | [] -> Nil
            | _  -> List.head vs
        | _ -> Nil

    let rest = function
        | [(List vs) | (Vector vs)] -> 
            match vs with
            | [] -> List []
            | _  -> List (List.tail vs)
        | _ -> Nil



    let isOfPattern f = function
        | [arg] -> if f arg then (Bool true) else Bool false
        | _ -> raise(Exception("Invalid arguments to pattern"))

    let isList = isOfPattern (function List _ -> true | _ -> false)
    let isNil = isOfPattern (function Nil -> true | _ -> false)
    let isTrue = isOfPattern (function Bool true -> true | _ -> false)
    let isFalse = isOfPattern (function Bool false -> true | _ -> false)
    let isSymbol = isOfPattern (function Symbol _ -> true | _ -> false)
    let isKeyword = isOfPattern (function Keyword _ -> true | _ -> false)
    let isVector = isOfPattern (function Vector _ -> true | _ -> false)
    let isMap = isOfPattern (function HashMap _ -> true | _ -> false)


    let apply = function
        | func :: rest ->
            let variableArgs = 
                rest
                |> List.rev
                |> List.tail
                |> List.rev

            let finalArg = 
                rest 
                |> List.rev
                |> List.head
            
            match func, finalArg with
            | (PrimitiveFunction(_, f) | Function(_, f, _, _, _)), 
              (List vs | Vector vs) -> 
                f (variableArgs @ vs)
            | _ -> raise(Exception("Invalid apply arguments"))
        | _ -> raise(Exception("Invalid apply arguments"))

    
    let map = function
        | func :: [List vs | Vector vs] ->
            match func with
            | (PrimitiveFunction(_, f) | Function(_, f, _, _, _)) ->
                vs
                |> List.map (fun x -> f [x])
                |> Types.List
            | _ -> raise(Exception("Invalid apply arguments"))
        | _ -> raise(Exception("Invalid apply arguments"))


    let vector = function
        | args ->
            Vector args

    let hashMap = function
        | args -> 
            Reader.splitListToPairs args
            |> Map.ofList
            |> HashMap

    let assoc = function
        | (HashMap mp) :: args ->
            let nextMap = 
                Reader.splitListToPairs args
                |> Map.ofList

            let ret = 
                Map.fold (fun acc key value -> Map.add key value acc) mp nextMap

            HashMap ret

        | _ -> raise(Exception("Invalid arguments"))

    let dissoc = function
        | (HashMap mp) :: args -> 

            mp
            |> Map.filter (fun key _ -> not (List.exists (fun x -> x = key) args))
            |> HashMap

        | _ -> raise(Exception("Invalid arguments"))

    let throw = function
        | [value] -> raise <| MalException value
        | _ -> raise(Exception("Invalid throw arguments"))

    let coreFunctions = [ "+", singleMathOp (+)
                          "-", singleMathOp (-)
                          "*", singleMathOp (*)
                          "/", singleMathOp (fun x y -> int (x / y))
                          "list", List
                          "list?", isList
                          "empty?", isEmpty
                          "count", count
                          "=", boolBinop (=)
                          ">", boolBinop (>)
                          ">=", boolBinop (>=)
                          "<", boolBinop (<)
                          "<=", boolBinop (<=)
                          "pr-str", prstr
                          "str", str
                          "prn", prn
                          "println", println 
                          "read-string", readString
                          "slurp", slurp

                          "cons", cons
                          "concat", concat

                          "nth", nth
                          "first", first
                          "rest", rest

                          "nil?", isNil
                          "true?", isTrue
                          "false?", isFalse
                          "symbol?", isSymbol

                          "apply", apply

                          "map", map

                          "map?", isMap
                          "hash-map", hashMap

                          "throw", throw

                          "symbol", (function [String s] -> Symbol s | _ -> raise(Exception("Invalid symbol")))
                          "keyword", (function [String s] -> Keyword s | [Keyword _ as kw] -> kw | _ -> raise(Exception("Invalid symbol")))


                          "symbol?", isSymbol
                          "keyword?", isKeyword
                          "vector?", isVector

                          "vector", vector


                          "assoc", assoc
                          "dissoc", dissoc
                          "get", noop
                          "contains?", noop
                          "keys", noop
                          "vals", noop
                          "sequential?", noop
                          ]