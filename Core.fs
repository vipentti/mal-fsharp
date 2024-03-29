﻿module Core
    open Types
    open System

    let rec unpackNumber value = 
        match value with 
        | Number x -> x
        | List (_, [n]) -> unpackNumber n
        | Vector (_, [n]) -> unpackNumber n
        | String x -> int(float(x))
        | _        -> raise (Exception("Typemismatch"))


    let singleMathOp op values =
        let numbers = values |> List.map unpackNumber
        let result = numbers |> List.reduce op

        Number result

    let isEmpty args = 
        match args with 
        | ((List (_, xs)) | (Vector (_, xs))) :: anything -> Bool (xs.Length = 0)
        | _ -> Bool false

    let count = function
        | [List(_, xs) | Vector (_, xs)] ->
            Number xs.Length
        | [HashMap(_, xs)] -> Number xs.Count
        | [Nil] -> Number 0
        | [String s] -> Number s.Length
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
        | value :: ([List (m, items)] | [Vector (m, items)]) -> List (m, value :: items)
        | _ -> raise(Exception("Invalid cons arguments"))


    let conj = function
        | (Vector (_, vs)) :: args -> 
            makeVector (vs @ args)
        | (List (_, vs)) :: args -> 
            makeList ((List.rev args) @ vs)
        | _ -> raise(Exception("Invalid conj arguments"))

    let concat args = 
        match args with
        | []-> List (Nil, [])
        | (List (_, vs) :: rest) | (Vector (_, vs) :: rest) ->
            let result = 
                args
                |> List.map (function | (List (_, xs) | Vector (_, xs)) -> xs | _ -> [])
                |> List.filter (fun x -> x.Length > 0)
                |> List.concat

            List (Nil, result)
        | _ -> raise(Exception("Invalid concat arguments"))

    
    let nth = function
        | [] -> Nil
        | ((List (_, vs)) | (Vector (_, vs))) :: [Number n] -> 
            if n > vs.Length - 1 || n < 0 then
                raise(Exception("Index out of range " + n.ToString()))
            else
                vs.Item n

        | _ -> raise(Exception("Invalid nth arguments"))

    let first = function
        | [(List (_, vs)) | (Vector (_, vs))]-> 
            match vs with
            | [] -> Nil
            | _  -> List.head vs
        | _ -> Nil

    let rest = function
        | [(List (_, vs)) | (Vector (_, vs))] -> 
            match vs with
            | [] -> List (Nil, [])
            | _  -> List (Nil, (List.tail vs))
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
            
            match func with
            | PrimitiveFunction(_, _, f) 
            | Function(_, _, f, _, _, _) ->
                match finalArg with
//                | (List (_, []) | Vector (_, [])) -> 
//                    f (variableArgs @ [makeList []])

                | (List (_, vs) | Vector (_, vs)) -> 
                    f (variableArgs @ vs)
                
                | _ -> f (variableArgs @ [finalArg])
            | _ -> raise(Exception("Invalid apply arguments"))
        | _ -> raise(Exception("Invalid apply arguments"))

    
    let map = function
        | func :: [List (_, vs) | Vector (_, vs)] ->
            match func with
            | (PrimitiveFunction(_, _, f) | Function(_, _, f, _, _, _)) ->
                let temp = 
                    vs
                    |> List.map (fun x -> f [x])

                List (Nil, temp)
            | _ -> raise(Exception("Invalid apply arguments"))
        | _ -> raise(Exception("Invalid apply arguments"))

    let hashMap = function
        | args -> 
            Reader.splitListToPairs args
            |> Map.ofList
            |> makeHashMap

    let assoc = function
        | (HashMap (_, mp)) :: args ->
            let nextMap = 
                Reader.splitListToPairs args
                |> Map.ofList

            let ret = 
                Map.fold (fun acc key value -> Map.add key value acc) mp nextMap

            makeHashMap ret

        | _ -> raise(Exception("Invalid arguments"))

    let dissoc = function
        | (HashMap (_, mp)) :: args -> 

            mp
            |> Map.filter (fun key _ -> not (List.exists (fun x -> x = key) args))
            |> makeHashMap

        | _ -> raise(Exception("Invalid arguments"))

    let get = function
        | [(HashMap (_, mp)); key] ->
            if mp.ContainsKey key then
                mp.[key]
            else
                Nil
        | _ -> Nil

    let contains = function
        | [(HashMap (_, mp)); key] ->
            Bool (mp.ContainsKey key)
        | _ -> raise(Exception("Invalid arguments"))

    let keys = function
        | [HashMap (_, mp)] ->
            mp
            |> Map.toList
            |> List.map fst
            |> makeList
        | _ -> raise(Exception("Invalid arguments"))

    let vals = function
        | [HashMap (_, mp)] ->
            mp
            |> Map.toList
            |> List.map snd
            |> makeList
        | _ -> raise(Exception("Invalid arguments"))
    
    let sequential = isOfPattern (function Vector _ | List _ -> true | _ -> false)

    let throw = function
        | [value] -> raise <| MalException value
        | _ -> raise(Exception("Invalid throw arguments"))


    let meta = function
        | [func] -> 
            match func with 
            | List (m, _)
            | Vector (m, _)
            | HashMap (m, _)
            | PrimitiveFunction(m, _, _) 
            | Function(m, _, _, _, _, _)
            | Macro(m, _, _, _, _, _) -> 
                m
            | _ -> Nil
        | _ -> raise(Exception("Invalid arguments"))

    let withMeta = function
        | [obj; meta] -> setMeta meta obj
        | _ -> raise(Exception("Invalid arguments"))

    let isAtom = isOfPattern(function Atom _ -> true | _ -> false)

    let atom = function
        | [arg] -> Env.makeAtom arg
        | _ -> raise(Exception("Invalid arguments"))

    let deref = function
        | [Atom (_, arg)] -> !arg
        | _ -> raise(Exception("Invalid arguments"))

    let reset = function
        | [Atom (_, arg) as atm; value] -> 
            arg := value
            value
        | _ -> raise(Exception("Invalid arguments"))

    let swap = function
        | Atom (_, arg)
            ::(PrimitiveFunction(_, _, f) | Function(_, _, f, _, _, _))
            ::args ->
                let result = f (!arg :: args)
                arg := result
                !arg
        | _ -> raise(Exception("Invalid arguments"))


    let readline = function
        | [String prompt] -> 
            Console.Write(prompt)
            Console.Out.Flush()
            Types.String (Console.ReadLine())
        | _ -> 
            Types.String (Console.ReadLine())
    
    let coreFunctions = [ "+", singleMathOp (+)
                          "-", singleMathOp (-)
                          "*", singleMathOp (*)
                          "/", singleMathOp (fun x y -> int (x / y))
                          "list", makeList
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
                          "conj", conj
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
                          "keyword", (function [String s] -> Keyword ("\xff" + s) | [Keyword _ as kw] -> kw | _ -> raise(Exception("Invalid symbol")))


                          "symbol?", isSymbol
                          "keyword?", isKeyword
                          "vector?", isVector

                          "vector", makeVector


                          "assoc", assoc
                          "dissoc", dissoc
                          "get", get
                          "contains?", contains
                          "keys", keys
                          "vals", vals
                          "sequential?", sequential

                          "meta", meta
                          "with-meta", withMeta

                          "atom", atom
                          "atom?", isAtom

                          "deref", deref
                          "reset!", reset
                          "swap!", swap

                          "readline", readline
                          ]