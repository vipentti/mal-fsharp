module Main 
    open System
    open Reader
    open Printer
    open Types

    open Env


    let initialEnv = makeRootEnv ()

    let rec READ str =
        Reader.ReadStr str

    and evalAst (env : EnvChain) ast = 
        match ast with
        | Symbol v -> get env v
        | List vs -> vs |> List.map (EVAL env) |> List
        | Vector vs -> vs |> List.map (EVAL env) |> Vector
        | HashMap map ->
            map
            |> Map.map (fun key value -> EVAL env value)
            |> HashMap
        | _ -> ast


    and ifForm env args = 

        let innerIf condition trueForm falseForm = 
            match EVAL env condition with
            | Nil | Bool false -> falseForm
            | _ -> trueForm

        match args with
        | [condition; trueForm; falseForm] -> innerIf condition trueForm falseForm
        | [condition; trueForm] -> innerIf condition trueForm Nil
        | _ -> raise(Exception("Invalid if Form"))

    and doForm env args =
        match args with
        | [a] -> a
        | a::rest ->
            EVAL env a |> ignore
            doForm env rest
        | _ -> raise(Exception("Something"))

    
    and letStarForm env args = 
        match args with
        | [bindings; calls] ->
            let newChain = makeNewEnv env [] []

            let updateEnv (key, value) =
                match key with
                | Symbol v -> set newChain v (EVAL newChain value)
                | _ -> ()
            
            match bindings with 
            | List vs | Vector vs -> splitListToPairs vs |> List.iter updateEnv 
            | _ -> raise(Exception("Invalid let* form"))

            newChain, calls
            
        | _ -> raise(Exception("Invalid form"))

    and EVAL (env : EnvChain) ast =
        match ast with

        | List (Symbol "do" :: rest) -> doForm env rest |> EVAL env

        | List (Symbol "if" :: rest) -> ifForm env rest |> EVAL env

        | List [Symbol "def!"; Symbol name; form] ->
            let evaled = EVAL env form
            set env name evaled
            evaled

        | List (Symbol "let*" :: rest) ->
            let newChain, calls = letStarForm env rest
            EVAL newChain calls

        | List [Symbol "fn*"; (List args) | (Vector args); body] ->
            let temp = makeFunction Core.noop body args env
            temp
            
        | List (func :: args) as item-> 
            let values = evalAst env item
            match values with 
            | List (func :: args) ->
                match func with
                | PrimitiveFunction(_, f) -> f args
                | Function(_, _, body, binds, env) ->
                    let newEnv = makeNewEnv env binds args
                    body |> EVAL newEnv
                | _ -> raise(Exception("Invalid function"))
            | _ -> raise(Exception("Invalid form"))

        | _ -> evalAst env ast

    and PRINT exp =
        Printer.PrStr exp

    and REP str =
        str |> READ |> (EVAL initialEnv) |> PRINT true

    let read (prompt :string) = 
        Console.Write(prompt)
        Console.Out.Flush()
        Console.ReadLine()
    
    [<EntryPoint>]
    let main argv = 

        ignore (REP "(def! not (fn* (a) (if a false true)))")

        let rec loop () =
            match read "user> " with
            | null -> 0
            | input -> 
                try 
                    printfn "%s" (REP input)
                with
                    | ex -> printfn "%s" ex.Message
                loop()
        loop()