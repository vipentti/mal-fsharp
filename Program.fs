module Main 
    open System
    open Reader
    open Printer
    open Types

    open Env


    let initialEnv = makeRootEnv ()

    let rec READ str =
        Reader.ReadStr str


    and apply fn args = 
        match fn with
        | PrimitiveFunction(_, f) -> f args
        | Function(_, _, body, binds, env) ->
            let newEnv = makeNewEnv env binds args
            EVAL body newEnv
        | _ -> raise(Exception("Invalid function"))

    and evalAst ast (env : EnvChain) = 
        match ast with
        | Symbol v -> get env v
        | List vs -> vs |> List.map (fun x -> EVAL x env) |> List
        | Vector vs -> vs |> List.map (fun x -> EVAL x env) |> Vector
        | HashMap map ->
            map
            |> Map.map (fun key value -> EVAL value env)
            |> HashMap
        | _ -> ast

    and EVAL ast (env : EnvChain) =
        match ast with

        | List (Symbol "do" :: rest)->
            let evaled = evalAst (List rest) env

            match evaled with 
            | List results -> List.rev results |> List.head
            | _ -> evaled


        | List (Symbol "if" :: condition :: rest) ->
            let evalCond = EVAL condition env
            match evalCond, rest with
            | Nil, [first; last] | Bool false, [first; last] ->
                EVAL last env
            | Nil, [first] | Bool false, [first] -> 
                Nil
            | _, [first] ->
                EVAL first env
            | _, [first; _] ->
                EVAL first env
            | _, _ -> 
                raise (Exception("Invalid if form"))


        | List [Symbol "def!"; Symbol name; form] ->
            let evaled = EVAL form env
            set env name evaled
            evaled

        | List [Symbol "let*"; bindings; calls] ->
            let newChain = makeNewEnv env [] []

            let updateEnv (key, value) =
                match key with
                | Symbol v -> set newChain v (EVAL value newChain)
                | _ -> ()
            
            match bindings with 
            | List vs | Vector vs -> splitListToPairs vs |> List.iter updateEnv 
            | _ -> raise(Exception("Invalid let* form"))

            EVAL calls newChain

        | List [Symbol "fn*"; (List args) | (Vector args); body] ->
            let temp = makeFunction Core.noop body args env

            temp
            
            
        | List (func :: args) as item-> 
            let values = evalAst item env
            match values with 
            | List (func :: rest) ->
                apply func rest
            | _ -> raise(Exception("Invalid form"))

        | _ -> evalAst ast env

    and PRINT exp =
        Printer.PrStr exp

    and REP str =
        str |> READ |> (fun x -> EVAL x initialEnv) |> PRINT true

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
//    [<EntryPoint>]
//    let main argv = 
//        //printfn "%A" argv
//
//        let mutable running = true
//
//
//        while running do
//            Console.Write("user> ")
//            let line = Console.ReadLine()
//
//            if line = null || line = "quit" then
//                running <- false
//            else
//                //Console.WriteLine(REP line)
//                try 
//                    let result = REP line
//                    Console.WriteLine(result)
//                with
//                    | ex -> printfn "%s" ex.Message
//
//        0 // return an integer exit code
