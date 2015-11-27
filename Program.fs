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
        | List [Symbol "def!"; Symbol name; form] ->
            let evaled = EVAL form env
            set env name evaled
            evaled

        | List [Symbol "let*"; List bindings; calls] ->
            let newChain = makeEmptyEnv() :: env

            let updateEnv (key, value) =
                match key with
                | Symbol v -> set newChain v (EVAL value newChain)
                | _ -> ()

            splitListToPairs bindings |> List.iter updateEnv 

            EVAL calls newChain
            
        | List (func :: args) -> 
            let funcEval = evalAst func env
            let rest = evalAst (List args) env

            match rest with
            | List vs -> apply funcEval vs
            | x -> apply funcEval [x]

        | _ -> evalAst ast env

    and PRINT exp =
        Printer.PrStr exp

    and REP str =
        str |> READ |> (fun x -> EVAL x initialEnv) |> PRINT

    [<EntryPoint>]
    let main argv = 
        //printfn "%A" argv

        let mutable running = true

        while running do
            Console.Write("user> ")
            let line = Console.ReadLine()

            if line = null || line = "quit" then
                running <- false
            else
                //Console.WriteLine(REP line)
                try 
                    let result = REP line
                    Console.WriteLine(result)
                with
                    | ex -> printfn "%s" ex.Message

        0 // return an integer exit code
