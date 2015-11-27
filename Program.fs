namespace MAL

module Main = 
    open System
    open Reader
    open Printer
    open Types


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

    let replEnv = 
        [
            "+", singleMathOp (+);
            "-", singleMathOp (-);
            "*", singleMathOp (*);
            "/", singleMathOp (fun x y -> int(x/y));
        ] 
        |> List.map (fun (x, y) -> x, PrimitiveFunction (x,y))
        |> Map.ofList

    let rec READ str =
        Reader.ReadStr str


    and apply fn args = 
        match fn with
        | PrimitiveFunction(_, f) -> f args
        | _ -> raise(Exception("Invalid function"))

    and evalAst ast (env : Map<string, MalType>) = 
        match ast with
        | Symbol v -> env.[v]
        | List vs -> vs |> List.map (fun x -> EVAL x env) |> List
        | _ -> ast

    and EVAL ast (env : Map<string, MalType>) =
        match ast with
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
        str |> READ |> (fun x -> EVAL x replEnv) |> PRINT

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
