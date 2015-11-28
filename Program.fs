﻿module Main 
    open System
    open Reader
    open Types

    open Env

    let rec evalAst (env : EnvChain) ast = 
        match ast with
        | Symbol v -> get env v
        | List vs -> vs |> List.map (eval env) |> List
        | Vector vs -> vs |> List.map (eval env) |> Vector
        | HashMap map ->
            map
            |> Map.map (fun key value -> eval env value)
            |> HashMap
        | _ -> ast

    and ifForm env args = 

        let innerIf condition trueForm falseForm = 
            match eval env condition with
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
            eval env a |> ignore
            doForm env rest
        | _ -> raise(Exception("Something"))

    
    and letStarForm env args = 
        match args with
        | [bindings; calls] ->
            let newChain = makeNewEnv env [] []

            let updateEnv (key, value) =
                match key with
                | Symbol v -> set newChain v (eval newChain value)
                | _ -> ()
            
            match bindings with 
            | List vs | Vector vs -> splitListToPairs vs |> List.iter updateEnv 
            | _ -> raise(Exception("Invalid let* form"))

            newChain, calls
            
        | _ -> raise(Exception("Invalid form"))


    and isNonEmpty args = 
        match args with 
        | (List vs) | (Vector vs) -> vs.Length > 0
        | _ -> false


    //NOTE(ville): Do we actually need this ?
    and convertVectorToList args = 
        match args with
        | Vector vs -> 
            vs
            |> List.map convertVectorToList
            |> Types.List
        | _ -> args

    and quasiquote ast = 
        match (isNonEmpty ast), (convertVectorToList ast) with
        | false, _ -> List ((Symbol "quote") :: [ast])
        | _, List [Symbol "unquote"; rest] -> rest

        | true, List (List (Symbol "splice-unquote" :: spliceArgs) :: restArgs) ->
            let restQuasi = quasiquote (List restArgs)
            List ([Symbol "concat"] @ spliceArgs @ [restQuasi])

        | _, List (xs :: rest) -> 
            List ([Symbol "cons"] @ [quasiquote xs] @ [quasiquote (List rest)])

        | _, _ -> raise(Exception("Invalis quasiquote form"))

    and macroexpand env ast = 
        let is_macro_call env ast = 
            match ast with
            | List (Symbol name :: _) ->
                match (find env name) with
                | Some(Macro(_)) -> true
                | _ -> false
            | _ -> false

//        let get_macro env ast =
//            match ast with
//            | List (Symbol name :: _) ->
//                match (get env name) with
//                | Macro(_) as v -> v
//                | _ -> raise(Exception("Not a macro"))
//            | _ -> raise(Exception("Not a macro"))
//            
        let rec loop env ast = 
            match is_macro_call env ast, ast with 
            | true, (List (Symbol name :: args) as item) ->
                let macro = get env name 
                match macro with 
                | Macro(_, _, content, _, _) ->
                    let nextAst = eval env content
                    loop env nextAst
                | _ -> raise(Exception("Not a macro."))
//                let macro = get_macro env item
//
//                match macro with
//                | Macro(_, _, body, _, _) ->
//                | _ -> ast

            | _ -> 
                ast

        //loop env ast
        ast

    and fnStarForm outer ast = 
        
        let func binds body = 
            let f = 
                fun items -> 
                    let inner = makeNewEnv outer binds items
                    eval inner body
            Env.makeFunction f body binds outer

        match ast with
        | [(List args) | (Vector args); body] ->
            func args body

        | _ -> raise(Exception("Invalid fn* form"))

    and eval (env : EnvChain) oAst =

        match oAst with
        | List _ -> 
            let expandedAst = macroexpand env oAst

            match expandedAst with
            | List _ ->
                let ast = expandedAst
                match ast with

                | List (Symbol "do" :: rest) -> doForm env rest |> eval env

                | List (Symbol "if" :: rest) -> ifForm env rest |> eval env

                | List [Symbol "def!"; Symbol name; form] ->
                    let evaled = eval env form
                    set env name evaled
                    evaled

                | List [Symbol "defmacro!"; Symbol name; form] ->
                    let evaled = eval env form
                    let macro = Env.makeMacro Core.noop evaled [] env
                    set env name macro
                    macro

                | List (Symbol "let*" :: rest) ->
                    let newChain, calls = letStarForm env rest
                    eval newChain calls

                | List (Symbol "fn*" :: rest) -> fnStarForm env rest

                | List [Symbol "quote"; rest] -> rest

                | List [Symbol "quasiquote"; rest] -> quasiquote rest |> eval env 

                | List [Symbol "macroexpand"; args] -> macroexpand env args
                    
                | List (func :: args) as item-> 
                    let values = evalAst env item
                    match values with 
                    | List (func :: args) ->
                        match func with
                        | PrimitiveFunction(_, f) -> f args
                        | Function(_, f, body, binds, outer) ->
                            f args

    //                        let newEnv = makeNewEnv outer binds args
    //                        body |> eval newEnv

                        | Macro(_, _, body, binds, outer) ->
                            let newEnv = makeNewEnv outer binds args
                            body |> eval newEnv

                        | _ -> raise(Exception("Invalid function"))
                    | _ -> raise(Exception("Invalid form"))

                | _ -> evalAst env ast

            | _ -> expandedAst
        | _ -> evalAst env oAst

    and READ str =
        Reader.ReadStr str

    and PRINT exp =
        Printer.PrStr exp

    and EVAL (env : EnvChain) ast =
        eval env ast

    and REP env str =
        str 
        |> READ 
        |> (EVAL env) 
        |> PRINT true

    let read (prompt :string) = 
        Console.Write(prompt)
        Console.Out.Flush()
        Console.ReadLine()


    let evalFunction env args =
        match args with
        | [item] -> eval env item
        | _ -> raise(Exception("Invalid eval form"))
    
    [<EntryPoint>]
    let main argv = 

        let env = makeRootEnv ()

        set env "eval" (makePrimitiveFunction(evalFunction env))

        let malArgs =
            let tempArgs = 
                if argv.Length = 0 then
                    [||]
                else
                    argv.[1..]
            tempArgs
            |> Array.map Types.String
            |> Array.toList
            |> Types.List

        set env "*ARGV*" malArgs


        REP env "(def! not (fn* (a) (if a false true)))" |> ignore
        REP env "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))" |> ignore

        let rec loop () =
            match read "user> " with
            | null -> 0
            | input -> 
                //printfn "%s" (REP input)
                try 
                    printfn "%s" (REP env input)
                with
                    | ex -> 
                        if ex.Message.Length > 0 then
                            printfn "%s" ex.Message
                        else
                            ()
                loop()
        loop()