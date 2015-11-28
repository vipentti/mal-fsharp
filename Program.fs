module Main 
    open System
    open Reader
    open Types

    open Env

    let rec evalAst (env : EnvChain) =  function
        | Symbol v -> get env v
        | List (_, vs) -> vs |> List.map (eval env) |> makeList
        | Vector (_, vs) -> vs |> List.map (eval env) |> makeVector
        | HashMap (_, map) ->
            map
            |> Map.map (fun key value -> eval env value)
            |> makeHashMap
        | item -> item

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
            | List (_, vs) | Vector (_, vs) -> splitListToPairs vs |> List.iter updateEnv 
            | _ -> raise(Exception("Invalid let* form"))

            newChain, calls
            
        | _ -> raise(Exception("Invalid form"))


    and isNonEmpty args = 
        match args with 
        | (List (_, vs)) | (Vector (_, vs)) -> vs.Length > 0
        | _ -> false


    //NOTE(ville): Do we actually need this ?
    and convertVectorToList args = 
        match args with
        | Vector (_, vs) -> 
            vs
            |> List.map convertVectorToList
            |> makeList
        | _ -> args

    and quasiquote ast = 
        match (isNonEmpty ast), (convertVectorToList ast) with
        | false, _ -> makeList ((Symbol "quote") :: [ast])
        | _, List (_, [Symbol "unquote"; rest])-> rest

        | true, List (_, List (_, Symbol "splice-unquote" :: spliceArgs) :: restArgs) ->
            let restQuasi = quasiquote (makeList restArgs)
            makeList ([Symbol "concat"] @ spliceArgs @ [restQuasi])

        | _, List (_, xs :: rest) -> 
            makeList ([Symbol "cons"] @ [quasiquote xs] @ [quasiquote (makeList rest)])

        | _, _ -> raise(Exception("Invalis quasiquote form"))

    and macroexpand env ast = 
        let get_macro_call env = function 
            | List (_, (Symbol name :: rest)) ->
                match (find env name) with
                | Some(Macro(_)) as m -> m, rest
                | _ -> None, []
            | _ -> None, []

        match get_macro_call env ast with
        | Some(Macro(_, _, f, _, _, _)), rest -> 
            f rest |> macroexpand env
        | _, _ -> ast 

    and fnStarForm outer ast = 
        
        let func binds body = 
            let f = 
                fun items -> 
                    let inner = makeNewEnv outer binds items
                    eval inner body
            Env.makeFunction f body binds outer

        match ast with
        | [(List (_, args)) | (Vector (_, args)); body] ->
            func args body

        | _ -> raise(Exception("Invalid fn* form"))

    and tryCatchForm env = function
        | [tryBlock; catchBlock] -> 
            match catchBlock with
            | List (_, [Symbol "catch*"; exc; form]) ->
                try 
                    eval env tryBlock
                with
                    | MalException v as ex -> 
                        let exMal =  v
                        let exEnv = makeNewEnv env [exc] [exMal]
                        eval exEnv form
                    | ex -> 
                        let exMal = Types.String ex.Message
                        let exEnv = makeNewEnv env [exc] [exMal]
                        eval exEnv form

            | _ -> raise(Exception("Invalid try*/catch* form"))
        | _ -> Nil

    and eval (env : EnvChain) = function
        | List _ as outerAst -> 
            match macroexpand env outerAst with
            | List (_, (Symbol "do" :: rest)) -> doForm env rest |> eval env
            | List (_, (Symbol "if" :: rest)) -> ifForm env rest |> eval env
            | List (_, [Symbol "def!"; Symbol name; form]) ->
                let evaled = eval env form
                set env name evaled
                evaled
            | List (_, [Symbol "defmacro!"; Symbol name; form]) ->
                let evaled = eval env form
                match evaled with
                | Function(_, _, f, body, binds, outer) ->
                    let macro = Env.makeMacro f body binds outer
                    set env name macro
                    macro
                | _ -> raise(Exception("Invalid macro form"))

            
            | List (_, (Symbol "try*" :: rest)) -> tryCatchForm env rest
            | List (_, (Symbol "let*" :: rest)) ->
                let newChain, calls = letStarForm env rest
                eval newChain calls

            | List (_, (Symbol "fn*" :: rest)) -> fnStarForm env rest
            | List (_, [Symbol "quote"; rest]) -> rest
            | List (_, [Symbol "quasiquote"; rest]) -> quasiquote rest |> eval env 
            | List (_, [Symbol "macroexpand"; args]) -> macroexpand env args
            | List (_, _) as item -> 
                let values = evalAst env item
                match values with 
                | List (_, (func :: args)) ->
                    match func with
                    | PrimitiveFunction(_, _, f) -> f args
                    | Function(_, _, _, body, binds, outer) ->
                        let newEnv = makeNewEnv outer binds args
                        body |> eval newEnv
                    | _ -> raise(Exception("Invalid function"))
                | _ -> raise(Exception("Invalid form"))
            | item -> item
        | item -> evalAst env item

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

        let env = makeRootEnv Core.coreFunctions

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
            |> makeList

        set env "*ARGV*" malArgs
        set env "*host-language*" (String("F-Sharp"))


        REP env "(def! not (fn* (a) (if a false true)))" |> ignore
        REP env "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))" |> ignore
        REP env "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))" |> ignore
        REP env "(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))" |> ignore

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