module Env
    
    open Types
    open System
    open Core

    let makeEmptyEnv () = Env ()

    let set (env : EnvChain) key value = 
        match env with
        | head::_ -> head.[key] <- value
        | _ -> raise(Exception("Empty environment"))


    let rec find (env : EnvChain) key =
        match env with
        | [] -> None
        | env::rest -> 
            match env.TryGetValue(key) with
            | true, v -> Some(v)
            | false, _ -> find rest key

    let get (env : EnvChain) key = 
        match find env key with
        | Some v -> v
        | _ -> raise(Exception("No such value found"))

    let makeNewEnv outer binds exprs = 
        let newChain = (makeEmptyEnv ()) :: outer

        let rec loop binds exprs = 
            match binds, exprs with
            | Symbol s :: symbs, n :: ns ->
                set newChain s n
                loop symbs ns
            | [], [] -> newChain
            | _ -> raise(Exception("Something went wrong"))

        loop binds exprs


    let makeRootEnv () = 
        let env = makeEmptyEnv()
        let result = [env]
        let replEnv = 
            [
                "+", singleMathOp (+);
                "-", singleMathOp (-);
                "*", singleMathOp (*);
                "/", singleMathOp (fun x y -> int(x/y));

                "list", List;
                "list?", isList;
                "empty?", isEmpty;
                "count", count;
                "=", boolBinop (=);
                ">", boolBinop (>);
                ">=", boolBinop (>=);
                "<", boolBinop (<);
                "<=", boolBinop (<=);

                "pr-str", prstr;
                "str", str;
                "prn", prn;
                "println", println;
            ] 
            |> List.iter (fun (x, y) -> set result x (PrimitiveFunction (x, y)))

        result
//            |> List.map (fun (x, y) -> x, PrimitiveFunction (x,y))
//            |> 
        
