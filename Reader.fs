namespace MAL

module Reader = 
    open System.Text.RegularExpressions
    open Types

    let PATTERN = """[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)]*)"""
    
    type Reader(tokens : list<string>) =
        let mutable position = 0
        member this.Tokens = tokens

        member this.Peek() = 
            this.Tokens.Item position

        member this.Next() =
            let ret = this.Peek()
            position <- position + 1
            ret

        member this.IsDone() =
            position = this.Tokens.Length - 1
        


    let makeParseError item = 
        "expected '" + item + "', got EOF"

    let rec ReadStr str = 
        //ReadForm (new Reader(Tokenizer str))
        let reader = new Reader(Tokenizer str)

        let (matches, error) = EnsureMatch reader

        if matches then
            ReadForm reader
        else
            raise (ParseError(error))
        

    and Tokenizer str =
        let nrQuotes s = 
            s 
            |> Seq.filter (fun x -> x = '"')
            |> Seq.length

        if (nrQuotes str) % 2 <> 0 then
            raise (ParseError(makeParseError "\""))
        else
            let matches = Regex.Matches(str, PATTERN)

            //NOTE(ville): The item at index 1 in each matched group is the actual match string
            if matches.Count > 0 then
                [for m in matches -> (m.Groups.Item 1).Value.Trim()] |> List.filter (fun x -> x.Length > 0)
            else
                []

    and EnsureMatch (reader : Reader) = 
        let howMany pred = Seq.filter pred >> Seq.length

        let pairMatch (fs, sn) = 
            howMany (fun x -> x = fs) reader.Tokens = howMany (fun x -> x = sn) reader.Tokens


        let getResult (op, cl) =
            let m1 = howMany (fun x -> x = op) reader.Tokens
            let m2 = howMany (fun x -> x = cl) reader.Tokens

            if m1 = m2 then
                (true, "")
            else
                if m1 > m2 then
                    (false, makeParseError cl)
                else
                    (false, makeParseError op)


        let symbolPairs = [
            ("(", ")");
            ("[", "]");
            ("{", "}");
        ]

        let res = 
            symbolPairs 
            |> Seq.map getResult
            |> Seq.filter (fun (a, _ ) -> not a)
            |> Seq.map snd
            |> String.concat "\n"

        if res.Length = 0 then
            (true, "")
        else
            (false, res)


    and ReadForm (reader : Reader) = 
        match reader.Peek() with
        | "("   -> ReadList reader
        | "["   -> ReadVector reader
        | _     -> ReadAtom reader


    and ReadUntil (reader : Reader) endChar lst = 
        match reader.Peek() with
        | x when x.Equals(endChar) -> lst
        | _ -> ReadUntil reader endChar (lst @ [ReadForm reader])

    and ReadList (reader : Reader) = 
        //Skip the initial (
        ignore (reader.Next())

        List (ReadUntil reader ")" [])

//        let mutable ret : list<MalType> = []
//
//        //TODO(ville): Make this into a functional call
//        while not (reader.Peek().Equals(")")) do
//
//            if reader.IsDone() then raise (ParseError("Missing matching parenthesis"))
//            
//            let value = ReadForm reader
//
//            ret <- ret @ [value]
//
//        List ret

    and ReadVector (reader : Reader) = 
        ignore (reader.Next())
        Vector (ReadUntil reader "]" [])

    and ReadAtom (reader : Reader) = 
        let value = reader.Next()

        match value with
        | "nil" -> Nil
        | "true" -> Bool true
        | "false" -> Bool false
        | _ ->
            try
                let number = System.Int32.Parse(value)
                Number number
            with
                | _ -> Atom value
            

//    and ReadNil (reader : Reader) =
//        ignore (reader.Next())
//        Nil
//
//    and ReadBool (reader : Reader) = 
//        match reader.Next() with
//        | "true" | "#t" -> Bool true
//        | "false" | "#f" -> Bool false
//        | _ -> Bool false
//
    